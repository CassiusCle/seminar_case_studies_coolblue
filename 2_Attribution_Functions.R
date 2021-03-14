library(CausalImpact)
library(foreach)
library(doParallel)
library(lubridate)
library(dplyr)

# Script that defines the functions needed for the Causal impact estimation of the commercials
# It defines the following two functions:
#     - commercial_impact
#     - impact_estimation_parallel

# Function that estimates the causal effect of a particular commercial on the response variable
commercial_impact <- function(i, data, commercials, pre, post, control, verbose = FALSE){
  this_date <- commercials$date_time[i]
  this_index <- rownames(commercials)[i]
  current_window <- subset(data_for_impact, between(date_time, this_date-pre*60, this_date+post*60))
  current_window$cum_commercials[-(1:pre)] <- current_window$cum_commercials[-(1:pre)] - control[1:(1+post)]
  current_window$date_time <- NULL
  
  # Use CausalImpact function to estimate the impact
  impact <- CausalImpact(zoo(current_window), 
                         c(1,pre),           # pre.period
                         c(pre+1,pre+1+post) # post.period
                         )
  
  # Print progress over iterations
  if (i %% 250 == 0) print(paste0( round(i/nrow(commercials)*100,1), '% complete'))
  
  return(c(as.numeric(this_index), # Commercial index number
           as.numeric(this_date), # Date_time of commercial
           colSums(impact[["model"]][["bsts.model"]][["coefficients"]] != 0)/impact[["model"]][["bsts.model"]][["niter"]], # Inclusion probability
           impact[["summary"]][["p"]][2], # P-value of measured effect being due to chance
           impact[["summary"]][["AbsEffect"]][2], # Absolute effect (s.d.) Cumulative:
           impact[["summary"]][["RelEffect"]][2]  # Relative effect (s.d.) Cumulative:
  ))
}

# Function that estimates the causal impact of all commercials in the data and outputs results as a DataFrame
impact_estimation_parallel <- function (pre = 100, post = 3, vars, vdata, commercials, control){
  data_for_impact <- vdata[,c("date_time",vars)]
  
  # Select correct commercials (no clashes with Belgian commercials and not at the border)
  commercials <- filter(commercials, clash_be == 0 &
                          date_time > (as_datetime("2019-01-01 UTC")          + pre*60) &
                          date_time < (as_datetime("2019-06-30 23:59:00 UTC") - post*60)
  )
  
  # Setup Parallel Backend to use many processors
  cores=detectCores()
  cl <- makeCluster(cores[1]-1, outfile = "")
  registerDoParallel(cl)
  
  # Calculate impact for all selected commercials in parallel
  impact_matrix_par <- foreach(i = 1:nrow(commercials), 
                               .combine = rbind, 
                               .packages = "CausalImpact", 
                               .export= c('commercial_impact', 'between')) %dopar% {
    commercial_impact(i, data_for_impact, commercials, pre, post, control, verbose = TRUE)
                               }
  
  impacts <- data.frame(impact_matrix_par)
  colnames(impacts) <- c('Index','date_time','(Intercept)', colnames(data_for_impact)[3:ncol(data_for_impact)], 
                         'stsm_pval', 'abs_cum_effect', 'rel_cum_effect')
  impacts$date_time <- as_datetime(impacts$date_time)
  
  stopCluster(cl)
  return(impacts)
}



