library(CausalImpact)
library(foreach)
library(doParallel)
library(lubridate)
library(dplyr)

# Script that runs the estimation procedure for the Causal Impact of individual commercials on our response variable
# NB:   - This script might take a while to run
#       - This script requires the workspace resulting from running '1_Attribution_Data.R' and '2_Attribution_Functions.R' to run properly

pre <- 100
post <- 3
vars <- c("dsp_nl_0B","other_nl_0B","dsp_be_0B","other_be_0B","other_nl_1B","dsp_be_1B","other_be_1B","cum_commercials")
alpha <- 0.1


# Run Causal Impact estimation (This takes a while)
impacts_base <- impact_estimation_parallel(pre, post, 
                                 vars = vars, vdata = visit_data,
                                 commercials = commercials_nl,
                                 control = control_var)

impacts <- impacts_base
impacts[which(impacts$stsm_pval > alpha),c('abs_cum_effect','rel_cum_effect')] <- 0

# Merge with commercial data.
cdata_effects <- filter(commercials_nl, clash_be == 0 &
                        date_time > (as_datetime("2019-01-01 UTC")          + pre*60) &
                        date_time < (as_datetime("2019-06-30 23:59:00 UTC") - post*60)
)
impacts <- impacts[c('stsm_pval','abs_cum_effect','rel_cum_effect')]
cdata_effects <- cbind(cdata_effects, impacts)

# Estimate impact for the commercial at 2019-03-17 19:51:00 UTC as example
this_date <- as_datetime("2019-03-17 19:51:00 UTC")
current_window <- subset(visit_data, between(date_time, this_date-pre*60, this_date+post*60))
current_window$cum_commercials[-(1:pre)] <- current_window$cum_commercials[-(1:pre)] - control_var[1:(1+post)]
current_window$date_time <- NULL
impact <- CausalImpact(zoo(current_window), 
                       c(1,pre),            # pre.period
                       c(pre+1,pre+1+post), # post.period
                       alpha = 0.1
)
example.plot <- plot(impact, c("original", "cumulative"))

# Print and plot output
summary(impact)
plot(example.plot)

rm(list=setdiff(ls(), c('cdata_effects', 'example.plot')))





