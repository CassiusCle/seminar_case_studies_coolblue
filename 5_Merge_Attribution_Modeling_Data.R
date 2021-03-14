library(readr)
library(dplyr)
library(ggplot2)
library(Metrics)
library(caret)

# Script that combines the transformed broadcast data set with our attribution results to prepare for modeling the estimated effectiveness of TV commercials
# NB:   - This script requires the workspace resulting from running '3_Attribution_Estimation.R' and '4_Modeling_Data.R' to run properly

# Merge Attribution results with broadcast data
df_T <- merge(dt, cdata_effects, by = c("date_time", "channel", "gross_rating_point"))

columns_to_remove <- c("gross_rating_point","date"               ,"time"           ,"position_in_break.x",
                       "program_before.x"  ,"program_after.x"    ,"country.x"      ,"operator.y"               ,"position_in_break.y",
                       "length_of_spot.y"  ,"program_before.y"   ,"program_after.y","program_category_before.y","program_category_after.y",
                       "product_category.y","country.y"          ,"clash_nl"       ,"clash_be"                 ,"clash_tot",
                       "stsm_pval")
lr_df  <- df_T
lr_df [,columns_to_remove] <- NULL
remove(columns_to_remove)

lr_df %>% colnames
colnames(lr_df) <- c('date_time',"channel", "operator", "spotlength", "program_category_before","program_category_after", 
                     "product", "time_interval", "position_in_break", "absolute_effect", "relative_effect")


# Function that creates a data frame according to specified set of broadcast dimensions
create_subset <- function(data, dimensions, absolute = TRUE, relative = TRUE){
  selection <- dimensions
  if (absolute){
    selection <- c(selection, 'absolute_effect')
  }
  if (relative){
    selection <- c(selection, 'relative_effect')
  }
  data <- data[,selection]
  data[dimensions] <- lapply(data[dimensions], factor)
  data
}

# Specify the dimensions included in the broadcast dimension sets
dimensions_base <- c("operator", "spotlength", "product")
dimensions_full <- c("channel", "operator", "spotlength", "program_category_before","program_category_after", 
                     "product", "time_interval", "position_in_break")

# Create the dataframe for Baseline set
df_base <- create_subset(lr_df, dimensions_base)
str(df_base)

# Create the dataframe for the Extended set
df_full <- create_subset(lr_df, dimensions_full)
str(df_full)

df <- lr_df
remove(df_T, lr_df, replace_with, dt, create_subset, dimensions_base, dimensions_full)


