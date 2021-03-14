library(dplyr)
library(readr)
library(lubridate)

# Script for pre-processing the raw traffic_data and broadcast_data data sets
# After running this script, your workspace consists of the following:
#   Data
#     - broadcasting_data         :   Raw dataframe of input data.
#     - commercials_nl            :   Dataframe with all information on commercials in the Netherlands.
#                                       -Has POSIXct-type date_time variable instead of 'date' and 'time'
#                                       - Has variables that count the number of commercials that air within a specific window of each commercial
#     - commercials_be            :   Same as above for Belgium.
#     - tdata_wide                :   Dataframe on visit numbers per minute per country split over sources for bounces and non-bounces
#     - visit_data                :   Dataframe on total relevant visit numbers per minute per country.
#                                       - Contains control variable for current cumulative commercials.
#                                       - NB: This is the dataframe that you should use for analysis of the visits.
#
#   Values
#     - control_var               :   Vector of ones used to construct the variable that is used to control for simultaneous commercials
#     - post_min, pre_min         :   Number of minutes after (before) commercial is aired that are included in the time-window.
#     - post_window, post_window  :   Same as above, but for seconds.

rm(list=ls()) # Discard old data


##################### Traffic Data #################################################################

# Read Traffic data
traffic_data <- read_csv("traffic_data.csv", col_types = cols(avg_session_quality = col_double()))

# NA in bounces means that there were no bounces among visitors
traffic_data[is.na(traffic_data$bounces),'bounces'] <- 0

# Convert visits_index to real number of visits
tdata <- data.frame()
for (cntry in c('Netherlands', 'Belgium')){
  for ( mdm in c('website', 'app')){
    df <- filter(traffic_data, country == cntry & medium == mdm)
    df$visits <- df$visits_index/min(df$visits_index)
    tdata <- rbind(tdata, df)
  }
}
remove(df, cntry, mdm)

# Split visits by source: "other","direct","search","paid search","push notification"
tdata$visits_other <- tdata$visits * as.numeric(tdata$visit_source == 'other')
tdata$visits_direct <- tdata$visits * as.numeric(tdata$visit_source == 'direct')
tdata$visits_search <- tdata$visits * as.numeric(tdata$visit_source == 'search')
tdata$visits_paid <- tdata$visits * as.numeric(tdata$visit_source == 'paid search')
tdata$visits_push <- tdata$visits * as.numeric(tdata$visit_source == 'push notification')
tdata <- rename(tdata, visits_total = visits)

tdata_agg <- aggregate(x = tdata[c("visits_total", "visits_other", "visits_direct", "visits_search", "visits_paid", "visits_push")],
                       FUN = sum,
                       by = list(date_time = tdata$date_time, bounces = tdata$bounces, country = tdata$country))

tdata_long <- tdata_agg
tdata_long$visits_push <- NULL # Remove push, only represents 0.082% of visits (0.0008194797)
tdata_long$visits_dsp <- tdata_long$visits_direct + tdata_long$visits_search + tdata_long$visits_paid
tdata_long$visits_direct <- NULL
tdata_long$visits_search <- NULL
tdata_long$visits_paid <- NULL 
tdata_long$visits_total <- tdata_long$visits_other + tdata_long$visits_dsp

# Get all minutes in out total timeframe
date_times_full <- as.data.frame(list(date_time = seq(as_datetime('2019-01-01 00:00:00'), as_datetime('2019-06-30 23:59:00'), by = 'min')))

a <- filter(tdata_long, bounces == 0, country == 'Netherlands')[c('date_time','visits_total','visits_other','visits_dsp')]
colnames(a) <-  c('date_time',paste0(c("total", "other", "dsp"), '_nl_0B'))

b <- filter(tdata_long, bounces == 0, country == 'Belgium')[c('date_time','visits_total','visits_other','visits_dsp')]
colnames(b) <-  c('date_time',paste0(c("total", "other", "dsp"), '_be_0B'))

c <- filter(tdata_long, bounces == 1, country == 'Netherlands')[c('date_time','visits_total','visits_other','visits_dsp')]
colnames(c) <-  c('date_time',paste0(c("total", "other", "dsp"), '_nl_1B'))

d <- filter(tdata_long, bounces == 1, country == 'Belgium')[c('date_time','visits_total','visits_other','visits_dsp')]
colnames(d) <-  c('date_time',paste0(c("total", "other", "dsp"), '_be_1B'))

# Get full time series of traffic data with missing minutes, by country
tdata_wide <- date_times_full %>% left_join(x = . , y = a) %>% left_join(x = . , y = b) %>% left_join(x = . , y = c) %>% left_join(x = . , y = d)
tdata_wide[is.na(tdata_wide)] <- 0 # Replace NA with zeros

remove(a,b,c,d,tdata,tdata_agg,tdata_long,traffic_data)

# Create dataframe with only relevant visits for Netherlands and Belgium
visit_data <- tdata_wide[,c('date_time','dsp_nl_0B','other_nl_0B','dsp_be_0B','other_be_0B','other_nl_1B','dsp_be_1B','other_be_1B')]

##################### Broadcast Data #################################################################

# Read Broadcast data
broadcasting_data <- read_csv("broadcasting_data.csv", col_types = cols(time = col_time(format = "%H:%M:%S")))

# Commercials by country
commercials_nl <- broadcasting_data[broadcasting_data$country == "Netherlands",]
commercials_be <- broadcasting_data[broadcasting_data$country == "Belgium",]

# Create date_time variable
commercials_nl <- as.data.frame(list(date_time = as_datetime(paste(commercials_nl$date,commercials_nl$time)))) %>% cbind(., commercials_nl)
commercials_nl$date <- NULL
commercials_nl$time <- NULL

commercials_be <- as.data.frame(list(date_time = as_datetime(paste(commercials_be$date,commercials_be$time)))) %>% cbind(., commercials_be)
commercials_be$date <- NULL
commercials_be$time <- NULL

# Window Setting for attribution
pre_min <- 5
pre_window <- pre_min*60
post_min <- 5
post_window <- post_min*60

# Find isolated Dutch commercials with respect to other Dutch commercials for given window
within_window_commercials_nl <- sapply(commercials_nl$date_time, function (x){
  length(subset(
    commercials_nl$date_time, 
    commercials_nl$date_time >= (x-pre_window) & commercials_nl$date_time<= (x+post_window)))-1
})
print(sum(within_window_commercials_nl == 0)) # For 5-5 window: should equal 2634

# Find isolated Dutch commercials with respect to Belgian commercials for given window
within_window_commercials_nl_vs_be <- sapply(commercials_nl$date_time, function (x){
  length(subset(commercials_be$date_time, 
                commercials_be$date_time >= (x-pre_window) & commercials_be$date_time <= (x+post_window)))
})
print(sum(within_window_commercials_nl_vs_be == 0)) # For 5-5 window: should equal 4446

# Create vector of clashes with other commercials per Dutch commercial
commercial_clashes <- within_window_commercials_nl + within_window_commercials_nl_vs_be
print(sum(commercial_clashes == 0)) # For 5-5 window: should equal 2496

###################### Create control variable for current commercials ##################
visit_data$cum_commercials <- 0
remove(date_times_full)

first_time <- min(visit_data$date_time)
last_time <- max(visit_data$date_time)

control_var <- rep(1,post_min+1)

for (x in commercials_nl$date_time){
  date_seq <- seq(from = x, to = (x+post_window), by = 60)
  in_range <- between(date_seq, first_time, last_time)
  date_seq <- date_seq[in_range]
  this_control <- control_var[in_range]
  
  matches <- match(as_datetime(date_seq),
                   visit_data$date_time)
  visit_data$cum_commercials[matches] <- visit_data$cum_commercials[matches] + this_control
}
remove(date_seq, in_range,this_control, matches, x, first_time, last_time)

commercials_nl$clash_nl <- within_window_commercials_nl
commercials_nl$clash_be <- within_window_commercials_nl_vs_be
commercials_nl$clash_tot <- commercial_clashes

remove(commercial_clashes, within_window_commercials_nl, within_window_commercials_nl_vs_be)

