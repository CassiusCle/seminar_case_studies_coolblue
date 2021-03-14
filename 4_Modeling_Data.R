library(dplyr)
library(lubridate)
library(fastDummies)

# Script that prepares the broadcast data set for modeling the estimated effectiveness of TV commercials

# Import data set
path <- "/Users/cpdh/Documents/Studie/MSc QM & BA/Case Studies Seminar CoolBlue/Wetransfer Data/broadcasting_data.csv" 
broadcasting_data <- read.csv(path)
dt <- broadcasting_data

# Select only broadcasting data from the Netherlands
dt <- dt[which(dt$country == "Netherlands"),]

dt$date_time <- as_datetime(ymd(dt$date) + hms(dt$time))

myIntervals <- c("0 AM - 6 AM", "6 AM - 11 AM", "11 AM - 4 PM", "4 PM - 6 PM", "6 PM - 8 PM", "8 PM - 10 PM", "10 PM - 0 AM")
dt$time_interval <- myIntervals[findInterval(period_to_seconds(hms(dt$time)), c(0, 6, 11, 16, 18, 20, 22, 24) * 3600)]


# Create position in break variable
dt <- dt[which(dt$position_in_break != 0),] #remove observations with '0' values. 
dt$position_in_break <- as.vector(dt$position_in_break) 
dt$position_in_break <- strtoi(dt$position_in_break)

channels <- unique(dt$channel)
dt$position_in_break_new <- 0

dt <- tibble::rownames_to_column(dt, "index") #to be removed later
for (i in 1:length(channels))
{
  dt_sub <- dt[which(dt$channel == channels[i]),]
  unique_positions <- unique(dt_sub$position_in_break)
  dt_sub$position_in_break_new[which(dt_sub$position_in_break == min(unique_positions))] <- "first position"
  dt_sub$position_in_break_new[which(dt_sub$position_in_break == sort(unique_positions, decreasing = FALSE)[2])] <- "second position"
  dt_sub$position_in_break_new[which(dt_sub$position_in_break < sort(unique_positions, decreasing = TRUE)[2] & dt_sub$position_in_break > sort(unique_positions, decreasing = FALSE)[2])] <- "any other position"
  dt_sub$position_in_break_new[which(dt_sub$position_in_break == sort(unique_positions, decreasing = TRUE)[2])] <- "second last position"
  dt_sub$position_in_break_new[which(dt_sub$position_in_break == max(unique_positions))] <- "last position"
  for (k in 1:length(dt_sub$index))
  {
    ind <- which(dt$index == strtoi(dt_sub$index[k]))
    dt$position_in_break_new[ind] <- dt_sub$position_in_break_new[k]
  }
}

dt <- dt[,-c(1)]
remove(dt_sub, unique_positions, myIntervals, i, k, channels, ind)

dt <-dt[dt$program_category_after != "storing", ] 

# Auxiliary function for merging program categories
replace_with <- function(data, replacement, targets){
  for(t in targets){
    data$program_category_after <- gsub(t,replacement,data$program_category_after)
    data$program_category_before <- gsub(t,replacement,data$program_category_before)
  }
  return(data)
}

# New category: News
dt <- replace_with(dt, 'News', c('actualiteiten', 'nieuws', 'weerbericht', 'news/flash', 'tekstuele informatie'))

# New category: Game
dt <- replace_with(dt, 'Game', c('spel & quiz', 'game/quiz'))

# New category: Sport
dt <- replace_with(dt, 'Sport', c('actuele sportinformatie', 'overige sportinformatie', 'overige sportreportage', 'voetbalreportage'))

# New category: Unknown
dt <- replace_with(dt, 'Unknown', c('niet opgegeven', 'other varied'))

# New category: Show
dt <- replace_with(dt, 'Show', c('debate/talk show', 'on stage', 'other studio/structured/show', 'show', 'talentenjacht of auditieprogramma'))

# New category: Reality TV
dt <- replace_with(dt, 'Reality TV', c('docusoap/reality serie', 'reality structured', 'reality Show'))

# New category: Other Amusement
dt <- replace_with(dt, 'Other Amusement', c("cabaret/kleinkunst", "magazine", "overig amusement", "programme trailer", 
                                            "satirisch programma", "kinderen: amusement", "kinderfilms: tekenfilm/animatie/poppen"))
# New category: Film
dt <- replace_with(dt, 'Film', c('animation film', 'btl films: comedy', 'btl films: drama', 'btl films: overig', 'btl films: spanning', 
                                 'documentary film', 'nld films: comedy', 'nld films: drama', 'nld films: spanning', 'film'))

# New category: TV Series
dt <- replace_with(dt, 'TV Series', c("animation serie/cartoon", 'btl series: (sit)comedy', 'btl series: drama', 'btl series: overig',   
                                   'btl series: soap', 'btl series: spanning', "documentary serie", 'nld series: (sit)comedy', 
                                   'nld series: drama', 'nld series: overig', 'nld series: soap', 'nld series: spanning'))

dt$program_category_before[dt$program_category_before == "btl series: (sit)comedy"] <- "TV Series"
dt$program_category_after[dt$program_category_after == "btl series: (sit)comedy"] <- "TV Series"

dt$program_category_before[dt$program_category_before == "nld series: (sit)comedy"] <- "TV Series"
dt$program_category_after[dt$program_category_after == "nld series: (sit)comedy"] <- "TV Series"

# New category: Music
dt <- replace_with(dt, 'Music', c("clip[[:punct:]]s[[:punct:]]", "overige muziek[[:punct:]] overig", 
                                  "populaire muziek[[:punct:]] programma", "populaire muziek[[:punct:]] videoclips"))

# New category: Health and Lifestlye
dt <- replace_with(dt, 'Health / Lifestyle', c("gezondheid/lifestyle/opvoeding/coaching"))

# New category: Living
dt <- replace_with(dt, 'Living / Home Decorating', c("wonen/interieurs/ tuin/doe het zelf"))

# New category: Vehicles
dt <- replace_with(dt, 'Vehicles', c("auto/motor/boot/fiets/verkeer"))

# New category: Food
dt <- replace_with(dt, 'Food', c("eten/drinken/koken"))

# New category: Other non-Fiction  
dt <- replace_with(dt, 'Other non-Fiction', c("reizen/vakantie/toerisme", "kinderen: non fictie", 'algemene consumenten informatie',
                                              "overige non fictie", "comment of event","godsdienst/verkondiging"))

# New category: Intellectual
dt <- replace_with(dt, 'Intellectual', c("[[:punct:]]populaire[[:punct:]] wetenschap", "natuur & milieu", "justitie/recht", "kunst"))

dt <- dt[order(dt$date_time),]

