library(dplyr)
library(ggplot2)
library(ggthemes)
library(ggpubr)
library(hash)
library(cowplot)
library(lubridate)
library(ggsci)

# Script that produces the plot used in the data section of the paper
# NB:   - This script requires the workspace resulting from running '5_Merge_Attribution_Modeling_Data.R' to run properly

# Hash setup for channel/operator pairs
pairs <- unique(df[c('channel','operator')])
op_hash <- hash() 
op_hash[pairs$channel] <- pairs$operator
remove(pairs)

# Formatting parameters
text_angle = 22
barcolour1 = 'steelblue'
font_size = 10


# Plot: TV Commercials per day per product
data <- df
data$date <- date(data$date_time)
data <- data %>% group_by(date, product) %>% summarise(no_rows = length(date)) 

data <- left_join(x = as.data.frame(list(date = seq(as_datetime('2019-01-01 00:00:00'), as_datetime('2019-06-30 23:59:00'), by = 'day'))), y = data)
data$no_rows[is.na(data$no_rows)] <- 0
data$product <- factor(data$product)
levels(data$product) <- c('Laptops', 'Televisions', 'Washing Machines')
data$date <- as_date(data$date)

data$`Washing Machines` <- (data$product == 'Washing Machines')*data$no_rows
data$`Washing Machines`[is.na(data$`Washing Machines`)] <- 0

data$`Televisions` <- (data$product == 'Televisions')*data$no_rows
data$`Televisions`[is.na(data$`Televisions`)] <- 0

data$`Laptops` <- (data$product == 'Laptops')*data$no_rows
data$`Laptops`[is.na(data$`Laptops`)] <- 0

brks <- data$date[which(day(data$date) == 1)]
lbls <- months(brks)

ggplot(data, aes(x=date)) + 
  geom_area(aes(y= Laptops, fill = 'Laptops')) +
  geom_area(aes(y= `Washing Machines`, fill = 'Washing Machines')) +
  geom_area(aes(y= Televisions, fill = 'Televisions')) +
  scale_x_date(labels = lbls, breaks = brks) +
  scale_fill_jco(name = "Product:") + 
  labs(y="Number of TV Commercials",
       x = 'Date') + theme_hc() # 11 x 4 inch


########## Horizontally oriented Plots #################

# Plot: TV Commercials per channel and operator
data <- df %>% group_by(channel) %>% 
  summarise(no_rows = length(channel)) 
print(data)
data$Operator <- values(op_hash, keys=data$channel)

plot_channel <- ggbarplot(data, x = "channel", y = "no_rows",
                fill = 'Operator',        
                color = "white",          
                palette = "jco",          
                sort.val = "asc",         
                sort.by.groups = TRUE,    
                x.text.angle = 0  
) + ylab('Number of TV commercials') + xlab('Channel') + coord_flip() + 
  theme_hc() + font("x.text", size = font_size)
plot_channel


# Plot: TV Commercials per programme category
data_pre <- df %>% group_by(program_category_before) %>% 
  summarise(no_rows = length(program_category_before))
data_post <- df %>% group_by(program_category_after) %>% 
  summarise(no_rows = length(program_category_after))

data <- data_pre
data$no_rows <- ((data_pre$no_rows + data_post$no_rows)/2) %>% round
print(data)

plot_programme_cat <- ggbarplot(data, x = "program_category_before", y = "no_rows",
                fill = 'steelblue',   
                color = "steelblue",  
                palette = "jco",      
                sort.val = "asc",     
                sort.by.groups = TRUE,
                x.text.angle = 0      
) + ylab('Number of TV commercials') + xlab('Program Category') + coord_flip() + 
  theme_hc()  + font("x.text", size = font_size)
plot_programme_cat

# Combine horizontal plots in grid
plot_grid(plot_channel, 
          plot_programme_cat,
          labels = c("(a)", "(b)"),
          align = "h",
          axis = 'b',
          rel_widths = c(1, 0.8),
          ncol = 2, nrow = 1
) # 11 x 6 inch

########## Vertically oriented Plots #################

# Plot: TV Commercials per product category
data <- df %>% group_by(product) %>% 
  summarise(no_rows = length(product)) 
data$product <- factor(data$product)
print(data)

levels(data$product) <- c('Laptops', 'Televisions', 'Washing Machines')
print(data)

plot_prod_category <- ggbarplot(data, x = "product", y = "no_rows",
                fill = barcolour1,    
                color = barcolour1,   
                palette = "jco",      
                sort.val = "asc",     
                sort.by.groups = TRUE,
                x.text.angle = 0      
) + ylab('Number of TV commercials') + ggtitle('Product Category') + xlab(NULL) + 
  theme_hc() + font("x.text", size = font_size)
plot_prod_category

# Plot: TV Commercials by commercial length
data <- df %>% group_by(spotlength) %>% 
  summarise(no_rows = length(spotlength)) 
print(data)

plot_length <- ggbarplot(data, x = "spotlength", y = "no_rows",
                fill = barcolour1,      
                color = barcolour1,     
                palette = "jco",        
                sort.val = "none",      
                sort.by.groups = TRUE,  
                x.text.angle = 0        
) + ylab('Number of TV commercials') + ggtitle('Length of Spot') + xlab(NULL) + 
  theme_hc() + font("x.text", size = font_size)
plot_length

# Plot: TV Commercials by time slot
data <- df %>% group_by(time_interval) %>% 
  summarise(no_rows = length(time_interval)) 
print(data)

target <- c('0 AM - 6 AM' , '6 AM - 11 AM', '11 AM - 4 PM', '4 PM - 6 PM' , 
            '6 PM - 8 PM' , '8 PM - 10 PM', '10 PM - 0 AM')
data$time_interval <- factor(data$time_interval, levels = target)
plot_timeslot <- ggbarplot(data, x = "time_interval", y = "no_rows",
                fill = barcolour1,      
                color = barcolour1,     
                palette = "jco",        
                sort.val = "none",      
                sort.by.groups = TRUE,  
                x.text.angle = text_angle
) + ylab('Number of TV commercials') + ggtitle('Time Slot') + xlab(NULL) + 
  theme_hc() + font("x.text", size = font_size)
plot_timeslot

# Plot: TV Commercials by position in break
data <- df %>% group_by(position_in_break) %>% 
  summarise(no_rows = length(position_in_break)) 
print(data)

target <- c('first position','second position','any other position',
            'second last position', 'last position')
data$position_in_break <- factor(data$position_in_break, levels = target)

plot_position <- ggbarplot(data, x = "position_in_break", y = "no_rows",
                fill = barcolour1,       
                color = barcolour1,      
                palette = "jco",         
                sort.val = "none",       
                sort.by.groups = FALSE,  
                x.text.angle = text_angle
) + ylab('Number of TV commercials') + ggtitle('Position in Break') + xlab(NULL) + 
  theme_hc() + font("x.text", size = font_size)
plot_position

# Combine vertical plots in grid
ggarrange(plot_prod_category, 
          plot_length,
          plot_timeslot,
          plot_position,
          labels = c("(a)", "(b)", "(c)",'(d)'),
          align = "h",
          ncol = 2, nrow = 2
          ) # 11 x 7 inch


