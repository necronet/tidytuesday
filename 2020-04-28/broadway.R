# Based on David Robinson live coding https://youtu.be/OhY5ZaILRpg
library(tidytuesdayR)
library(dplyr)
library(ggplot2)
library(tidymetrics)
library(devtools)


install_github("datacamp/tidymetrics")
# Loading tidy tuesday data
tuesdata <- tidytuesdayR::tt_load('2020-04-28')

grosses <- tuesdata$grosses
synopses <- tuesdata$synopses
cpi <- tuesdata$cpi
pre_1985_starts <- tuesdata$`pre-1985-starts`

grosses %>% arrange(-weekly_gross) %>% select(show) %>% unique

grosses %>% dplyr::filter(show %in% c('Hamilton', 'The Lion King')) %>% 
            rename(date = week_ending) %>% 
            cross_by_periods(c("month","quarter")) %>%
            summarise(gross = sum(weekly_gross), avg_gross=mean(avg_ticket_price)) %>%
            ggplot(aes(date, gross, colour = period)) +
            scale_y_continuous(labels = scales::dollar) +
            geom_line() + expand_limits(y = 0) 


  tdmetrics_broadway_gross <- grosses %>% dplyr::filter(show %in% c('Wicked')) %>% 
    rename(date = week_ending) %>% 
    cross_by_periods(c(), windows = 28) %>%
    summarise(gross = sum(weekly_gross), 
              avg_gross=mean(avg_ticket_price),
              nb_seats_sold = sum(seats_sold),
              pct_capacity = mean(pct_capacity) )
    
    
  tdmetrics_broadway_gross %>% ggplot(aes(date, avg_gross, colour = period)) +
    scale_y_continuous(labels = scales::dollar) +
    geom_line(size =.5, alpha = .6) + expand_limits(y = 0) 
  
  
  tdmetrics_broadway_gross %>% ggplot(aes(date, avg_gross, colour = period)) +
    scale_y_continuous(labels = scales::dollar) +
    geom_line(size =.5, alpha = .6) + expand_limits(y = 0)
  
  
  tdmetrics_broadway_gross %>% ggplot(aes(date, pct_capacity, colour = period)) +
    scale_y_continuous(labels = scales::percent_format()) +
    geom_line(size =.5, alpha = .6) 


  
   





