# Exploring dataset for the office TV show
# more info on: https://github.com/rfordatascience/tidytuesday/blob/master/data/2020/2020-03-17/readme.md
library(ggplot2)
library(tidytuesdayR)
library(dplyr)
library(tidyr)
library(hrbrthemes)

tuesdata <- tidytuesdayR::tt_load('2020-03-17')
office_ratings <- tuesdata$office_ratings

max_date <-max(office_ratings$air_date)

rects <- office_ratings %>% group_by(season) %>% arrange(season) %>% filter(row_number()==1 | row_number()==n()) %>% 
                   mutate(type = ifelse(row_number() == 1, "start","end"))  %>% select(season, air_date, type) %>%
                   pivot_wider(names_from = type, values_from = air_date) %>% ungroup() %>% 
                   mutate(end = dplyr::lead(start, default = max_date), season = as.factor(season)) 

mapping <- aes(x = air_date, y = imdb_rating)
office_ratings %>% ggplot() + 
                   geom_rect(data = rects, aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf, fill = season), alpha = 0.2) +
                   geom_point(mapping) +
                   stat_smooth(aes(x = air_date, y = imdb_rating, color = as.factor(season), fill = as.factor(season)), method = "lm", alpha = 0.2) +
                   xlab("Release date") + ylab("IMBD rating") +
                   theme_ipsum() +
                   theme(axis.text.x=element_text(angle=60, hjust=1), legend.position = "none") +
                   scale_x_date(limit=c(as.Date("2005-03-24"),as.Date("2013-05-16")), date_labels = "%Y-%b") + 
                   ggtitle("The office rating trend(IMDB) accross seasons")


