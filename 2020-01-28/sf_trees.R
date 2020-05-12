# Based on Julia Silge https://www.youtube.com/watch?v=ts5bRZ7pRKQ
library(tidytuesdayR)
library(readr)
library(dplyr)
library(skimr)
library(ggplot2)

tuesdata <- tidytuesdayR::tt_load('2020-01-28') 
sf_trees <- tuesdata$sf_trees

sf_trees %>% dplyr::count(legal_status, sort = TRUE)

trees_df <- sf_trees %>% mutate(legal_status = case_when(legal_status == "DPW Maintained" ~ legal_status, TRUE ~ "Other")) %>%
             select(-address) %>% na.omit() %>% mutate(plot_size = parse_number(plot_size) ) %>%
             mutate_if(is.character, factor)

skimr::skim(trees_df)


trees_df %>% ggplot(aes(longitude, latitude, color=legal_status)) + geom_point(size = 0.3,alpha = 0.4) + labs(color = NULL)

trees_df %>% count(legal_status, caretaker, sort=TRUE) %>% 
        add_count(caretaker, wt = n, name = "caretaker_count") %>% 
        filter(caretaker_count > 50) %>%
        group_by(legal_status) %>%
        mutate(percent_legal = n / sum(n)) %>%
        ggplot(aes(percent_legal, caretaker, fill = legal_status)) + geom_col(position = "dodge")
  
  
  