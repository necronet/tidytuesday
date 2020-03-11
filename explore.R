library(tidytuesdayR)
library(dplyr)
library(ggplot2)
library(maps)
library(ggthemes)
us_states <- map_data("state")

# References links:
# - https://socviz.co/maps.html

# As provided on https://github.com/rfordatascience/tidytuesday/blob/master/data/2020/2020-03-10/readme.md
fetchData <- function() {
  # Get the Data
  # Or read in with tidytuesdayR package (https://github.com/thebioengineer/tidytuesdayR)
  # PLEASE NOTE TO USE 2020 DATA YOU NEED TO USE tidytuesdayR version ? from GitHub
  
  # Either ISO-8601 date or year/week works!
  
  # Install via devtools::install_github("thebioengineer/tidytuesdayR")
  
  tidytuesdayR::tt_load('2020-03-10')
}


tuesdata <- fetchData()

tuesdata$tuition_cost
tuesdata$tuition_income
tuesdata$diversity_school
tuesdata$historical_tuition
tuesdata$student_diversity
tuesdata$salary_potential
tuesdata$`all-schools`


tuesdata$tuition_cost %>% dplyr::left_join(tuesdata$diversity_school, by = c("name","state"))

# TODO: this does not contains all state in the dataset as `state.name` array does not contain 
# US territories such as Puerto rico or America Samoa 
# I was surprise that Hawaii and Alaska are not even included

plot_map_avg_tuition <- function(map_data) {
  p <- ggplot(data = map_data,
              mapping = aes(x = long, y = lat,
                            group = group, fill=average_in_state_tuition))
  p + geom_polygon(color = "gray90", size = 0.1) +
    coord_map(projection="albers", lat0 = 39, lat1 = 45) +
    labs(title = "Average in state tuition in the US") + theme_map() + labs(fill="Average in state tuition") 
}

map_data <- tuesdata$tuition_cost %>% mutate(region=tolower(state.name[match(state_code, state.abb)])) %>%
  group_by(region) %>% summarise(average_in_state_tuition = mean(in_state_tuition)) %>% dplyr::inner_join(us_states) 

plot_map_avg_tuition(map_data)
  

    
                      
  

