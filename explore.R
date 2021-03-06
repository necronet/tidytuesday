library(tidytuesdayR)
library(dplyr)
library(ggplot2)
library(maps)
library(ggthemes)
library(forcats)
library(tidyr)
library(wesanderson)
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

plot_map_avg_tuition <- function(map_data, props) {
  params = aes(x = long, y = lat, group = group, fill=.data[[props$fill]])
  
  p <- ggplot(data = map_data, mapping=params)
  p + geom_polygon(color = "gray90", size = 0.1) +
    coord_map(projection="albers", lat0 = 39, lat1 = 45) +
    labs(title = props$title) + theme_map() + labs(fill=props$legend_title) +
    scale_fill_gradient(low = "#00d871", high = "#006372")
}



# TODO: this does not contains all state in the dataset as `state.name` array does not contain 
# US territories such as Puerto rico or America Samoa 
# I was surprise that Hawaii and Alaska are not even included
map_data_in_state_avg <- tuesdata$tuition_cost %>% mutate(region=tolower(state.name[match(state_code, state.abb)])) %>%
  group_by(region) %>% summarise(average_in_state_tuition = mean(in_state_tuition)) %>% dplyr::inner_join(us_states) 

map_data_out_state_avg <- tuesdata$tuition_cost %>% mutate(region=tolower(state.name[match(state_code, state.abb)])) %>%
  group_by(region) %>% summarise(average_out_state_tuition = mean(out_of_state_tuition)) %>% dplyr::inner_join(us_states) 

plot_map_avg_tuition(map_data_in_state_avg, list(title="Average in state tuition in the US", legend_title="Avg", fill="average_in_state_tuition"))
plot_map_avg_tuition(map_data_out_state_avg, list(title="Average out of state tuition in the US", legend_title="Avg", fill="average_out_state_tuition"))

percentage_diversity <- function(diversity_enrollment, total, na.rm = FALSE) diversity_enrollment/total


tuesdata$student_diversity %>% head(10) %>% pivot_longer(c(-INSTITUTION,-ENROLLMENT), names_to = "type", values_to="count") %>% 
group_by(INSTITUTION) %>% mutate(percentage = count/ENROLLMENT) %>%
  ggplot(aes(y = percentage, x = fct_reorder(INSTITUTION, ENROLLMENT), fill=type)) + 
  geom_bar(stat="identity", position="fill") +   
  coord_flip() + theme(legend.position = "top")  

# tuesdata$student_diversity %>% head(10) %>%
#   mutate_at(vars(!matches(c("INSTITUTION","ENROLLMENT"))), ~percentage_diversity(., ENROLLMENT, na.rm = TRUE)) %>% 
#   ggplot(aes(y = ENROLLMENT, x = fct_reorder(INSTITUTION, ENROLLMENT))) + 
#   geom_bar(stat="identity") + 
#   coord_flip()



