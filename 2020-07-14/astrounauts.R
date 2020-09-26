library(rsample)
library(recipes)
library(stringr)
library(baguette)
library(workflows)
library(yardstick)
library(tidyr)

data <- tidytuesdayR::tt_load('2020-07-14')
astronauts <- data$astronauts

#astronauts %>% View()

astronauts %>% mutate(year_of_mission = 10 * (year_of_mission %/% 10),
                      year_of_mission = factor(year_of_mission)) %>%
              ggplot(aes(year_of_mission, hours_mission)) + geom_boxplot() + scale_y_log10()



astronauts_df <- astronauts %>% select(name, mission_title, hours_mission, 
                military_civilian, occupation,year_of_mission, in_orbit) %>% 
                filter(hours_mission > 0 ) %>%
                mutate(in_orbit = case_when(str_detect(in_orbit, "^Salyut") ~ "Salyut",
                                            str_detect(in_orbit,"^STS") ~ "STS", 
                                            TRUE ~ in_orbit ),
                       occupation = str_to_lower(occupation)) %>%
                mutate(hours_mission = log(hours_mission)) %>% na.omit()

set.seed(11052019)

split <- initial_split(astronauts_df, strata = hours_mission)

training_data <- training(split)
test_data <- testing(split)


astro_recipe <- recipe(hours_mission ~ ., data = training_data) %>% 
    update_role(name, mission_title, new_role = "id") %>%
    step_other(occupation, in_orbit, threshold = 0.005) %>%
    step_dummy(all_nominal(), -has_role("id"))
          
astro_recipe %>% prep() %>% juice() %>% names



astro_workflow <- workflow() %>% add_recipe(astro_recipe) 

tree_spec <- bag_tree() %>% set_engine("rpart", times = 25) %>% set_mode("regression")

mars_spec <- bag_mars() %>% set_engine("earth", times = 25) %>% set_mode("regression")


tree_rs <- astro_workflow %>% add_model(tree_spec) %>% fit(training_data)
mars_rs <- astro_workflow %>% add_model(mars_spec) %>% fit(training_data)


test_data %>% bind_cols(predict(tree_rs, test_data)) %>% rename(.pred_tree = .pred) %>% 
  bind_cols(predict(mars_rs, test_data)) %>% rename(.pred_mars = .pred) %>%
  metrics(hours_mission, .pred_tree)

test_data %>% bind_cols(predict(tree_rs, test_data)) %>% rename(.pred_tree = .pred) %>% 
              bind_cols(predict(mars_rs, test_data)) %>% rename(.pred_mars = .pred) %>%
              metrics(hours_mission, .pred_mars)
    

new_astrounaut <- crossing(in_orbit = c("ISS", "Mir", "STS", "other"), 
                                       military_civilian = "civilian", occupation = "other",
                                       year_of_mission = seq(1960, 2020, by = 10),
                                       name = "new-ast", mission_title = "simulated") %>%
                    filter(
                      !(in_orbit == "ISS" & year_of_mission < 2000),
                      !(in_orbit == "Mir" & year_of_mission < 1990),
                      !(in_orbit == "STS" & year_of_mission > 2010),
                      !(in_orbit == "STS" & year_of_mission < 1980)
                    )

new_astrounaut %>%
  bind_cols(predict(tree_rs, new_astrounaut)) %>%
  ggplot(aes(year_of_mission, .pred, color = in_orbit)) +
  geom_line(size = 1.5, alpha = 0.7) +
  geom_point(size = 2)
  

