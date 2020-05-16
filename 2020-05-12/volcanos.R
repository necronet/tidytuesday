# Based on the work by Julia Silge
# https://www.youtube.com/watch?v=0WCmLYvfHMw
library(stringr)
library(tidymodels)
library(themis)
library(vip)
library(janitor)

tuesdata <- tidytuesdayR::tt_load('2020-05-12')
volcano_raw <- tuesdata$volcano

volcano_df <- volcano_raw %>% transmute(volcano_type = case_when(str_detect(primary_volcano_type,"Stratovolcano") ~"Stranovolcano",
                                                   str_detect(primary_volcano_type, "Shield")~ "Shield",
                                                   T ~ "Other"), 
                          volcano_number,latitude, longitude, elevation, tectonic_settings, major_rock_1) %>% 
                          mutate_if(is.character, factor)

world <- map_data("world")

ggplot() + 
  geom_map(data = world, map = world, mapping = aes(long, lat, map_id = region), color = "white", fill = "gray50", alpha = 0.2) + 
  geom_point(volcano_df, mapping = aes(longitude, latitude, color = volcano_type), size = 0.5, alpha = 0.9)



# Building a model

volcano_boot <- bootstraps(volcano_df)

volcano_recipe <- recipe(volcano_type ~ ., data = volcano_df) %>% 
            update_role(volcano_number, new_role = "Id") %>% 
            step_other(tectonic_settings, threshold = 0.08) %>%
            step_other(major_rock_1) %>% step_dummy(all_nominal(), -all_outcomes()) %>% 
            step_zv(all_predictors()) %>% step_normalize(all_predictors()) %>% step_smote(volcano_type)

volcano_prep <- prep(volcano_recipe)

rf_spec <- rand_forest(trees = 500) %>% set_mode("classification") %>% set_engine("ranger")



volcano_workflow <- workflows::workflow() %>% add_recipe(volcano_recipe) %>% add_model(rf_spec)


volcano_rf_results <- fit_resamples(volcano_workflow, 
                                    resamples = volcano_boot, 
                                    control = control_resamples(save_pred = T, verbose = T))


volcano_rf_results %>% collect_metrics()

volcano_rf_results %>% collect_predictions() %>% conf_mat(volcano_type, .pred_class)

volcano_rf_results %>% collect_predictions()  %>% ppv(volcano_type, .pred_class)


rf_spec %>% set_engine("ranger", importance = "permutation") %>% 
            fit(volcano_type ~ ., data = volcano_prep %>% juice() %>% select(-volcano_number) %>% janitor::clean_names()) %>%
            vip(geom = "point")

volcano_predictions <- volcano_rf_results %>% collect_predictions() %>% mutate(correct = volcano_type == .pred_class) %>%
                       left_join(volcano_df %>% mutate(.row = row_number()))




ggplot() + 
  geom_map(data = world, map = world, mapping = aes(long, lat, map_id = region), color = "white", fill = "gray50", alpha = 0.2) + 
  stat_summary_hex(data = volcano_predictions, aes(longitude, latitude, z = as.integer(correct)), 
                   fun = "mean", alpha = 0.7, bins = 60) + scale_fill_gradient(high = "red", labels = scales::percent)



