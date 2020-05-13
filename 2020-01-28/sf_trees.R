# Based on Julia Silge https://www.youtube.com/watch?v=ts5bRZ7pRKQ
library(tidytuesdayR)
library(readr)
library(dplyr)
library(skimr)
library(ggplot2)

# modeling libraries
library(tidymodels)
library(vip)

tuesdata <- tidytuesdayR::tt_load('2020-01-28') 
sf_trees <- tuesdata$sf_trees

sf_trees %>% dplyr::count(legal_status, sort = TRUE)

trees_df <- sf_trees %>% mutate(legal_status = case_when(legal_status == "DPW Maintained" ~ legal_status, TRUE ~ "Other")) %>%
             select(-address) %>% na.omit() %>% mutate(plot_size = parse_number(plot_size) ) %>%
             na.omit() %>% mutate_if(is.character, factor)

skimr::skim(trees_df)


trees_df %>% ggplot(aes(longitude, latitude, color=legal_status)) + geom_point(size = 0.3,alpha = 0.4) + labs(color = NULL)

trees_df %>% count(legal_status, caretaker, sort=TRUE) %>% 
        add_count(caretaker, wt = n, name = "caretaker_count") %>% 
        filter(caretaker_count > 50) %>%
        group_by(legal_status) %>%
        mutate(percent_legal = n / sum(n)) %>%
        ggplot(aes(percent_legal, caretaker, fill = legal_status)) + geom_col(position = "dodge")
  
  
# Modeling
set.seed(11052019)
trees_split <- initial_split(trees_df, strata = legal_status)

trees_training <- training(trees_split)
trees_testing <- testing(trees_split)


tree_recipe <- recipe(legal_status ~ ., data = trees_training) %>% 
    update_role(tree_id, new_role = "ID") %>%
    step_other(species, caretaker, threshold =  0.01) %>%
    step_other(site_info, threshold =  0.005) %>%
    step_dummy(all_nominal(), -all_outcomes()) %>%
    step_date(date, features = c('year')) %>%
    step_rm(date) %>% step_downsample(legal_status)

  
tree_prep <- prep(tree_recipe)

juice(tree_prep) %>% count(legal_status, sort = T)
juice(tree_prep) %>% count(caretaker, sort = T)
juice(tree_prep) %>% count(site_info, sort = T)


tune_spec <- rand_forest(mtry = tune(), trees = 500, min_n = tune()) %>% set_mode(mode  = "classification") %>% set_engine('ranger')

tune_workflow <- workflows::workflow() %>% add_recipe(tree_recipe) %>% add_model(tune_spec)
 
set.seed(10052019)
tree_folds <- vfold_cv(trees_training)

doParallel::registerDoParallel()
set.seed(12052019)

tune_results <- tune_grid(
  tune_workflow,
  resamples = tree_folds,
  grid = 20
)

tune_results %>% collect_metrics() %>% filter(.metric == "roc_auc") %>% select(mean, min_n, mtry) %>% 
    tidyr::pivot_longer(min_n:mtry, values_to = "value", names_to = "parameter") %>% 
    ggplot(aes(value, mean, color = parameter))  + geom_point(show.legend = F) + facet_wrap(~parameter, scales = "free_x")


rf_grid <- grid_regular(
  mtry(range = c(10, 38)),
  min_n(range = c(2, 10)),
  levels = 5
)


regular_tune_results <- tune_grid(
  tune_workflow,
  resamples = tree_folds,
  grid = rf_grid
)

regular_tune_results %>% collect_metrics() %>% filter(.metric == "roc_auc") %>% 
                         mutate(min_n = factor(min_n)) %>% 
                         ggplot(aes(mtry, mean, color = min_n)) + geom_point(show.legend = F) +
                         geom_line(alpha = 0.6, size = 1)

best_auc <- select_best(regular_tune_results, "roc_auc")

final_rf <- finalize_model(tune_spec, best_auc)

vip_final_rf <- final_rf %>% set_engine("ranger", importance = "permutation") %>% 
  fit(legal_status ~ ., data = juice(tree_prep) %>% select(-tree_id)) %>%
  vip(geom = "point")



final_workflow <- workflows::workflow() %>% add_recipe(tree_recipe) %>% add_model(final_rf) 

final_result <- final_workflow %>% last_fit(trees_split)

final_result %>% collect_predictions() %>% 
              mutate( correct = case_when(legal_status == .pred_class ~ "Correct", T ~ "Incorrect")) %>%
              bind_cols(trees_testing) %>%
              ggplot(aes(longitude, latitude, color=correct)) + geom_point(size = 0.5,alpha = 0.7) + labs(color = NULL) +
              scale_color_manual(values = c("gray50","red"))

  