library(rsample)
library(recipes)
library(workflows)
library(tune)

tuesdata <- tidytuesdayR::tt_load('2020-07-28')

penguins <- tuesdata$penguins

penguins %>% View()

penguins %>% na.omit %>%
  ggplot(aes(flipper_length_mm, bill_length_mm, color = sex)) + geom_point(alpha = 0.8)


penguins %>% count(species)

penguins %>% na.omit %>%
  ggplot(aes(flipper_length_mm, bill_depth_mm, color = sex)) + 
      geom_point(alpha = 0.8) + 
      facet_wrap(~species)


penguins %>% na.omit %>%
  ggplot(aes(flipper_length_mm, body_mass_g, color = sex)) + 
  geom_point(alpha = 0.8) 

penguins %>% na.omit %>% count(species, island, sex) %>% 
    ggplot(aes(species, n, fill = sex)) + geom_col() + facet_wrap(~island, scales = "free_x")


# Work out with imputation

penguins_df <- penguins %>% na.omit %>% select(-year, -island) %>%mutate_if(is.character, as.factor)

set.seed(11052019)
initial_split <- rsample::initial_split(penguins_df, strata = sex)
training_set <- rsample::training(initial_split)
testing_set <- rsample::testing(initial_split)

## Boostraping the data


penguin_boot <- bootstraps(training_set)


logistic_spec <- logistic_reg() %>% set_engine("glm") %>% set_mode("classification")

rf_spec <-rand_forest() %>% set_engine("ranger") %>% set_mode("classification")


# Building workflow 

penguin_wf <- workflow() %>% add_formula(sex ~ . )

glm_res <- penguin_wf %>% add_model(logistic_spec) %>% fit_resamples(resamples = penguin_boot, 
                                                          control = control_resamples(save_pred = TRUE, verbose = TRUE))

rf_res <- penguin_wf %>% add_model(rf_spec) %>% fit_resamples(resamples = penguin_boot, 
                                                                     control = control_resamples(save_pred = TRUE, verbose = TRUE))



collect_metrics(rf_res)
collect_metrics(glm_res)



glm_res %>% conf_mat_resampled()


glm_res %>% collect_predictions() %>% group_by(id) %>% roc_curve(sex, .pred_female) %>% 
  ggplot(aes(1 - specificity, sensitivity, color = id)) + geom_abline(lty = 3, color = "gray70", size = 1) +
  geom_path(show.legend = FALSE, alpha = 0.7, size = 1) + coord_equal()


rf_res %>% collect_predictions() %>% group_by(id) %>% roc_curve(sex, .pred_female) %>% 
  ggplot(aes(1 - specificity, sensitivity, color = id)) + geom_abline(lty = 3, color = "gray70", size = 1) +
  geom_path(show.legend = FALSE, alpha = 0.7, size = 1) + coord_equal()
  

# Testing the data
final_res <- penguin_wf %>% add_model(logistic_spec) %>% last_fit(initial_split)


final_res %>% collect_predictions() %>% conf_mat(sex, .pred_class)
final_res %>% collect_metrics()
  

final_res$.workflow[[1]] %>% tidy(exponentiate = TRUE)  %>% arrange(estimate)



  
