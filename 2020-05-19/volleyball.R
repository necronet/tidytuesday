library(stringr)
library(tidyr)
library(tidymodels)
library(vip)
library(devtools)

# Handle crash with dylib
# OMP: Error #15: Initializing libomp.dylib, but found libomp.dylib already initialized.
Sys.setenv(KMP_DUPLICATE_LIB_OK = TRUE)

# Using dplyr 1.0.0 not yet release
install_github("tidyverse/dplyr")

tuesdata <- tidytuesdayR::tt_load('2020-05-19')
vb_matches <- tuesdata$vb_matches

vb_parse <- vb_matches %>% transmute(circuit, gender, year, 
                         w_attacks = w_p1_tot_attacks + w_p2_tot_attacks, 
                         w_kills = w_p1_tot_kills + w_p2_tot_kills,
                         w_errors = w_p1_tot_errors + w_p2_tot_errors,
                         w_aces = w_p1_tot_aces + w_p2_tot_aces,
                         w_serve_errors = w_p1_tot_serve_errors + w_p2_tot_serve_errors,
                         w_blocks = w_p1_tot_blocks + w_p2_tot_blocks,
                         w_digs = w_p1_tot_digs + w_p2_tot_digs,
                         
                         l_attacks = l_p1_tot_attacks + l_p2_tot_attacks, 
                         l_kills = l_p1_tot_kills + l_p2_tot_kills,
                         l_errors = l_p1_tot_errors + l_p2_tot_errors,
                         l_aces = l_p1_tot_aces + l_p2_tot_aces,
                         l_serve_errors = l_p1_tot_serve_errors + l_p2_tot_serve_errors,
                         l_blocks = l_p1_tot_blocks + l_p2_tot_blocks,
                         l_digs = l_p1_tot_digs + l_p2_tot_digs) %>% na.omit


winners <- vb_parse %>% select(circuit, gender, year, w_attacks:w_digs) %>% 
            rename_with(~ str_remove_all(.,  "w_"), w_attacks:w_digs) %>% 
            mutate(win = "win")
losers <- vb_parse %>% select(circuit, gender, year, l_attacks:l_digs) %>% 
              rename_with(~ str_remove_all(.,  "l_"), l_attacks:l_digs) %>% 
            mutate(win = "lose")


vb_df <- bind_rows(winners, losers) %>% mutate_if(is.character, factor)


vb_df %>% pivot_longer(attacks:digs, names_to = "stat", values_to  = "value") %>%
          ggplot(aes(gender, value, fill = win, color = win)) + 
          geom_boxplot(alpha = 0.5) +facet_wrap(~stat, scales = "free_y", nrow = 2) +
          labs(y = NULL, color = NULL, fill = NULL)


vb_df %>% count(circuit)



set.seed(11052019)
vb_split <- initial_split(vb_df, strata = win)
vb_train <- training(vb_split)
vb_test <- testing(vb_split)


xgboost_spec <- boost_tree(
  trees = 500,
  tree_depth = tune(), 
  min_n = tune(),
  loss_reduction = tune(),
  mtry = tune(),
  learn_rate = tune(),
  sample_size = tune()
) %>% set_engine("xgboost") %>% set_mode("classification")


xgboost_grid <- grid_latin_hypercube(tree_depth(), min_n(), 
                                     loss_reduction(), 
                                     sample_size = sample_prop(), 
                                     finalize(mtry(), vb_train), learn_rate(), size = 20)


xgboost_wf <- workflow() %>% add_formula(win ~ .) %>% add_model(xgboost_spec)

set.seed(10052019)
vb_folds <- vfold_cv(vb_train, strata = win)

doParallel::registerDoParallel()
set.seed(11052019)

xgboost_rs <- tune_grid(xgboost_wf, resamples = vb_folds, grid = xgboost_grid, control = control_grid(save_pred = TRUE))

xgboost_rs %>% collect_metrics() %>% filter(.metric == "roc_auc") %>% select(mean, mtry:sample_size) %>% 
               pivot_longer(mtry:sample_size, names_to = "parameter", values_to = "value") %>%
               ggplot(aes(value, mean, color = parameter)) + geom_point(show.legend = F) + 
               facet_wrap(~ parameter, scales = "free_x")

#saveRDS(xgboost_rs, "2020-05-19/xgboost_rs.rds")
xgboost_rs <- readRDS("2020-05-19/xgboost_rs.rds")

show_best(xgboost_rs, "roc_auc")

best_auc <- select_best(xgboost_rs, "roc_auc")

xgboost_final <- finalize_workflow(xgboost_wf, best_auc)

xgboost_final %>% fit( data = vb_train) %>% pull_workflow_fit() %>% vip(geom = "point")

final_res <- last_fit(xgboost_final, vb_split)
final_res %>% collect_metrics()


final_res %>% collect_predictions() %>% conf_mat(win, .pred_class) 

final_res %>% collect_predictions() %>% roc_curve(win, .pred_win) %>% autoplot()






