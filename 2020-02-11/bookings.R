# Based on Julia silge coding for tidytuesday
# Screencast: https://www.youtube.com/watch?v=dbXDkEEuvCU

library(tidytuesdayR)
library(dplyr)
library(skimr)
library(ggplot2)
library(GGally)
library(tidymodels)
library(kknn)

tuesdata <- tidytuesdayR::tt_load('2020-02-11')
hotels <- tuesdata$hotels

hotel_stays <- hotels %>% filter( is_canceled == 0) %>% 
    mutate(children = case_when(children + babies > 0 ~ "children", T ~ "none"),
           required_car_parking_spaces = case_when(required_car_parking_spaces > 0 ~"parking", T ~"none")) %>%
    select(-is_canceled, -reservation_status, -babies)
    

skimr::skim(hotel_stays)


# Exploratory Data analysis
hotel_stays %>% mutate(arrival_date_month = factor(arrival_date_month, levels = month.name)) %>%
               count(hotel, arrival_date_month, children, sort = TRUE) %>% group_by(hotel, children) %>% 
               mutate(proportion = n / sum(n)) %>% 
               ggplot(aes(arrival_date_month, proportion, fill = children))  + 
               geom_col(position = "dodge") +
               scale_y_continuous(labels=scales::percent_format()) + facet_wrap(~ hotel, nrow = 2)


hotel_stays <- hotel_stays %>% 
  select(children, hotel, arrival_date_month, meal, adr, adults, required_car_parking_spaces,
         total_of_special_requests, stays_in_week_nights, stays_in_weekend_nights) %>%
         mutate_if(is.character, factor)




hotel_stays %>% count(hotel, required_car_parking_spaces, children, sort = TRUE) %>% group_by(hotel, children) %>% 
  mutate(proportion = n / sum(n)) %>% 
  ggplot(aes(required_car_parking_spaces, proportion, fill = children))  + 
  geom_col(position = "dodge") +
  scale_y_continuous(labels=scales::percent_format()) + facet_wrap(~ hotel, nrow = 1, scales = "free_x")

hotel_stays %>% select(children, adr, required_car_parking_spaces, total_of_special_requests)%>%
  ggpairs(mapping = aes(color = children))


set.seed(11052019)
hotels_stays_split  <- initial_split(hotel_stays)
hotels_stay_train <- training(hotels_stays_split)
hotels_stay_test <- testing(hotels_stays_split)


# Preprocess and feature engineering

hotel_rec <- recipe(children ~  ., data = hotels_stay_train) %>%
  step_downsample(children) %>%
  step_dummy(all_nominal(), -all_outcomes()) %>%
  step_zv(all_numeric()) %>%
  step_normalize(all_numeric()) %>% prep()
  

test_proc <- bake(hotel_rec, new_data = hotels_stay_test)

knn_hotel_spec <- nearest_neighbor() %>% set_engine("kknn") %>% set_mode("classification")

knn_fit <- knn_hotel_spec %>% fit(children ~ ., data = juice(hotel_rec)) 


trees_spec <- decision_tree() %>% set_engine('rpart') %>% set_mode("classification") 

trees_fit <- trees_spec %>% fit(children ~. , data = juice(hotel_rec))

# Evaluate models

set.seed(11052019)
validation_splits <- mc_cv(juice(hotel_rec), prop=0.9, strata = children)

knn_res <- fit_resamples(children ~ ., 
                         knn_hotel_spec, 
                         validation_splits, 
                         control = control_resamples(save_pred = T))

decision_trees_res <- fit_resamples(children ~ ., 
                        trees_spec, 
                         validation_splits, 
                         control = control_resamples(save_pred = T))


knn_res %>% collect_metrics()
decision_trees_res %>% collect_metrics()

knn_res %>% tidyr::unnest(.predictions) %>% mutate(model = "kknn") %>% 
  bind_rows(decision_trees_res %>% tidyr::unnest(.predictions)  %>% mutate(model = "rpart")) %>%
  group_by(model) %>%
  roc_curve(children, .pred_children) %>% autoplot()

knn_res %>% tidyr::unnest(.predictions) %>% conf_mat(children, .pred_class)
decision_trees_res %>% tidyr::unnest(.predictions) %>% conf_mat(children, .pred_class)


knn_fit %>% predict(new_data = test_proc, type = "prob") %>% mutate(truth = hotels_stay_test$children) %>%
  roc_auc(truth, .pred_children)

