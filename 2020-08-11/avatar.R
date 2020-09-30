library(forcats)
library(tidytext)
library(tidylo)
library(tidyr)
library(rsample)
library(textrecipes)
library(parsnip)
library(workflows)
library(tune)
library(yardstick)
library(vip)
library(stringr)

tuesdata <- tidytuesdayR::tt_load('2020-08-11')

avatar <- tuesdata$avatar

avatar %>% count(character, sort = TRUE)
avatar %>% count(chapter_num, sort = TRUE)
avatar %>% count(book, sort = TRUE)

avatar %>% filter(!is.na(character_words)) %>% 
           mutate(book = fct_inorder(book), character = fct_lump_n(character, 10)) %>%
           count(book, character) %>% mutate(character = reorder_within(character, n, book)) %>%
           ggplot(aes(n, character, fill = book)) + scale_y_reordered() +
           geom_col(show.legend = FALSE) + facet_wrap(~book, scales = "free") + labs(y = NULL)

avatar_df <- avatar %>% filter(!is.na(character_words)) %>% mutate(aang = if_else(character == "Aang", "Aang", "Other")) %>%
           select(aang, book, text = character_words)

avatar_df %>% filter(aang == "Aang") %>% sample_n(10)

avatar_lo <- avatar_df %>%
  unnest_tokens(word, text) %>%
  count(aang, word) %>%
  bind_log_odds(aang, word, n) %>%
  arrange(-log_odds_weighted)


avatar_lo %>%
  group_by(aang) %>%
  top_n(15) %>%
  ungroup() %>%
  mutate(word = reorder(word, log_odds_weighted)) %>%
  ggplot(aes(log_odds_weighted, word, fill = aang)) +
  geom_col(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~aang, scales = "free") +
  labs(y = NULL)




library(textfeatures)

tf <- textfeatures( avatar_df, sentiment = FALSE, word_dims = 0, normalize = FALSE, verbose = FALSE)

tf %>%
  bind_cols(avatar_df) %>%
  group_by(aang) %>%
  summarise(across(starts_with("n_"), mean)) %>%
  pivot_longer(starts_with("n_"), names_to = "text_feature") %>%
  filter(value > 0.01) %>%
  mutate(text_feature = fct_reorder(text_feature, -value)) %>%
  ggplot(aes(aang, value, fill = aang)) +
  geom_col(position = "dodge", alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~text_feature, scales = "free", ncol = 6) +
  labs(x = NULL, y = "Mean text features per spoken line")


set.seed(11052019)
avatar_split <- initial_split(avatar_df, strata = aang)
avatar_train <- training(avatar_split)
avatar_test <- testing(avatar_split)


avatar_folds <- vfold_cv(avatar_train, strata = aang)
avatar_folds


avatar_rec <- recipe(aang ~ text, data = avatar_train) %>%
  step_downsample(aang) %>%
  step_textfeature(text) %>%
  step_zv(all_predictors()) %>%
  step_normalize(all_predictors())

avatar_prep <- prep(avatar_rec)


rf_spec <- rand_forest(trees = 500) %>%
  set_engine("ranger") %>%
  set_mode("classification")

svm_spec <- svm_rbf(cost = 0.5) %>%
  set_engine("kernlab") %>%
  set_mode("classification")

avatar_wf <- workflow() %>% add_recipe(avatar_rec)

doParallel::registerDoParallel()

rf_rs <- avatar_wf %>%
  add_model(rf_spec) %>%
  fit_resamples(
    resamples = avatar_folds,
    metrics = metric_set(roc_auc, accuracy, sens, spec),
    control = control_grid(save_pred = TRUE)
  )

svm_rs <- avatar_wf %>%
  add_model(svm_spec) %>%
  fit_resamples(
    resamples = avatar_folds,
    metrics = metric_set(roc_auc, accuracy, sens, spec),
    control = control_grid(save_pred = TRUE)
  )

collect_metrics(rf_rs)
conf_mat_resampled(rf_rs)

collect_metrics(svm_rs)
conf_mat_resampled(svm_rs)

svm_rs %>%
  collect_predictions() %>%
  group_by(id) %>%
  roc_curve(aang, .pred_Aang) %>%
  ggplot(aes(1 - specificity, sensitivity, color = id)) +
  geom_abline(lty = 2, color = "gray80", size = 1.5) +
  geom_path(show.legend = FALSE, alpha = 0.6, size = 1.2) +
  coord_equal()

avatar_imp <- avatar_wf %>%
  add_model(svm_spec) %>%
  fit(avatar_train) %>%
  pull_workflow_fit() %>%
  vi(
    method = "permute", nsim = 10,
    target = "aang", metric = "auc", reference_class = "Other",
    pred_wrapper = kernlab::predict, train = juice(avatar_prep)
  )

avatar_imp %>%
  slice_max(Importance, n = 8) %>%
  mutate(
    Variable = str_remove(Variable, "textfeature_text_n_"),
    Variable = fct_reorder(Variable, Importance)
  ) %>%
  ggplot(aes(Importance, Variable, color = Variable)) +
  geom_errorbar(aes(xmin = Importance - StDev, xmax = Importance + StDev),
                alpha = 0.5, size = 1.3
  ) +
  geom_point(size = 3) +
  theme(legend.position = "none") +
  labs(y = NULL)

