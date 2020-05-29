# Based on screencast from J. Silge 
# https://www.youtube.com/watch?v=_1msVvPE_KY&t=0s
library(readr)
library(stringr)
library(tidyr)
library(tidymodels)
library(forcats)
library(tidytext)

boston_cocktails <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-26/boston_cocktails.csv')


boston_cocktails %>% count(ingredient, sort = T) %>% View()


cocktail_parsed <- boston_cocktails %>% mutate(ingredient = str_to_lower(ingredient), 
                            ingredient = str_replace_all(ingredient, "-", " "),
                            ingredient = str_remove(ingredient, " liqueur| (if desired)"),
                            ingredient = case_when(
                                                  str_detect(ingredient, "bitters") ~ "bitters", 
                                                  str_detect(ingredient, "orange") ~ "orange juice", 
                                                  str_detect(ingredient, "lemon") ~ "lemon juice", 
                                                  str_detect(ingredient, "lime") ~ "lime juice", 
                                                  str_detect(ingredient, "grapefruit") ~ "grapefruit juice", 
                                                  T ~ ingredient
                                                   ),
                            measure = case_when( str_detect(ingredient, "bitters") ~ str_replace(measure, "oz$", "dash"),
                                                T ~ measure),
                            measure = str_replace(measure, " ?1/2",".5"),
                            measure = str_replace(measure, " ?3/4",".75"),
                            measure = str_replace(measure, " ?1/4",".25"),
                            measure_number = parse_number(measure),
                            measure_number = if_else(str_detect(measure, "dash$"), measure_number / 50, measure_number)) %>%
                            add_count(ingredient) %>%
                            filter( n > 15) %>% select(-n) %>% distinct(row_id, ingredient, .keep_all = T)
                            

cocktails_df <- cocktail_parsed %>% select(-ingredient_number, -row_id, -measure) %>% 
      pivot_wider(names_from = ingredient, values_from = measure_number, values_fill =  list(measure_number = 0) ) %>%
      janitor::clean_names() %>% na.omit


pca_recipe <- recipe(~ ., data = cocktails_df) %>% update_role(name, category, new_role = "id") %>% 
      step_normalize(all_predictors()) %>% 
      step_pca(all_predictors())

pca_prep <- prep(pca_recipe)

pca_prep %>% tidy(2) %>% filter(component %in% paste0("PC",1:5)) %>% mutate(component = fct_inorder(component)) %>% 
                          ggplot(aes(value, terms, fill = terms)) + geom_col(show.legend=F) + 
                          facet_wrap(~component, nrow = 1) + labs(y = NULL)
                      


pca_prep %>% tidy(2) %>% filter(component %in% paste0("PC",1:4)) %>% 
  group_by(component) %>% top_n(10, abs(value)) %>% ungroup() %>% 
  mutate(terms = reorder_within(terms, abs(value), component)) %>%
  ggplot(aes(abs(value), terms, fill = value > 0)) + geom_col(show.legend=T) + 
  scale_y_reordered() +
  facet_wrap(~component, scales = "free_y") + labs(y = NULL, fill = "Positive")

juice(pca_prep) %>% ggplot(aes(PC1, PC2, label = name)) + 
    geom_point(aes(color = category),alpha = 0.5, size = 2) + 
    geom_text(check_overlap = T, hjust = "inward") + 
    labs(color = NULL)


library(embed)

umap_recipe <- recipe(~ ., data = cocktails_df) %>% update_role(name, category, new_role = "id") %>% 
  step_normalize(all_predictors()) %>% 
  step_umap(all_predictors())

umap_prep <- prep(umap_recipe)

juice(umap_prep) %>% ggplot(aes(umap_1, umap_2, label = name)) + 
  geom_point(aes(color = category),alpha = 0.5, size = 2) + 
  geom_text(check_overlap = T, hjust = "inward") + 
  labs(color = NULL)

                  