library(dplyr)
library(tidytext)
library(forcats)
theme_set(theme_light())

tuesdata <- tidytuesdayR::tt_load('2020-08-18')
threats <- tuesdata$threats
actions <- tuesdata$actions


plants %>% count(country = fct_lump(country, 5 ), sort=T) %>% 
  mutate(country = fct_reorder(country, n)) %>%
  ggplot(aes(n, country)) + geom_col()


plants %>% count(continent, sort=T) %>% 
  mutate(continent = fct_reorder(continent, n)) %>%
  ggplot(aes(n, continent)) + geom_col()

plants %>% count(red_list_category, sort=T)

plants %>% count(group, sort=T)

plants %>% 
  filter(!is.na(year_last_seen)) %>%
  mutate(year_last_seen = fct_relevel(year_last_seen, "Before 1900")) %>% 
  count(year_last_seen, continent) %>% ggplot(aes(year_last_seen, n, fill = continent)) + geom_col()

## Explore threats


threats %>% filter(threatened == 1) %>% 
  count(threat_type, continent, sort = T) %>% 
  mutate(threat_type = fct_reorder(threat_type, n, sum),
         threat_type = reorder_within(threat_type, n, continent)) %>%
  ggplot(aes(n, threat_type)) + geom_col() + scale_y_reordered() +
  facet_wrap(~ continent, scales = "free")


threats %>% 
  filter(!is.na(year_last_seen), threatened == 1) %>%
  count(year_last_seen, threat_type) %>% 
  mutate(threat_type = fct_reorder(threat_type, -n, sum)) %>% 
  ggplot(aes(year_last_seen, n)) + geom_col() + facet_wrap(~threat_type) +
  theme(axis.text.x = element_text(angle=90, hjust = 1)) + labs(x = "Last seen", y = "# of plants extinct")



actions %>% View()

