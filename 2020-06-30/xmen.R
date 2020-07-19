library(tidytuesdayR)
library(dplyr)
library(tidyr)
library(forcats)
library(rsample)
library(purrr)
library(broom)

tuesdata <- tidytuesdayR::tt_load('2020-06-30')


character_visualization <- tuesdata$character_visualization
xmen_bechdel <- tuesdata$xmen_bechdel
locations <- tuesdata$locations


per_issue <- character_visualization %>%
  group_by(issue) %>%
  summarise(across(speech:depicted, sum)) %>%
  ungroup()


x_mansion <- locations %>%
  group_by(issue) %>%
  summarise(mansion = "X-Mansion" %in% location)

locations_joined <- per_issue %>%
  inner_join(x_mansion)

locations_joined %>%
  mutate(mansion = if_else(mansion, "X-Mansion", "No mansion")) %>%
  pivot_longer(speech:depicted, names_to = "visualization") %>%
  mutate(visualization = fct_inorder(visualization)) %>%
  ggplot(aes(mansion, value, fill = visualization)) +
  geom_dotplot(
    binaxis = "y", stackdir = "center",
    binpositions = "all",
    show.legend = FALSE
  ) +
  facet_wrap(~visualization, scales = "free_y") +
  labs(
    x = NULL, y = NULL,
    title = "Which issues contain the X-Mansion as a location?",
    subtitle = "Comparing the top 25 characters' speech, thought, narrative portrayal, and total depictions",
    caption = "Data from the Claremont Run Project"
  )


set.seed(123)
boots <- bootstraps(locations_joined, times = 1000, apparent = TRUE)

boot_models <- boots %>%
  mutate(
    model = map(
      splits,
      ~ glm(mansion ~ speech + thought + narrative + depicted,
            family = "binomial", data = analysis(.)
      )
    ),
    coef_info = map(model, broom::tidy)
  )

boot_coefs <- boot_models %>%
  unnest(coef_info)

int_pctl(boot_models, coef_info)

boot_coefs %>%
  filter(term != "(Intercept)") %>%
  mutate(term = fct_inorder(term)) %>%
  ggplot(aes(estimate, fill = term)) +
  geom_vline(
    xintercept = 0, color = "gray50",
    alpha = 0.6, lty = 2, size = 1.5
  ) +
  geom_histogram(alpha = 0.8, bins = 25, show.legend = FALSE) +
  facet_wrap(~term, scales = "free") +
  labs(
    title = "Which issues contain the X-Mansion as a location?",
    subtitle = "Comparing the top 25 characters' speech, thought, narrative portrayal, and total depictions",
    caption = "Data from the Claremont Run Project"
  )



bechdel_joined <- per_issue %>%
  inner_join(xmen_bechdel) %>%
  mutate(pass_bechdel = if_else(pass_bechdel == "yes", TRUE, FALSE))

bechdel_joined %>%
  mutate(pass_bechdel = if_else(pass_bechdel, "Passes Bechdel", "Fails Bechdel")) %>%
  pivot_longer(speech:depicted, names_to = "visualization") %>%
  mutate(visualization = fct_inorder(visualization)) %>%
  ggplot(aes(pass_bechdel, value, fill = visualization)) +
  geom_dotplot(
    binaxis = "y", stackdir = "center",
    binpositions = "all",
    show.legend = FALSE
  ) +
  facet_wrap(~visualization, scales = "free_y") +
  labs(
    x = NULL, y = NULL,
    title = "Which Uncanny X-Men issues pass the Bechdel test?",
    subtitle = "Comparing the top 25 characters' speech, thought, narrative portrayal, and total depictions",
    caption = "Data from the Claremont Run Project"
  )

set.seed(123)
boots <- bootstraps(bechdel_joined, times = 1000, apparent = TRUE)

boot_models <- boots %>%
  mutate(
    model = map(
      splits,
      ~ glm(pass_bechdel ~ speech + thought + narrative + depicted,
            family = "binomial", data = analysis(.)
      )
    ),
    coef_info = map(model, tidy)
  )

boot_coefs <- boot_models %>%
  unnest(coef_info)

int_pctl(boot_models, coef_info)

boot_coefs %>%
  filter(term != "(Intercept)") %>%
  mutate(term = fct_inorder(term)) %>%
  ggplot(aes(estimate, fill = term)) +
  geom_vline(
    xintercept = 0, color = "gray50",
    alpha = 0.6, lty = 2, size = 1.5
  ) +
  geom_histogram(alpha = 0.8, bins = 25, show.legend = FALSE) +
  facet_wrap(~term, scales = "free") +
  labs(
    title = "Which Uncanny X-Men issues pass the Bechdel test?",
    subtitle = "Comparing the top 25 characters' speech, thought, narrative portrayal, and total depictions",
    caption = "Data from the Claremont Run Project"
  )


