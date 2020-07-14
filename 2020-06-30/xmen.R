library(tidytuesdayR)
library(dplyr)
library(tidyr)
library(forcats)
library(rsample)
library(purrr)

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
    coef_info = map(model, tidy)
  )

boot_coefs <- boot_models %>%
  unnest(coef_info)

int_pctl(boot_models, coef_info)

