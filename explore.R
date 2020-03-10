library(redr)

# As provided on https://github.com/rfordatascience/tidytuesday/blob/master/data/2020/2020-03-10/readme.md
fetchData <- function() {
  # Get the Data
  
  tuition_cost <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-10/tuition_cost.csv')
  
  tuition_income <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-10/tuition_income.csv') 
  
  salary_potential <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-10/salary_potential.csv')
  
  historical_tuition <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-10/historical_tuition.csv')
  
  diversity_school <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-10/diversity_school.csv')
  
  # Or read in with tidytuesdayR package (https://github.com/thebioengineer/tidytuesdayR)
  # PLEASE NOTE TO USE 2020 DATA YOU NEED TO USE tidytuesdayR version ? from GitHub
  
  # Either ISO-8601 date or year/week works!
  
  # Install via devtools::install_github("thebioengineer/tidytuesdayR")
  
  #tuesdata <- tidytuesdayR::tt_load('2020-03-10')
  #tuesdata <- tidytuesdayR::tt_load(2020, week = 11)
  #tuition_cost <- tuesdata$tuition_cost
}


fetchData()