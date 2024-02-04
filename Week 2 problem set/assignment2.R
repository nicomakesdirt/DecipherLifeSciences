# Load packages
library(tidyverse)
library(dslabs)

# Display all diseases in the data set
disease_levels <- us_contagious_diseases |> 
  pull(disease) |>
  levels()
print(disease_levels)

# Filter to measles, excluding Alaska and Hawaii, summarizing rate by year
avg <- us_contagious_diseases |>
  filter(disease == "Measles" &
         !state %in% c("Alaska","Hawaii") &
         weeks_reporting > 0) |>
  # mutate(rate = count / (population / 10^5) * (52 / weeks_reporting)) |>
  group_by(year) |>
  summarize(rate = sum(count * 52 / weeks_reporting, na.rm=TRUE) / sum(population / 10^5, na.rm=TRUE))
head(avg)
