
library(stringr)
library(dplyr)
library(knitr)
library(kableExtra)

dataset <- read.csv("data/shootings-2018.csv", stringsAsFactors = FALSE)

## SUMMARY INFORMATION
# (1) How many shootings occurred
# (2) How many lives were lost
# (3) Which city was most impacted by shootings,
# with impact measured by the combined number of injured and killed (casualties)
# (4) Which state had the most shootings
# (5) Which state had the most casualties

# (1) How many shootings occurred
number_of_shootings <- nrow(dataset)

# (2) How many were killed in shootings in 2018
number_lost <- dataset %>%
  select(num_killed) %>%
  sum(dataset$num_killed)

# (3) Which city was impacted most by the shootings
# impact is measured by the greatest combined number of injured and killed
impact_data <- dataset %>%
  select(city, num_killed, num_injured) %>%
  group_by(city) %>%
  summarize(num_killed_and_injured =
              sum(num_killed, na.rm = TRUE) +
              sum(num_injured, na.rm = TRUE)) %>%
  filter(num_killed_and_injured == max(num_killed_and_injured, na.rm = TRUE))

# Most impacted city
most_impacted_city <- impact_data %>%
  pull(city)

# Number of casualties in the most impacted city
most_impact_city_numbers <- impact_data %>%
  pull(num_killed_and_injured)

# (4) Which state had the most shootings
state_data <- dataset %>%
  select(state) %>%
  group_by(state) %>%
  summarize(number_of_shootings = length(state))

# State(s) with most shootings
state_most_shootings <- state_data %>%
  filter(number_of_shootings == max(number_of_shootings, na.rm = TRUE)) %>%
  pull(state)

# Number of states that had shootings
num_states_with_shootings <- nrow(state_data)

# (5) State with most casualties + how many
state_casualties_data <- dataset %>%
  select(state, num_killed, num_injured) %>%
  group_by(state) %>%
  summarize(state_casualties =
              sum(num_killed, na.rm = TRUE) + sum(num_injured, na.rm = TRUE))

# State with most casualties
state_most_casualties <- state_casualties_data %>%
  filter(state_casualties == max(state_casualties, na.rm = TRUE)) %>%
  pull(state)

# Number of casualties from that state
number_casualties <- state_casualties_data %>%
  filter(state_casualties == max(state_casualties, na.rm = TRUE)) %>%
  pull(state_casualties)

## AGGREGATE TABLE 
# Grouped by State:
# Number of shootings by state (# of shootings)
# Injured total (Injured)
# Killed total (Killed)
# Total casualties ()

agg_table <- dataset %>% 
  select(state, num_killed, num_injured) %>% 
  group_by(state) %>% 
  summarize(
    num_shootings = length(state),
    num_injured = sum(num_injured, na.rm = TRUE),
    num_killed = sum(num_killed, na.rm = TRUE),
    total_casualties = num_killed + num_injured, 
    killed_over_casualties = paste0(round((num_killed / total_casualties) * 100, 1), "%")
    ) %>% 
  arrange(-num_shootings)

# Average percent
avg_percent <- agg_table %>%
  mutate(percent_as_numeric = (num_killed / total_casualties) * 100) %>% 
  select(percent_as_numeric) %>% 
  summarize(avg = paste0(round(sum(percent_as_numeric) / nrow(agg_table), 1), "%")) %>% 
  pull(avg)

# Highest percent
max_percent<- agg_table %>% 
  mutate(percent_as_numeric = (num_killed / total_casualties) * 100) %>% 
  filter(percent_as_numeric == max(percent_as_numeric, na.rm = TRUE)) %>% 
  pull(killed_over_casualties)

# State with highest percent
max_state <- agg_table %>% 
  mutate(percent_as_numeric = (num_killed / total_casualties) * 100) %>% 
  filter(percent_as_numeric == max(percent_as_numeric, na.rm = TRUE)) %>% 
  pull(state)

## PARTICULAR INCIDENT
# Delaware, July 9, 2018



## FUNCTIONS
# (1) get_correct_grammar
# returns a sentence with correct grammar depending on the number of states

get_correct_grammar <- function(states) {
  temp <- strsplit(state_most_shootings, " ")
  if (length(temp) == 2) {
    result <- paste(temp[[1]], "and", paste(temp[[2]]))
  } else if (length(temp) > 2) {
    x <- length(temp) - 1
    result <- paste0(temp[[1]], ",")
    x <- length(temp) - 1
    for (i in 2:x) {
      result <- paste(result, paste0(temp[[1]], ","))
    }
    result <- paste(result, "and", temp[[x + 1]])
  } else { # length(temp) == 1
    result <- temp[[1]]
  }
  return(result)
}





