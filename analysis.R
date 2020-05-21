
library(stringr)
library(dplyr)
library(knitr)
library(kableExtra)
library(leaflet)
library(ggplot2)

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
  summarize(
    num_killed_and_injured =
      sum(num_killed, na.rm = TRUE) +
        sum(num_injured, na.rm = TRUE)
  ) %>%
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
  summarize(
    state_casualties =
      sum(num_killed, na.rm = TRUE) + sum(num_injured, na.rm = TRUE)
  )

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
    killed_over_casualties =
      paste0(round((num_killed / total_casualties) * 100, 1), "%")
  ) %>%
  arrange(-num_shootings)

# Average percent
avg_percent <- agg_table %>%
  mutate(percent_as_numeric = (num_killed / total_casualties) * 100) %>%
  select(percent_as_numeric) %>%
  summarize(avg = paste0(
    round(
      sum(percent_as_numeric) / nrow(agg_table), 1
    ),
    "%"
  )) %>%
  pull(avg)

# Highest percent
max_percent <- agg_table %>%
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

# Isolate particular case
delaware_data <- dataset %>%
  filter(state == "Delaware")

# State
case_state <- pull(delaware_data, state)

# City
case_city <- pull(delaware_data, city)

# Number injured
num_injured_case <- pull(delaware_data, num_injured)
# Number killed
num_killed_case <- pull(delaware_data, num_killed)

# Date of incident
date_case <- pull(delaware_data, date)

## INTERACTIVE MAP

interactive_map <- leaflet(data = dataset) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addCircles(
    lat = ~lat,
    lng = ~long,
    stroke = FALSE,
    radius = ~num_killed * 30000,
    color = "red",
    popup = paste("<b>City, State:</b>",
                  paste0(dataset$state, ", ", dataset$city), "<br>",
                  "<b>Date:</b>",
                  dataset$date, "<br>",
                  "<b>Number of People Injured:</b>",
                  dataset$num_injured, "<br>",
                  "<b>Number of People Killed:</b>",
                  dataset$num_killed)
    )

## PLOT
# (1) Create barplot sorting the number of shootings by months of the year
# (2) Retrieve values from the barplot for index.Rmd

# (1) Create barplot
# Modify dataset to group by month and show number of shootings by month
months_data <- dataset %>%
  mutate(month = word(date, 1)) %>%
  group_by(month) %>%
  summarize(num_shootings = length(month))

# Months are treated as factors so they can be ordered properly
months_factors_data <- months_data %>%
  mutate(month = as.factor(month))

months_factors_data$month <-
  ordered(months_factors_data$month,
          levels = c("January", "February", "March",
                     "April", "May", "June",
                     "July", "August", "September",
                     "October", "November", "December"))

# Create bar plot with months on x-axis and number of shootings on y-axis
months_barplot <- ggplot(data = months_factors_data, aes(x = month, y = num_shootings))
months_barplot <- months_barplot + theme_bw() +
  geom_bar(stat = "identity") +
  labs(title = "U.S. Shootings by Month in 2018",
       caption = "Shootings in the U.S. sorted by month.",
       x = "Month",
       y = "Number of Shootings")

# (2) Retrieve values
# month with least shootings
month_least_data <- months_data %>%
  filter(num_shootings == min(num_shootings, na.rm = TRUE))

month_least_shootings <- pull(month_least_data, month)
month_least_number <- pull(month_least_data, num_shootings)

# month with most shootings
month_most_data <- months_data %>%
  filter(num_shootings == max(num_shootings, na.rm = TRUE))

month_most_shootings <- pull(month_most_data, month)
month_most_number <- pull(month_most_data, num_shootings)

# percentage increase between the two values
perc_increase <- round(month_most_number / month_least_number, 2) * 100

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
  } else {
    result <- temp[[1]]
  }
  return(result)
}
