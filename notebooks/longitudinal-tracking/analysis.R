library(readr)
library(lubridate)
library(dplyr)
library(tidyr)

df <- read_csv(file = "data/processed/questionnaires_linked.csv")

# focus on:
#   - Wellbeing: Q185
#   - Anxiety: Q206
#   - Remote-learning: Q188

# "Is it possible to track a micro-cohort of pupils across the whole period?"
# "Focusing on those respondents who have multiple responses in the period
# – there should be a good number of pupils with at least 4 or more responses over the time period"
pupils_long <- df %>%
  count(pupil_id) %>%
  filter(n > 4) %>%
  distinct(pupil_id) %>%
  pull()

# are pupils who have returned multiple surveys within the same month
# as we're informed that ideally, each student should be filling out a survey every month
# then there's something possibly dodgy about multiple returns in the same month
# so let's get these students and isolate them
pupils_return_several_in_one_month <- df %>%
  filter(pupil_id %in% pupils_long) %>%
  # check they have completed multiple returns across several months
  # as we want to avoid duplicates
  mutate(measurement_month = month(measurement_date)) %>%
  group_by(pupil_id, measurement_month) %>%
  tally() %>%
  filter(n > 1) %>%
  distinct(pupil_id) %>%
  pull()

df_wellbeing <- df %>%
  # filter for students with >= 4 returns
  # and students who did not return more than one response in same month
  filter((pupil_id %in% pupils_long) & !(pupil_id %in% pupils_return_several_in_one_month)) %>%
  # select Q to focus on
  select(pupil_id, measurement_date, starts_with(match = "185")) %>%
  # unpivot
  pivot_longer(
    cols = -c("pupil_id", "measurement_date"),
    names_to = "question",
    values_to = "response"
  )
