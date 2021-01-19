library(readr)
library(lubridate)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

df <- read_csv(file = "data/interim/questionnaires_linked.csv")

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
  ) %>%
  # extract month for simplification
  mutate(
    measurement_month = factor(x = month(measurement_date)),
    response = factor(
      x = response,
      levels = c(NA, seq(from = 1, to = 5, by = 1)),
      ordered = TRUE
    )
  )


# suggestion i:
# I guess my go-to would be % of respondents rating 'highly likely' per data point, with time as x-axis?
# So it could be a stacked area / bar chart / line chart if you decide to include the breaks as well
df_stack <- df_wellbeing %>%
  group_by(measurement_month, response) %>%
  tally() %>%
  rename("counts" = "n") %>%
  mutate(label = paste0(round(x = 100 * counts / sum(counts), digits = 2), "%"))

ggplot(data = df_stack, mapping = aes(
  x = measurement_month,
  y = counts,
  fill = response
)) +
  geom_bar(stat = "identity") +
  geom_text(
    mapping = aes(label = label),
    colour = "#D55E00",
    position = position_stack(vjust = 0.5)
  ) +
  labs(
    title = "Bar Chart: Student responses to wellbeing",
    x = "Month",
    y = "Count"
  ) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(face = "bold", hjust = 0.5),
    panel.background = element_blank(),
    axis.line = element_line(color = "black")
  )


# suggestion ii:
# geom point + jitter them + colour them by ordinal scale (1 - 7 or whatever) + use borders
# if you need to highlight the micro-cohorts + x-axis is time
ggplot(
  data = df_wellbeing,
  mapping = aes(
    x = measurement_month,
    y = as.factor(pupil_id),
    colour = response
  )
) +
  geom_point() +
  facet_grid(. ~ question)

# suggestion iii:
# graph visualisation where nodes are responses and edges are % of responses following this path

# no. of surveys they complete
n <- 4

# no. of levels (likert)
lvls <- list(1:5)

l <- rep(x = lvls, n)

# get no. of unique permutations
combos <- expand.grid(l)
