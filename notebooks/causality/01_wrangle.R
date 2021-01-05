library(readr)
library(dplyr)
library(stringr)

source("src/utils/get_mode.R")

# ensure you run `direnv allow` in terminal
readRenviron(path = ".env")
dir_interim <- Sys.getenv(x = "DIR_DATA_INTERIM")
dir_processed <- Sys.getenv(x = "DIR_DATA_PROCESSED")
df_names <- c("df_responses", "df_context", "df_questions")
df_files <- c("questionnaires_linked.csv", "context.csv", "questions.csv")


# Load data ---------------------------------------------------------------
# load data into named list
list_dfs <- list()
for (i in seq_len(length.out = length(x = df_files))) {
  list_dfs[[i]] <- read_csv(file = paste0(dir_interim, "/", df_files[i]))
}
names(list_dfs) <- df_names

rm(df_names, df_files, i)

# Is there any way of understanding which factors
# (contextual survey responses or demographics)
# are most predictive of the wellbeing or learning scores


# Compute dependent variable ----------------------------------------------
# what are our wellbeing and learning variables?
wellbeing_learn <- list_dfs[["df_questions"]] %>%
  filter(str_detect(string = measure, pattern = "(?i)wellbeing|learn"))
wellbeing_learn %>% distinct(measure)
filter_dv <- wellbeing_learn %>%
  distinct(questionnaire) %>%
  pull()

# get single outcome variable for our dependent variables of wellbeing and learning
# to make modelling easier
# do this by using mode since are dealing with categorical variables

# 187 and 188 are logical, TRUE/FALSE

# likert scale so 5
n_lvls <- 5
factor_lvls <- c(seq_len(length.out = n_lvls), NA)

df_dv <- list_dfs[["df_responses"]] %>%
  select(contains(match = c("pupil_id", "measurement_date", filter_dv))) %>%
  # convert to ordered factors
  mutate(across(.cols = starts_with(match = "185"), .fns = ~ factor(x = .x, levels = factor_lvls, ordered = TRUE))) %>%
  rowwise() %>%
  # compute mode for each record
  mutate(
    mode_wellbeing = get_mode(x = c_across(cols = starts_with(match = "185"))),
    mode_remotelearn = get_mode(x = c_across(cols = starts_with(match = "188")))
  ) %>%
  # ignore ImpactEd Covid-learning index - 207
  select(pupil_id, measurement_date, starts_with(match = "187"), starts_with(match = "mode"))

# how many dvs do we have? wellbeing is most completed;
# this might be because context learning access and remote learn are mixed data types (not just likert scale)
# so harder to get overall measure for it
nrow(filter(.data = df_dv, !is.na(x = mode_wellbeing)))
nrow(filter(.data = df_dv, !is.na(x = mode_remotelearn)))

# each person and date is unique
nrow(x = distinct(.data = df_dv, pupil_id, measurement_date)) == nrow(x = df_dv)


rm(wellbeing_learn, factor_lvls, filter_dv, n_lvls)

# Include independent variables -------------------------------------------
# contextual iv
df_iv <- list_dfs[["df_context"]] %>%
  select(pupil_id, school_id, EAL, key_stage:send_marker, d_female = dFemale) %>%
  # change datatype for modelling
  mutate(across(.cols = c(pupil_id, school_id), .fns = as.integer),
    d_key_stage = as.factor(key_stage),
    across(.cols = c(EAL, pupil_premium_eligible, send_marker), .fns = as.logical, .names = "d_{.col}")
  )

# get summary stats
summary(object = select(.data = df_dv, starts_with(match = "mode")))
summary(object = select(.data = df_iv, starts_with(match = "d")))

# export
write_csv(x = df_iv, file = paste0(dir_processed, "/df_iv.csv"))
write_csv(x = df_dv, file = paste0(dir_processed, "/df_dv.csv"))
