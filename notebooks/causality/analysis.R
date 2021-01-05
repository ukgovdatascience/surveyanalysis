library(readr)
library(dplyr)
library(stringr)

source("src/utils/get_mode.R")

# ensure you run `direnv allow` in terminal
readRenviron(path = ".env")
dir_processed <- Sys.getenv(x = "DIR_DATA_PROCESSED")
df_names <- c("df_responses", "df_context", "df_questions")
df_files <- c("questionnaires_linked.csv", "context.csv", "questions.csv")

# load data into named list
list_dfs <- list()
for (i in seq_len(length.out = length(x = df_files))) {
  list_dfs[[i]] <- read_csv(file = paste0(dir_processed, "/", df_files[i]))
}
names(list_dfs) <- df_names

rm(df_names, df_files, i)

# Is there any way of understanding which factors
# (contextual survey responses or demographics)
# are most predictive of the wellbeing or learning scores

# what are our wellbeing and learning variables?
filter_dv <- list_dfs[["df_questions"]] %>%
  filter(str_detect(string = measure, pattern = "(?i)wellbeing|learn")) %>%
  distinct(questionnaire) %>%
  pull()

# get single outcome variable for our dependent variables of wellbeing and learning
# to make modelling easier
# do this by using mode since are dealing with categorical variables

# likert scale so 5
n_lvls <- 5
factor_lvls <- c(seq_len(length.out = n_lvls), NA)

df_dv <- list_dfs[["df_responses"]] %>%
  select(contains(match = c("pupil_id", "measurement_date", filter_dv))) %>%
  # convert to factors
  mutate(across(.cols = contains(match = filter_dv), .fns = ~ factor(x = .x, levels = factor_lvls, ordered = TRUE))) %>%
  rowwise() %>%
  # compute mode for each record
  mutate(
    mode_wellbeing = get_mode(x = c_across(cols = starts_with(match = "185"))),
    mode_contextlearningaccess = get_mode(x = c_across(cols = starts_with(match = "187"))),
    mode_remotelearn = get_mode(x = c_across(cols = starts_with(match = "188")))
  )
