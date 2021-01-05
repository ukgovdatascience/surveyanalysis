library(googledrive)
library(readxl)
library(httr)
library(dplyr)
library(purrr)
library(tidyr)

source("src/utils/read_responses.R")

drive_path <- "https://drive.google.com/file/d/1PS9xQIP_O048rGb-uvwPonyrV_3CCYc4/view?usp=sharing"

# Data Load ---------------------------------------------------------------
# authorise access to gdrive
drive_auth()

# get list of all files
file_main <- drive_get(path = as_id(x = drive_path))
files <- drive_ls(
  path = as_id(x = "https://drive.google.com/drive/u/0/folders/1sfavbXr3UAqfd_zWAuDChvC7Hnb69gi5"),
  type = "csv"
)

# store Excel file temporarily
GET(
  url = file_main$drive_resource[[1]]$webContentLink,
  write_disk(tf <- tempfile())
)

# import Excel sheet
sheet_questions <- read_excel(
  path = tf,
  sheet = "List of Questions",
  col_names = FALSE
)
sheet_questionnaires <- read_excel(
  path = tf,
  sheet = "Questionnaires",
  skip = 1,
  col_names = TRUE
)

# rename columns
sheet_questions <- sheet_questions %>%
  rename(
    measure = `...1`,
    questionnaire = `...2`,
    question = `...3`
  ) %>%
  fill(measure, .direction = "down")
sheet_questionnaires <- select(.data = sheet_questionnaires, pupil_id:dSEND)



# Question Responses ------------------------------------------------------
# import each csv from gdrive
list_df <- list()
j <- 1

for (i in files$drive_resource) {
  link <- i$webContentLink
  # extract question numbers
  file_name <- i$name
  file_name <- unlist(x = str_match_all(string = file_name, pattern = "[0-9]+"))
  df <- read_responses(file = link)
  df <- mutate(.data = df, questionnaire_number = as.character(x = file_name))
  # store in list
  list_df[[j]] <- df
  j <- j + 1
}

responses <- map_dfr(.x = list_df, .f = rbind) %>%
  mutate(
    measurement_date = parse_date(x = measurement_date, format = "%d/%m/%Y"),
    questionnaire_number = as.integer(x = questionnaire_number)
  )

# clear environment
unlink(tf)
rm(df, file_main, files, i, j, link, list_df, tf)


# get questionnaire info in to use as column names later
responses <- responses %>%
  left_join(y = sheet_questions, by = "question") %>%
  mutate(qq = paste0(questionnaire, " - ", question)) %>%
  # remove duplicates
  distinct()


# see if have unique combo of rows so can pivot_wider safely
responses %>%
  group_by(pupil_id, pupil_impacted_id, measurement_date, qq) %>%
  summarise(count = n()) %>%
  filter(count > 1)
# have duplicates, here's an example
responses %>%
  filter(
    pupil_id == "100165",
    measurement_date == "2020-09-09",
    qq == "207_10 - I have felt like I have missed important school work"
  )

# partition these duplicate responses with a row number; allocation of this is random
# this seems the best we can do
responses_dedupe <- responses %>%
  # drop rows with NAs
  drop_na(questionnaire) %>%
  select(pupil_id:measurement_date, qq, response) %>%
  group_by(pupil_id, measurement_date, qq) %>%
  mutate(rank = row_number()) %>%
  arrange(pupil_id, pupil_impacted_id, measurement_date, qq)

# isolate unique records
responses_dedupe <- filter(.data = responses_dedupe, rank == 1)

# pivot wider for ImpactEd's purposes
df_output <- responses_dedupe %>%
  pivot_wider(
    id_cols = c(pupil_id, pupil_impacted_id, measurement_date),
    names_from = "qq",
    values_from = "response"
  ) %>%
  arrange(pupil_id, measurement_date)
write_csv(x = df_output, file = "data/processed/questionnaires_linked.csv")



# Questionnaires ----------------------------------------------------------
# drop duplicates
col_unique <- colnames(x = select(.data = sheet_questionnaires, pupil_id, school_id:dSEND))
contextual_responses <- sheet_questionnaires %>%
  distinct(across(.cols = all_of(col_unique)))
# check if have unique records - yes
nrow(x = contextual_responses) == nrow(x = distinct(.data = contextual_responses, pupil_id))
write_csv(x = sheet_questionnaires, file = "data/processed/context.csv")
write_csv(x = sheet_questions, file = "data/processed/questions.csv")
