# Read parts of the spreadsheet into R and store on disk as .Rds

library(tidyverse)
library(readxl)
library(tidyxl)
library(unpivotr)
library(here)

file_path <- here("data", "raw", "Skills Measure Main August no PII For UCL.xlsx")

# Lookup table of questions, question IDs, and question categories -------------

questions <-
  read_excel(file_path,
             sheet = "List of Questions",
             col_names = c("question_category", "question_id", "question"),
             range = "A1:C69") %>%
  fill(question_category)

saveRDS(questions, here("data", "processed", "questions.Rds"))

# Answers by each pupil to each questions, except the survey with ID 184. ------

cells <- xlsx_cells(file_path, sheet = "Questionnaire")

tidy <-
  cells %>%
  filter(!is_blank) %>%
  select(address, row, col, data_type, date, character, numeric) %>%
  # Fix the dates in column 6 (F)
  # * Parse the dates stored as characters.
  # * Replace the dates stored as dates with NA, because they're corrupt,
  #   apparently in mm/dd/yyyy format.  These will be replaced by the `Created`
  #   column.  This seems to have happened to midnight values only.
  # Ignore the warnings, which are because `parse_datetime()` is evaluated on
  # every `character` value, not just the relevant ones.
  mutate(
    date = if_else(
      col == 6,
      parse_datetime(character, format =  "%d/%m/%Y %H:%M"),
      date
    ),
    data_type = if_else(col == 6, "date", data_type)
  ) %>%
  # Interpret as an ordinary table
  behead("up", "question_id") %>%
  behead("left", "pupil_id") %>%
  behead("left", "pupil_impact_id") %>%
  behead("left", "questionnaire_definition_id") %>%
  behead("left", "score") %>%  # "Score = Average response score given for that questionnaire", i.e. we don't need it
  behead("left", "Created") %>%
  behead("left", "created_at") %>%
  behead("left", "Count") %>%
  mutate(created_at = if_else(is.na(created_at), Created, created_at)) %>%
  select(address, row, col, pupil_id, pupil_impact_id, questionnaire_definition_id, created_at, character, numeric)

saveRDS(tidy, here("data", "processed", "questionnaire.Rds"))
