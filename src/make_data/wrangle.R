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

questionnaire <-
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
  behead("left", "pupil_impacted_id") %>%
  behead("left", "questionnaire_definition_id") %>%
  behead("left", "score") %>%  # "Score = Average response score given for that questionnaire", i.e. we don't need it
  behead("left", "Created") %>%
  behead("left", "created_at") %>%
  behead("left", "Count") %>% # 'The "count" column is telling us which attempt for the student that is (chronologically). So the first attempt will be 1, the second 2 and so on', i.e. we don't need it.
  mutate(created_at = if_else(is.na(created_at), Created, created_at)) %>%
  select(pupil_id,
         pupil_impacted_id,
         questionnaire_definition_id,
         created_at,
         question_id,
         character,
         numeric) %>%
  left_join(questions, by = "question_id")

# Some pupils have submitted multiple reponses on the same date (usually when the
# time hasn't been recorded).  We choose to drop all the responses of these
# pupils.
pupil_ids_with_multiple_responses <-
  questionnaire %>%
  count(pupil_id, created_at, question_id) %>%
  filter(n > 1) %>%
  distinct(pupil_id)

questionnaire <-
  anti_join(questionnaire,
            pupil_ids_with_multiple_responses,
            by = "pupil_id")

saveRDS(questionnaire, here("data", "processed", "questionnaire.Rds"))

# Answers by each pupil to each the survey with ID 184. ------------------------

# These answers are missing from the Questionnaire sheet, though the set of
# pupils is almost the same (we think the difference is 10).

# Unfortunately, this sheet doesn't have any datetimes, only dates.

sheet_184 <-
  read_excel(file_path, sheet = "184") %>%
  rename(created_at = date) %>%
  select(-measurement_date) %>%
  pivot_longer(-c(pupil_id, pupil_impacted_id, created_at),
               names_to = "question",
               values_to = "numeric") %>%
  mutate(questionnaire_definition_id = 184, character = NA_character_) %>%
  left_join(questions, by = "question")

# Some pupils have submitted multiple reponses on the same date (usually when the
# time hasn't been recorded).  We choose to drop all the responses of these
# pupils.
pupil_ids_with_multiple_responses_184 <-
  sheet_184 %>%
  count(pupil_id, created_at, question_id) %>%
  filter(n > 1) %>%
  distinct(pupil_id)

sheet_184 <-
  anti_join(sheet_184,
            pupil_ids_with_multiple_responses_184,
            by = "pupil_id")

saveRDS(sheet_184, here("data", "processed", "questionnaire-184-responses.Rds"))

# Function to extract responses like "Tasks created by my school (for example,
# worksheets or suggested learning activities), Videos recorded by my school,
# Other online learning tools (for example, Hegarty Maths, Numbots, Seneca
# Learning, Century TECH)" into separate strings.
separate_multiple_choice_answers <- function(x) {
  out <-
    str_replace_all(
      x,
      c(
        "\\), " = ");",
        "above, " = "above;",
        "advance, " = "advance;",
        "back, " = "back;",
        "coronavirus, " = "coronavirus;",
        "have, " = "have;",
        "learning, " = "learning;",
        "like, " = "like;",
        "possible, " = "possible;",
        "resources, " = "resources;",
        "school, " = "school;",
        "while, " = "while;",
        "work, " = "work;"
      )
    )
  map(out, ~ str_split(.x, ";")[[1]])
}
# separate_multiple_choice_answers("Tasks created by my school (for example, worksheets or suggested learning activities), Videos recorded by my school, Other online learning tools (for example, Hegarty Maths, Numbots, Seneca Learning, Century TECH)")
# [[1]]
# [1] "Tasks created by my school (for example, worksheets or suggested learning activities)"
# [2] "Videos recorded by my school"
# [3] "Other online learning tools (for example, Hegarty Maths, Numbots, Seneca Learning, Century TECH)"

all_responses <-
  bind_rows(questionnaire, sheet_184) %>%
  arrange(questionnaire_definition_id, question_id, pupil_id, created_at) %>%
  select(questionnaire_definition_id,
         question_id,
         pupil_id,
         pupil_impacted_id,
         created_at,
         question_category,
         question,
         numeric,
         character) %>%
  # Replace empty character responses with NA
  mutate(character = if_else(character == "", NA_character_, character)) %>%
  # Drop rows with no answers
  filter(!(is.na(numeric) & (is.na(character)))) %>%
  # Convert t/f values to TRUE/FALSE
  mutate(logical = case_when(character == "t" ~ TRUE,
                             character == "f" ~ FALSE,
                             TRUE ~ NA),
         character = if_else(is.na(logical), character, NA_character_)) %>%
  # Extract multi-value character responses
  mutate(character =
    if_else(question_id %in% c("212_2", "212_3", "212_4"),
            separate_multiple_choice_answers(character),
            as.list(character)))

saveRDS(all_responses, here("data", "processed", "all-responses.Rds"))
