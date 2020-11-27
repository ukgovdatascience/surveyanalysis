library(googledrive)
library(readr)
library(readxl)
library(httr)
library(dplyr)

# from Duncan - function for importing
read_responses <- function(file) {
  read_csv(file, col_types = cols(.default = col_character())) %>%
    pivot_longer(-c(pupil_id, pupil_impacted_id, measurement_date),
      names_to = "question", values_to = "response"
    ) %>%
    # Deal with duplicated questions.
    # Three pupils in questionnaire 186 have a set of columns to themselves, so
    # we merge them back in by:
    #
    # 1. filtering out everyone's blank responses.  That means that most pupils
    #    only have responses to the first instance of each question, and the
    #    three affected pupils only have responses to the second instance of
    #    each question.
    # 2. dropping the suffix that readr automatically adds to the second
    #   instance of each question.  Now all pupils are the same.
    #
    # instance of each column.
    filter(!is.na(response)) %>%
    mutate(question = str_remove(question, "_\\d+$"))
}


# get list of all files
file_main <- drive_get(path = as_id(x = "https://drive.google.com/file/d/1PS9xQIP_O048rGb-uvwPonyrV_3CCYc4/view?usp=sharing"))
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
    .data = sheet_questions,
    measure = `...1`,
    questionnaire = `...2`,
    question = `...3`
  ) %>%
  fill(measure, .direction = "down")
sheet_questionnaires <- select(.data = sheet_questionnaires, pupil_id:dSEND)

# import each csv from gdrive
list_df <- list()
j <- 1

for (i in files$drive_resource) {
  link <- i$webContentLink
  df <- read_responses(file = link)
  # store in list
  list_df[[j]] <- df
  j <- j + 1
}

responses <- map_dfr(.x = list_df, .f = rbind) %>%
  mutate(measurement_date = parse_date(x = measurement_date, format = "%d/%m/%Y"))

# clear environment
unlink(tf)
rm(df, file_main, files, i, j, link, list_df, tf)
