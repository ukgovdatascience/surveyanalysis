library(googledrive)
library(readr)
library(readxl)
library(httr)
library(dplyr)

# get list of all files
file_main <- drive_get(path = as_id(x = "https://drive.google.com/file/d/1PS9xQIP_O048rGb-uvwPonyrV_3CCYc4/view?usp=sharing"))
files <- drive_ls(
  path = as_id(x = "https://drive.google.com/drive/u/0/folders/1sfavbXr3UAqfd_zWAuDChvC7Hnb69gi5"),
  type = "csv"
)

# store Excel file temporarily and import
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
sheet_questions <- rename(
  .data = sheet_questions,
  measure = `...1`,
  questionnaire = `...2`,
  question = `...3`
)
sheet_questionnaires <- select(.data = sheet_questionnaires, pupil_id:dSEND)

# import each csv from gdrive
list_df <- list()
j <- 1

for (i in files$drive_resource) {
  link <- i$webContentLink
  df <- read_csv(file = link)
  # store in list
  list_df[[j]] <- df
  j <- j + 1
}

# name list for easy access of each df
names(list_df) <- files$name
