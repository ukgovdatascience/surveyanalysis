library(readr)
library(dplyr)
library(tidyr)
library(stringr)


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
