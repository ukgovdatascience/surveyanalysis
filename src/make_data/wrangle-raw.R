# Wrangle the raw data provided on 2020-11-26

library(tidyverse)
library(readxl)
library(fs)
library(here)

# It turns out that each pupil only has a unique set of demographics, which have
# been joined to each of their questionnaire responses.  So we only need a
# single row of demographics, and no date, and then can join (nicely) to their
# questionnaire responses by pupil_id alone.

demographics <-
  read_excel("./data/raw/Skills Measure Main November (Anon) - scores and pupil demographics only.xlsx",
             sheet = "Questionnaires",
             range = "A2:R374774",
  col_types = c("text",    # pupil_id
                "text",    # pupil_impacted_id
                "skip",    # questionnaire_id
                "skip",    # score
                "skip",    # created_at, mixture of date formats
                "skip",    # count (nth response from pupil)
                "text",    # school_id
                "logical", # eal
                "text",    # gender
                "numeric", # key_stage
                "logical", # pupil_premium
                "logical", # send_marker
                "numeric", # year_group
                "logical", # dEAL
                "logical", # dFEMALE
                "logical", # dLAC
                "logical", # dPPP
                "logical"  # dSEND
                )) %>%
  distinct()

responses_files <- dir_ls(here("data", "raw"), glob = "*.csv")

read_responses <- function(file) {
  cat(file, "\n")
  read_csv(file, col_types = cols(.default = col_character())) %>%
    pivot_longer(-c(pupil_id, pupil_impacted_id, measurement_date),
                 names_to = "question", values_to = "response") %>%
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
    mutate(question = str_remove(question, "_\\d+$")) %>%
    mutate(questionnaire_id = str_extract(file, "\\d+"))
}
responses <-
  map_dfr(responses_files, read_responses) %>%
  mutate(measurement_date = parse_date(measurement_date, "%d/%m/%Y"))

# How many responses to each question in each questionnaire
responses %>%
  count(questionnaire_id, question) %>%
  pivot_wider(id_cols = question, names_from = questionnaire_id, values_from = n) %>%
  print(n = Inf)

# No questions appear in more than one questionnaire
responses %>%
  distinct(questionnaire_id, question) %>%
  count(question) %>%
  arrange(n, question) %>%
  print(n = Inf)

# Which questinnaires were responded to when?
responses %>%
  distinct(questionnaire_id, measurement_date) %>%
  ggplot(aes(measurement_date, questionnaire_id)) +
  geom_point() +
  labs(title = "Date of responses to each questionnaire")
ggsave(here("outputs", "date-of-responses-to-each-questionnaire.png"))

# Is pupil_id:impacted_id 1:1?  Yes.
ids <- responses %>% distinct(pupil_id, pupil_impacted_id)
ids %>% count(pupil_id, sort = TRUE)
ids %>% count(pupil_impacted_id, sort = TRUE)

# Join pupils to demographics by pupil_id.  23 pupils don't have demographics.
anti_join(demographics, ids, by = "pupil_id")
anti_join(ids, demographics, by = "pupil_id")

full_data <- left_join(responses, demographics, by = "pupil_id")

saveRDS(full_data, here("data", "processed", "full_data.Rds"))
