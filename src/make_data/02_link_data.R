source("src/make_data/01_link_data.R")

library(dplyr)
library(tidyr)



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
  filter(pupil_id == "100165", measurement_date == "2020-09-09", qq == "207_10 - I have felt like I have missed important school work")

# partition these duplicate responses with a row number; allocation of this is random
# this seems the best we can do
responses <- responses %>%
  select(pupil_id:measurement_date, qq, response) %>%
  group_by(pupil_id, pupil_impacted_id, measurement_date, qq) %>%
  mutate(rank = row_number()) %>%
  arrange(pupil_id, pupil_impacted_id, measurement_date, qq)

# isolate unique records
responses_dedupe <- filter(.data = responses, rank == 1)

# pivot wider for ImpactEd's purposes
df_output <- responses_dedupe %>%
  pivot_wider(
    id_cols = c(pupil_id, pupil_impacted_id, measurement_date),
    names_from = "qq",
    values_from = "response"
  ) %>%
  arrange(pupil_id, measurement_date)
write_csv(x = df_output, file = "data/processed/questionnaires_linked.csv")
