source("src/make_data/01_link_data.R")

library(dplyr)
library(tidyr)
library(purrr)


# join so in format requested
df_step <- list_df[[1]]
for (i in 1:(length(list_df) - 1)) {
  df_step <- df_step %>%
    full_join(y = list_df[[i + 1]], by = c("pupil_id", "pupil_impacted_id", "measurement_date"))
}

# we have repeated columns that exist in the source csv files
# however, there are three valid responses in the repeated columns that are not in the
# non-repeated columns
df <- df_step %>%
  select(!ends_with(match = "_1")) %>%
  rename_at(.vars = vars(sheet_questions$question), .funs = list(sheet_questions$questionnaire))
df <- rename_at(.data)
