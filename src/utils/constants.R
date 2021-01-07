library(readr)
library(dplyr)

# ensure you run `direnv allow` in terminal
readRenviron(path = ".env")
dir_processed <- Sys.getenv(x = "DIR_DATA_PROCESSED")

df <- read_csv(file = paste0(dir_processed, "/df_model.csv"))

# likert scale so 5
n_lvls <- 5
wellbeing_lvls <- c(seq_len(length.out = n_lvls))
key_stage_lvls <- df %>%
  distinct(d_key_stage) %>%
  arrange(d_key_stage) %>%
  pull()
df <- df %>%
  mutate(across(
    .cols = starts_with(match = "mode_wellbeing"),
    .fns = ~ factor(
      x = .,
      levels = wellbeing_lvls,
      ordered = TRUE
    )
  ),
  d_key_stage = factor(x = d_key_stage, levels = key_stage_lvls, ordered = TRUE)
  )

# won't deal with missing nor imbalanced data
df_nona <- df %>%
  drop_na(mode_wellbeing, mode_wellbeing_lag1, d_key_stage, d_EAL, d_pupil_premium_eligible, d_send_marker) %>%
  select(measurement_date, mode_wellbeing, mode_wellbeing_lag1, d_key_stage:d_send_marker)
