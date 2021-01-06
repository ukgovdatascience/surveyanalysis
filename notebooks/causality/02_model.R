library(readr)
library(dplyr)

# ensure you run `direnv allow` in terminal
readRenviron(path = ".env")
dir_processed <- Sys.getenv(x = "DIR_DATA_PROCESSED")

df_iv <- read_csv(file = paste0(dir_processed, "/df_iv.csv"))
df_dv <- read_csv(file = paste0(dir_processed, "/df_dv.csv"))


# Model Reformat ----------------------------------------------------------
# compute lag for IV - a student's current feelings can depend on their previous feeling
df <- df_dv %>%
  group_by(pupil_id) %>%
  # example of pupil_id == 221 having lags
  arrange(pupil_id, measurement_date) %>%
  mutate(mode_wellbeing_lag1 = lag(x = mode_wellbeing, n = 1))

# remove duplicates
df_iv <- distinct(.data = df_iv)

# check unique combo of pupil id
nrow(x = distinct(.data = df_dv)) == nrow(x = df_dv)
nrow(x = distinct(.data = df_iv, pupil_id, school_id)) == nrow(x = df_iv)

# bring together
df <- df %>%
  left_join(y = df_iv, by = "pupil_id") %>%
  # order so IV on RHS and DV on LHS
  select(
    pupil_id,
    school_id,
    measurement_date,
    mode_wellbeing,
    mode_wellbeing_lag1,
    starts_with(match = "187"),
    starts_with(match = "d_")
  )
