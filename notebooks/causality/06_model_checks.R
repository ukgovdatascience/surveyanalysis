library(dplyr)
library(lubridate)
library(rcompanion)


# import and transform data
source("src/utils/constants.R")

# are vip similar between lr and rf?
fit_ml_last %>%
  # pluck out first element in `.workflow`` column
  pluck(".workflow", 1) %>%
  pull_workflow_fit() %>%
  vip()

finalize_model(x = model_rf, parameters = best_rf) %>%
  set_engine(engine = "ranger", importance = "permutation") %>%
  set_mode(mode = "classification") %>%
  fit(mode_wellbeing ~ ., data = df_train) %>%
  vip()


# in vip, found variables most important to predicting mode_wellbeing
# sense-check by using Cramer V test to get association scores for IV and DVs,
# though note, don't necessarily expect them to be very high
# since the model looks at relative importance of each variable in a system/model
df_association <- df_nona %>%
  mutate(measurement_month = month(measurement_date)) %>%
  mutate(across(.cols = everything(), .fns = as.integer))
dv <- select(.data = df_association, mode_wellbeing)
iv <- select(.data = df_association, -c(measurement_date, mode_wellbeing))
associations <- list()
seq_iv <- length(iv)
seq_iv <- seq_len(length.out = seq_iv)
for (i in seq_iv) {
  associations[[i]] <- cramerV(x = pull(dv), y = iv[[i]])
}
names(associations) <- colnames(iv)
