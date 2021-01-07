library(dplyr)
library(tidyr)
library(tidymodels)
library(themis)
library(ggplot2)

source(file = "src/utils/constants.R")

# previously, found multinomial regression performed slightly better than random forest
# in modelling our data, though it still was not the best performance.
# however, we have imbalanced classes in our DV, `mode_wellbeing`, and this can impact
# performance.
# thus, will try up-sampling and down-sampling the classes in `mode_wellbeing` to
# see if this can improve our model performance.

# use same data as before for consistency
# won't deal with missing nor imbalanced data
df_nona <- df %>%
  drop_na(mode_wellbeing, mode_wellbeing_lag1, d_key_stage, d_EAL, d_pupil_premium_eligible, d_send_marker) %>%
  select(measurement_date, mode_wellbeing, mode_wellbeing_lag1, d_key_stage:d_send_marker)


# Model: Random forest - initial ----------------------------------------------------

# check for class imbalances
df_nona %>%
  count(mode_wellbeing) %>%
  mutate(prop = n / sum(n))

# split into train and test
set.seed(seed = 42)
splits <- initial_split(data = df_nona, prop = 0.75, strata = mode_wellbeing)
df_train <- training(x = splits)
df_test <- testing(x = splits)

# train and test set prop
# have similar proportion of class imbalances as whole df
df_train %>%
  count(mode_wellbeing) %>%
  mutate(prop = n / sum(n))
df_test %>%
  count(mode_wellbeing) %>%
  mutate(prop = n / sum(n))

# resampling using validations et (instead of k-fold cross-validation)
# for consistency with 03_model_naive.R
# https://stats.stackexchange.com/questions/18856/is-cross-validation-a-proper-substitute-for-validation-set
df_val <- validation_split(data = df_train, strata = mode_wellbeing, prop = 0.8)

# as using other resampling method of up/down-sampling instead of validation set
# then letting tune do parallel processing by not setting num.threads argument
model_rf <- rand_forest(mtry = tune(), min_n = tune(), trees = 1000) %>%
  set_engine(engine = "ranger") %>%
  set_mode(mode = "classification")

# create recipe
recipe_rf <- recipe(data = df_train, mode_wellbeing ~ .) %>%
  step_date(measurement_date) %>%
  step_rm(measurement_date) %>%
  step_upsample(mode_wellbeing, over_ratio = 0.5, seed = 42) %>%
  step_downsample(mode_wellbeing, under_ratio = 0.5, seed = 42)

# check that up- and down-sampling worked
df_middlesample <- prep(x = recipe_rf, training = df_train)
df_middlesample <- table(bake(object = df_middlesample, new_data = NULL)$mode_wellbeing, useNA = "always")
orig <- table(df_train$mode_wellbeing, useNA = "always")
data.frame(
  level = names(orig),
  freq_orig = as.vector(orig),
  freq_train = as.vector(df_middlesample)
)
rm(df_middlesample, orig)

# create workflow
workflow_rf <- workflow() %>%
  add_model(spec = model_rf) %>%
  add_recipe(recipe = recipe_rf)

# train and tune model
res_rf <- workflow_rf %>%
  tune_grid(df_val,
    grid = 25,
    control = control_grid(save_pred = TRUE),
    metrics = metric_set(roc_auc)
  )
# show best random forest models out of 25 candidates
show_best(x = res_rf, metric = "roc_auc")
best_rf <- select_best(x = res_rf, metric = "roc_auc")
# from plot below, want small `mtry` for optimal performance
# whereas `min_n` can take a number of values
autoplot(object = res_rf)


# Model: Random Forest - final --------------------------------------------
model_rf_last <- rand_forest(
  mtry = best_rf$mtry,
  min_n = best_rf$min_n,
  trees = 1000
) %>%
  set_engine(engine = "ranger", importance = "impurity") %>%
  set_mode(mode = "classification")
workflow_rf_last <- update_model(x = workflow_rf, spec = model_rf_last)
fit_rf_last <- last_fit(object = workflow_rf_last, splits)
# performance is not as good, possibly because random forest
# works okay for imbalanced classes?
collect_metrics(x = fit_rf_last)
