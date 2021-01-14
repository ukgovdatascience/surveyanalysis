library(dplyr)

# import and transform data
source("src/utils/constants.R")

# Summary Statistics ------------------------------------------------------
# have class imbalance in our dv, which can be handled with:
# - downsampling: randomly select a subset of samples from the class with more instances
#                 to match the number of samples coming from each class.
#                 could lose information by removing some samples.
# - upsampling: randomly duplicate samples from the class with fewer instances or
#               we generate additional instances based on the data that we have,
#               so as to match the number of samples in each class. While we avoid losing information
#               with this approach, we also run the risk of overfitting our model as we are more likely
#               to get the same samples in the training and in the test data.
summary(object = select(.data = df, -c(pupil_id, school_id, measurement_date)))


# Factor analysis ---------------------------------------------------------
# understand if for a collection of observed variables, there are a set of underlying variables/factors
# that explain the interrelationships among those variables
# (cannot conduct PCA as don't have numeric data, just factor)
factor_variables <- df %>%
  drop_na(mode_wellbeing, mode_wellbeing_lag1, d_key_stage, d_EAL, d_pupil_premium_eligible, d_send_marker) %>%
  select(!starts_with("cl_187_")) %>%
  select(-c(pupil_id:mode_wellbeing)) %>%
  mutate(across(.cols = everything(), .fn = as.integer))
# how many factors do we need?
fa.parallel(x = factor_variables, fm = "minres", fa = "fa")


# have missing values, which can be handled with:
# - ignore observations: deletes missing values but this risks losing information.
# - replace by average (mode) value: Doesn't impact mode but will affect variance which is less desirable.
# - mice package: imputes missing values through drawing from data distribution
#                 however, it's mostly used in case of continuous, quantitative variable.
#                 we have categorical and boolean variables.
# - knn: impute missing values using neighbourhood points.
# - predict: input missing values by using a multiclass predictor. Like interpolation
#            but won't add much extra info to mode?



# split and resample, reserve 25% for test set
# specifically, doing stratified random sample split due to class imbalance of mode_wellbeing
set.seed(seed = 42)
splits <- initial_split(data = df_nona, prop = 0.75, strata = mode_wellbeing)
df_train <- training(splits)
df_test <- testing(splits)

# check set proportions
df_train %>%
  count(mode_wellbeing) %>%
  mutate(prop = n / sum(n))
df_test %>%
  count(mode_wellbeing) %>%
  mutate(prop = n / sum(n))


# Sampling Methods --------------------------------------------------------
# are using stratified sampling here
# alternative could be to use k-fold cross-validation
# https://www.tidymodels.org/start/resampling/
# https://stats.stackexchange.com/questions/18856/is-cross-validation-a-proper-substitute-for-validation-set

# create 20% validation set from train set
df_val <- validation_split(data = df_train, prop = 0.8, strata = mode_wellbeing)
