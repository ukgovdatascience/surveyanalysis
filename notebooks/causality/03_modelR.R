library(readr)
library(dplyr)
library(tidyr)
library(tidymodels)
library(vip)
library(ggplot2)

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

# Summary Statistics ------------------------------------------------------
# have class imbalance in our dv, which can be handled with:
# - oversampling: randomly select a subset of samples from the class with more instances
#   to match the number of samples coming from each class.
# - undersampling: randomly duplicate samples from the class with fewer instances or
#                  we generate additional instances based on the data that we have,
#                  so as to match the number of samples in each class. While we avoid losing information
#                  with this approach, we also run the risk of overfitting our model as we are more likely
#                  to get the same samples in the training and in the test data.
summary(object = select(.data = df, -c(pupil_id, school_id, measurement_date)))



# have missing values, which can be handled with:
# - ignore observations: deletes missing values but this risks losing information.
# - replace by average (mode) value: Doesn't impact mode but will affect variance which is less desirable.
# - mice package: imputes missing values through drawing from data distribution
#                 however, it's mostly used in case of continuous, quantitative variable.
#                 we have categorical and boolean variables.
# - knn: impute missing values using neighbourhood points.
# - predict: input missing values by using a multiclass predictor. Like interpolation 
#            but won't add much extra info to mode?


# Model: Naive
# won't deal with missing nor imbalanced data
df_nona <- df %>%
  drop_na(mode_wellbeing, mode_wellbeing_lag1, d_key_stage, d_EAL, d_pupil_premium_eligible, d_send_marker) %>%
  select(measurement_date, mode_wellbeing, mode_wellbeing_lag1, d_key_stage:d_send_marker)

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

# create 20% validation set from train set
df_val <- validation_split(data = df_train, prop = 0.8, strata = mode_wellbeing)


# Model: Penalised Logistic Regression ----------------------------------
# set mixture to 1 means glmnet model removes irrelevant predictors
# and choose simpler model
model_lr <- multinom_reg(penalty = tune(), mixture = 1) %>% 
  set_engine(engine = 'glmnet')
# create recipe for preprocessing data to prep for model
# and convert all categorical variables to dummy variables
recipe_lr <- recipe(data = df_train, mode_wellbeing ~ .) %>% 
  step_date(measurement_date) %>% 
  step_rm(measurement_date) %>% 
  step_dummy(all_nominal(), -all_outcomes()) %>% 
  # remove iv that contain a single unique value since predictors should be centred and scaled
  step_zv(all_predictors())


# bundle model and recipe into single workflow
workflow_lr <- workflow() %>% 
  add_model(model_lr) %>% 
  add_recipe(recipe_lr)

# create grid of penalty values for tuning
# as only have one hyperparam to tune here, 
# can set grid manually using a one-column tibble with 30 candidate values
reg_grid_lr <- tibble(penalty = 10^seq(-4, -1, length.out = 30))
# lowest penalty values
reg_grid_lr %>% top_n(-5)
# highest penalty values
reg_grid_lr %>% top_n(5)

# train and tune model
# using tune::tune_grid() to train these 30 penalised log-reg models
# and save validation set prediction (via control_grid()) so diagnostic info
# is available after model fit
res_lr <- workflow_lr %>% 
  tune_grid(df_val,
            grid = reg_grid_lr,
            control = control_grid(save_pred = TRUE),
            metrics = metric_set(roc_auc))

# visualise validation set metrics by plotting area under ROC 
# against range of penalty values
plot_lr <- res_lr %>% 
  collect_metrics() %>% 
  ggplot(mapping = aes(x = penalty, y = mean)) +
  geom_point() +
  geom_line() +
  ylab("Area under the ROC curvee") +
  scale_x_log10(labels = scales::label_number())
plot_lr
# can see model performance improves at smaller penalty values,
# suggesting majority of predictors are important to model.
# also see steep drop in area under the ROC curve towards higher penalty values;
# this happens because a large enough penalty will remove all predictors from the model,
# so predictive accuracy plummets with no predictors in the model
# (note, ROC AUC value of 0.5 means model does no better than chance at predicting correct class)

# model performance plateaus at smaller penalty values,
# so going by `roc_auc` metric alone could lead to multiple options for the 'best' hyperparam value
res_lr %>% 
  show_best(metric = 'roc_auc', n = 15) %>% 
  arrange(penalty)
# as every candidate model in above tibble likely includes more predictor variables
# as the model in row below in, we take the lowest row to ensure we get the best model
# with fewest predictors
select_best(x = res_lr, metric = 'roc_auc')
plot_lr + 
  geom_vline(xintercept = select_best(x = res_lr, metric = 'roc_auc')$penalty,
             colour = 'red')
# however, may want to choose a higher penalty with a lower ROC area value
# as generally want a more parsimonious model,
# thus may prefer row 12 which gives the same mean but has less predictors,
# so select and visualise it
best_lr <- res_lr %>% 
  collect_metrics() %>% 
  arrange(penalty) %>% 
  slice(12)
best_lr
# plot
auc_lr <- res_lr %>% 
  collect_predictions(parameters = best_lr) %>% 
  roc_curve(mode_wellbeing, .pred_1:.pred_5) %>% 
  mutate(model = "Multinomial Regression")
autoplot(object = auc_lr)
# we can see that our multinomial regression model does okay,
# it certainly is better than random as the curve does not lie on the diagonal,
# but we could do better by having a larger area under the curve/be closer to top-left.
# could be because we had class imbalances as indicated by larger areas for `5`.



# Model: Tree-based ensemble ----------------------------------------------


