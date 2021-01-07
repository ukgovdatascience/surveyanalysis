library(tidyr)
library(tidymodels)
library(themis)
library(ggplot2)
library(scales)

source(file = "notebooks/causality/03_traintest.R")

# previously, found multinomial regression performed slightly better than random forest
# in modelling our data, though it still was not the best performance.
# however, we have imbalanced classes in our DV, `mode_wellbeing`, and this can impact
# performance.
# thus, will try up-sampling and down-sampling the classes in `mode_wellbeing` to
# see if this can improve our model performance.


# Model: Penalised multinomial regression ---------------------------------
# specify model
model_mr <- multinom_reg(penalty = tune(), mixture = 1) %>%
  set_engine(engine = "glmnet")

# create recipe
recipe_mr <- recipe(data = df_train, mode_wellbeing ~ .) %>%
  step_date(measurement_date) %>%
  step_rm(measurement_date) %>%
  step_dummy(all_nominal(), -all_outcomes()) %>%
  # remove iv that contain a single unique value since predictors should be centred and scaled
  step_zv(all_predictors()) %>%
  # up-and down-sample
  step_upsample(mode_wellbeing, over_ratio = 0.5, seed = 42) %>%
  step_downsample(mode_wellbeing, under_ratio = 0.5, seed = 42)

# create workflow
workflow_mr <- workflow() %>%
  add_model(spec = model_mr) %>%
  add_recipe(recipe = recipe_mr)

# create grid for tuning
grid_reg_mr <- tibble(penalty = 10^seq(from = -4, to = -1, length.out = 30))

# train and tune model
res_mr <- tune_grid(
  object = workflow_mr,
  df_val,
  control = control_grid(save_pred = TRUE),
  metrics = metric_set(roc_auc)
)

# show best models
best_15_mr <- show_best(x = res_mr, metric = "roc_auc", n = 15)
best_mr <- select_best(x = res_mr, metric = "roc_auc")$penalty
best_mr_own <- res_mr %>%
  show_best(metric = "roc_auc", n = 15) %>%
  # self-picked from plot
  filter(.config == "Preprocessor1_Model09")

# plot best model
# visualise validation set-metrics against range of penalty values
res_mr %>%
  collect_metrics() %>%
  ggplot(mapping = aes(x = penalty, y = mean)) +
  geom_point() +
  geom_line() +
  geom_vline(
    xintercept = c(best_mr$penalty, best_mr_own$penalty),
    colour = c("red", "purple")
  ) +
  ylab("Area under the ROC curve") +
  scale_x_log10(labels = label_number())

# visualise validation set ROC curve
auc_mr <- res_mr %>%
  collect_predictions(parameters = penalty_own) %>%
  roc_curve(mode_wellbeing, .pred_1:.pred_5) %>%
  mutate(model = "Multinomial Regression")
autoplot(auc_mr)
