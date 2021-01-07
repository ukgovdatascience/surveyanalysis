# import and transform data
source("notebooks/causality/042_model_rf.R")

# Model Fit: Using best model ---------------------------------------------
# after finding best model and hyperparam values
# can use it to fit final model on all rows of data not orignally held out for testing
# (train and validation set combined)
# then evaluate model performance one last with with held-out test set

# build best model, multinomial regression, with optimal hyperparam found earlier,
# and set importance = 'impurity' to provide variable-importance scores in this model,
# thereby gain insight into IV that drive model performance
model_mr_last <- multinom_reg(penalty = best_lr$penalty, mixture = 1) %>%
  set_engine(engine = "glmnet", importance = "impurity") %>%
  set_mode(mode = "classification")

workflow_mr_last <- workflow_lr %>%
  update_model(model_mr_last)

set.seed(seed = 42)
fit_ml_last <- workflow_mr_last %>%
  last_fit(splits)
# check accuracy and roc_auc
collect_metrics(x = fit_ml_last)

fit_ml_last %>%
  # pluck out first element in `.workflow`` column
  pluck(".workflow", 1) %>%
  pull_workflow_fit() %>%
  vip()
# most important predictors/IV for wellbeing DV are:
# i. mode_wellbeing_lag1_1 - first lagged wellbeing value when it is '1'
# ii. mode_wellbeing_lag1_2 - first lagged wellbeing value when it is '2'
# iii. measurement_date_month_May - when the student was surveyed in May, big COVID event
# iv. measurement_date_month_Jun - when student was surveyed in June, maybe big exam event

# generate last ROC curve to visualise.
fit_ml_last %>%
  collect_predictions() %>%
  roc_curve(mode_wellbeing, .pred_1:.pred_5) %>%
  autoplot()
# this looks similar to before where the shapes are the same,
# thus suggesting our validation and test set performance statistics are similar
