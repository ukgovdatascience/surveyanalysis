# import data and other up- and down-sampling models
source(file = "notebooks/causality/052_model_mr_updownsampling.R")


# Model: Multinomial Regression - final --------------------------------------------
# after finding best model and hyperparam values
# can use it to fit final model on all rows of data not orignally held out for testing
# (train and validation set combined)
# then evaluate model performance one last with with held-out test set
model_mr_last <- multinom_reg(penalty = best_mr_own$penalty, mixture = 1) %>%
  set_engine(engine = "glmnet", importance = "impurity") %>%
  set_mode(mode = "classification")

# create workflow
workflow_mr_last <- update_model(x = workflow_mr, spec = model_mr_last)

# create fit
set.seed(seed = 42)
# is this right?
fit_mr_initial <- model_mr_last %>%
  fit(data = df_train, mode_wellbeing ~ .)
fit_mr_last <- last_fit(object = workflow_mr_last, splits)


# check metrics - not as good as without up- and down-sampling
collect_metrics(x = fit_mr_last)

# check coefficients
tidy(x = fit_mr_initial)

# variable importance plot
fit_mr_last %>%
  pluck(".workflow", 1) %>%
  pull_workflow_fit() %>%
  vip()

# visualise last ROC curve
fit_mr_last %>%
  collect_predictions() %>%
  roc_curve(mode_wellbeing, .pred_1:.pred_5) %>%
  autoplot()
