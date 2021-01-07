# import and transform data
source("notebooks/causality/041_model_mr.R")

# Model: Tree-based ensemble ----------------------------------------------
# tree-based models require very little preprocessing
# and can effectively handle many types of predictors
# (sparse, skewed, continuous, categorical)
n_cores <- parallel::detectCores() - 1

# if use other resampling method, let tune do parallel processing for you
# so set num.threads = tune() - one things
# using tune() as placeholder for `mtry` and `min_n` argument values,
# as these are our two hyperparam that we will tune
model_rf <- rand_forest(
  mtry = tune(),
  min_n = tune(),
  trees = 1000
) %>%
  set_engine(engine = "ranger", num.threads = n_cores) %>%
  set_mode(mode = "classification")

# create recipe and workflow
# unlike ppenalised logistic regression models,
# random forest models don't require dummy or normalised IV,
# but want to do feature-engineering on measurement_date
recipe_rf <- recipe(data = df_train, mode_wellbeing ~ .) %>%
  step_date(measurement_date) %>%
  step_rm(measurement_date)

workflow_rf <- workflow() %>%
  add_model(model_rf) %>%
  add_recipe(recipe_rf)

# train and tune model
# when set-up model, specified two hyperparam:
# - mtry: # of IV each node in decision tree sees and can learn about
#     - when mtry = all possible features, model is same as bagging decision trees
# - min_n: sets minimum n to split at any node
# will use a space-filling design to tune, with 25 candidate models
set.seed(42)
res_rf <- workflow_rf %>%
  tune_grid(df_val,
    grid = 25,
    control = control_grid(save_pred = TRUE),
    metrics = metric_set(roc_auc)
  )

# top 5 random-forest models out of the 25 candidates are:
show_best(x = res_rf, metric = "roc_auc")
# see that the mean, or area under ROC, are not higher than our penalised multinomial regression,
# plotting results of tuning process highlights:
# mtry should be relatively small to optimise performance
# whereas min_n can take a range of values to optimise performance
autoplot(object = res_rf)

# select best model according to ROC AUC metric
best_rf <- select_best(x = res_rf, metric = "roc_auc")

# get predictions for best random-forest model
# and calculate area under ROC curve
auc_rf <- res_rf %>%
  collect_predictions(parameters = best_rf) %>%
  roc_curve(mode_wellbeing, .pred_1:.pred_5) %>%
  mutate(model = "Random Forest")

# compare validation set ROC curves for penalised multinomial regression model
# and random forest model
bind_rows(auc_rf, auc_lr) %>%
  ggplot(mapping = aes(x = 1 - specificity, y = sensitivity, col = model)) +
  geom_path(lwd = 1.5, alpha = 0.8) +
  geom_abline(lty = 3) +
  coord_equal() +
  scale_color_viridis_d(option = "plasma", end = 0.6)
# random forest and multinomial regression are not too dissimilar,
# though do have random forest covering the diagonal
