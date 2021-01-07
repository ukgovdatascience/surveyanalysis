source(file = "notebooks/causality/051_model_mr_updownsampling.R")


# Model: Tree-based ensemble ----------------------------------------------
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
best_15_rf <- show_best(x = res_rf, metric = "roc_auc", n = 15)
best_rf <- select_best(x = res_rf, metric = "roc_auc")
# from plot below, want small `mtry` for optimal performance
# whereas `min_n` can take a number of values
autoplot(object = res_rf)

# predictions for best rf model
auc_rf <- res_rf %>%
  collect_predictions(parameters = best_rf) %>%
  roc_curve(mode_wellbeing, .pred_1:.pred_5) %>%
  mutate(model = "Random Forest")

# compare validation set ROC curves for mr and rf models
# find mr model better because random forest they overlap
# but it doesn't cover the diagonal, which rf does
bind_rows(auc_rf, auc_mr) %>%
  ggplot(mapping = aes(x = 1 - specificity, y = sensitivity, col = model)) +
  geom_path(lwd = 1.5, alpha = 0.8) +
  geom_abline(lty = 3) +
  coord_equal() +
  scale_color_viridis_d(option = "plasma", end = 0.6)
