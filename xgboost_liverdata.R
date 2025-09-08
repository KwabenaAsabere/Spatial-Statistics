library(catboost)
library(tidyverse)
library(tidymodels)
library(tune)
library(bonsai)
library(janitor)


liver_data <- readr::read_csv(
  "Liver Data.csv",
  locale = locale(encoding = "latin1")
)

liver_data <- liver_data |> clean_names()
head(liver_data)
names(liver_data)

glimpse(liver_data)

liver_data <- liver_data |>
  mutate(
    gender_of_the_patient = factor(gender_of_the_patient),
    result = if_else(result == 2, 1, 0),
    result = factor(result)
  )


liver_split <- initial_split(liver_data, prop = 0.8, strata = "result")
liver_split

liver_train <- training(liver_split)
liver_test <- testing(liver_split)

dim(liver_train)

predictors <- names(select(liver_data, -result))
response <- "result"

xgb_formula <- as.formula(paste0("result ~", paste(predictors, collapse = "+")))
print(xgb_formula)

xgb_spec <- boost_tree(
  trees = 1000,
  tree_depth = tune(),
  min_n = tune(),
  loss_reduction = tune(),
  sample_size = tune(),
  mtry = tune(),
  learn_rate = tune()
) |>
  set_engine("xgboost") |>
  set_mode("classification")


xgb_spec

xgb_grid <- grid_latin_hypercube(
  tree_depth(),
  min_n(),
  loss_reduction(),
  sample_size = sample_prop(),
  finalize(mtry(), liver_train),
  learn_rate(),
  size = 20
)

xgb_grid


xgb_wf <- workflow() |>
  add_formula(xgb_formula) |>
  add_model(xgb_spec)

xgb_wf

set.seed(123)
liver_folds <- vfold_cv(liver_train, strata = result)
liver_folds

library(doFuture)
registerDoFuture()
plan(multisession, workers = 10)
set.seed(234)
xgb_res <- tune_grid(
  xgb_wf,
  resamples = liver_folds,
  grid = xgb_grid,
  control = control_grid(save_pred = TRUE, save_workflow = TRUE)
)

xgb_res

xgb_res |>
  collect_metrics() |>
  filter(.metric == "roc_auc") |>
  select(mean, mtry:sample_size) |>
  pivot_longer(mtry:sample_size, names_to = "parameter", values_to = "value") |>
  ggplot(aes(x = value, y = mean, color = parameter)) +
  geom_point(show.legend = FALSE) +
  facet_wrap(~parameter, scales = "free")

show_best(xgb_res, metric = "roc_auc")

best_auc <- select_best(xgb_res, metric = "roc_auc")

final_xgb <- finalize_workflow(xgb_wf, best_auc)

final_xgb

library(vip)
final_xgb |>
  fit(data = liver_train) |>
  pull_workflow_fit() |>
  vip(geom = "point")

final_res <- last_fit(final_xgb, liver_split)

final_res |>
  collect_metrics()

final_res |>
  collect_predictions() |>
  conf_mat(result, .pred_class)

final_res |>
  collect_predictions() |>
  roc_curve(result, .pred_0) |>
  autoplot()
