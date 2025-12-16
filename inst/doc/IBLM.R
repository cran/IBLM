## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 5,
  fig.align = "center"
)

## ----install, eval=FALSE------------------------------------------------------
# # From CRAN
# install.packages("IBLM")
# 
# # From GitHub
# remotes::install_github("IFoA-ADSWP/IBLM")

## ----load-package-------------------------------------------------------------
library(IBLM)

## ----other-packages, echo=FALSE, message=FALSE--------------------------------
library(dplyr)
library(ggplot2)
library(gt)

## ----data-prep----------------------------------------------------------------
df <- load_freMTPL2freq()

df <- df |> mutate(ClaimNb = round(ClaimNb)) 

df_list <- df |> split_into_train_validate_test(seed = 1)

## ----train--------------------------------------------------------------------
iblm_model <- train_iblm_xgb(
  df_list,
  response_var = "ClaimNb",
  family = "poisson",
  params = list(seed = 1)
)

class(iblm_model)

## ----train-xgb----------------------------------------------------------------
xgb_model <- train_xgb_as_per_iblm(iblm_model)

is_identical_config <- purrr::map2_lgl(
  xgboost::xgb.config(xgb_model) |> unlist(),
  xgboost::xgb.config(iblm_model$booster_model) |> unlist(),
  identical
) 

# the config is mostly identical. In our example the differences are:
is_identical_config[!is_identical_config] |> names()


## ----explain-example----------------------------------------------------------
ex <- explain_iblm(iblm_model, df_list$test)

## ----explain-beta_correct_density-VehPower------------------------------------
ex$beta_corrected_density(varname = "VehPower")

## ----explain-beta_correct_density-VehAge--------------------------------------
ex$beta_corrected_density(varname = "VehAge")

## ----explain-beta_correct_density-DrivAge-------------------------------------
ex$beta_corrected_density(varname = "DrivAge")

## ----explain-beta_correct_density-BonusMalus----------------------------------
ex$beta_corrected_density(varname = "BonusMalus")

## ----beta_correct_density-cat, fig.width=12, fig.height=20, fig.align="center"----
VehBrand <- ex$beta_corrected_density(varname = "VehBrand", type = "hist")

VehBrand |> patchwork::wrap_plots(ncol = 2) 

## ----explain-beta_correct_scatter-num-----------------------------------------
ex$beta_corrected_scatter(varname = "DrivAge", color = "VehPower")

## ----explain-beta_correct_scatter-cat-----------------------------------------
ex$beta_corrected_scatter(varname = "VehBrand")

## ----explain-bias_density-var-------------------------------------------------
bias_corrections <- ex$bias_density()
bias_corrections$bias_correction_var

## ----explain-bias_density-total-----------------------------------------------
bias_corrections <- ex$bias_density()
bias_corrections$bias_correction_total

## ----explain-overall_correction-log-------------------------------------------
ex$overall_correction()

## ----explain-overall_correction-identity--------------------------------------
ex$overall_correction(transform_x_scale_by_link = FALSE)

## ----predict-example----------------------------------------------------------
predictions <- predict(iblm_model, df_list$test)

## ----predict-alternative------------------------------------------------------
coeff_multiplier <- 
  df_list$test |>
  select(-all_of("ClaimNb")) |>
  mutate(
    across(
      all_of(iblm_model$predictor_vars$categorical),
      ~1
      )
    ) |>
  mutate(bias = 1, .before = 1)

predictions_alt <- 
  (ex$data_beta_coeff * coeff_multiplier) |>
  rowSums() |> 
  exp() |>
  unname()

# difference in predictions very small between two alternative methods
range(predictions_alt / predictions - 1)

## ----pinball-alternative------------------------------------------------------
get_pinball_scores(
  data = df_list$test, 
  iblm_model = iblm_model,
  additional_models = list(xgb = xgb_model)
  ) |> 
  gt() |> 
  fmt_percent("pinball_score")

## ----correction-corridor------------------------------------------------------
correction_corridor(
  iblm_model, 
  df_list$test,
  trim_vals = c(0.5, 0),
  sample_perc = 0.1,
  color = "DrivAge")

