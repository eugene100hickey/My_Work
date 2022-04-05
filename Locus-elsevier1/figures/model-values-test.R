library(tidyverse)
library(gt)


# SVR, GBM, RF, LM

zq <- tribble(~models, ~RMSE, ~Rsquared, ~MAE,
              "SVR", 0.5543971, 0.6929413, 0.3937136, 
              "GBM", 0.5840478, 0.6545808, 0.4225466,
              "RF", 0.5896328, 0.6480536, 0.4273223,
              "LM", 0.6393757, 0.5870244, 0.4804900 
              )


zq |> 
  # mutate(RMSE = round(RMSE, 3),
  #        Rsquared = round(Rsquared, 3),
  #        MAE = round(MAE, 3)) |> 
  gt(rowname_col = "models") |> 
  fmt_number(
    columns = 1:4,
    decimals = 3,
    suffixing = TRUE
  ) |> 
  cols_label(RMSE = md("RMSE"),
             Rsquared = md("R<sup>2</sup>"),
             MAE = md("MAE")) |> 
  gtsave("Locus-Elsevier/figures/model-values-test.png")
