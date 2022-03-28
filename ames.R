## https://www.tmwr.org/models.html

library(tidymodels)
tidymodels_prefer(quiet = F)
data("ames")
ames %>% 
  ggplot(aes(Sale_Price)) +
  geom_histogram(bins = 100) +
  scale_x_log10(labels = label_comma()) 

ames <- ames %>% mutate(Sale_Price = log10(Sale_Price))

set.seed(123)
ames_split <- initial_split(ames, prop = 0.80, strata = Sale_Price)
ames_train <- training(ames_split)
ames_test  <-  testing(ames_split)

lm_model <- 
  linear_reg() %>% 
  set_engine("lm")

lm_form_fit <- 
  lm_model %>% 
  # Recall that Sale_Price has been pre-logged
  fit(Sale_Price ~ Longitude + Latitude, data = ames_train)

lm_xy_fit <- 
  lm_model %>% 
  fit_xy(
    x = ames_train %>% select(Longitude, Latitude),
    y = ames_train %>% pull(Sale_Price)
  )

rand_forest(trees = 1000, min_n = 5) %>% 
  set_engine("ranger") %>% 
  set_mode("regression") %>% 
  translate()

lm_form_fit %>% extract_fit_engine()


# Mon Jan 31 19:57:33 2022 ------------------------------

## workflows

lm_model <- 
  linear_reg() %>% 
  set_engine("lm")
lm_wflow <- 
  workflow() %>% 
  add_model(lm_model)
lm_wflow <- 
  lm_wflow %>% 
  add_formula(Sale_Price ~ Longitude + Latitude)
lm_fit <- fit(lm_wflow, ames_train)
predict(lm_fit, ames_test %>% slice(1:3))
lm_fit %>% update_formula(Sale_Price ~ Longitude)
