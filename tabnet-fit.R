library(tidymodels)
library(tabnet)
library(showtext)
tidymodels_prefer(quiet = F)

set.seed(42)

font_add("Fuzzy Bubbles", regular = "fonts/ABeeZee-Regular.ttf")
showtext_auto()
theme_clean <- function() {
  theme_minimal(base_family = "Fuzzy Bubbles") +
    theme(panel.grid.minor = element_blank(),
          text = element_text(size = 16, family = "Fuzzy Bubbles"),
          plot.background = element_rect(fill = "white", color = NA),
          axis.text = element_text(size = 16),
          axis.title = element_text(face = "bold", size = 20),
          strip.text = element_text(face = "bold", size = rel(0.8), hjust = 0),
          strip.background = element_rect(fill = "grey80", color = NA),
          legend.text = element_text(size = 16))
}

z1 <- readRDS("data/z_4500_clean_extra") %>% 
  drop_na() %>% 
  select(u:z, u1:z1, cor_logit)
z1$cor_logit <- z1$cor_logit[,1]

z1_split <- initial_split(z1, prop = 0.70, strata = cor_logit)
z1_train <- training(z1_split)
z1_test  <-  testing(z1_split)


##https://rviews.rstudio.com/2019/06/19/a-gentle-intro-to-tidymodels/
z1_recipe <- training(z1_split) %>%
  recipe(cor_logit ~.) %>%
  step_normalize(all_numeric(), -all_outcomes())


fit_regress <- tabnet_fit(z1_recipe, 
                          z1_train, 
                          epochs = 30, 
                          valid_split = 0.25, 
                          verbose = T)

z_predict <- predict(fit_regress, z1_test)
z1_testing <- z1_recipe %>%
  bake(testing(z1_split)) 

glimpse(z1_testing)

z1_training <- juice(z1_recipe)

z1_ranger <- rand_forest(trees = 100, mode = "regression") %>%
  set_engine("ranger") %>%
  fit(cor_logit ~ ., data = z1_training)
z1_rf <-  rand_forest(trees = 100, mode = "regression") %>%
  set_engine("randomForest") %>%
  fit(cor_logit ~ ., data = z1_training)
z1_svm <-  svm_rbf() %>% 
  set_engine("kernlab") %>% 
  set_mode("regression") %>% 
  fit(cor_logit ~ ., data = z1_training)

z1_svm %>%
  predict(z1_testing) %>%
  bind_cols(z1_testing) %>%
  metrics(truth = cor_logit, estimate = .pred)

# Sun Feb  6 17:22:24 2022 ------------------------------

svm_rbf() %>% 
  set_engine("kernlab") %>% 
  set_mode("regression") %>% 
  translate()

svm_rec <- 
  recipe(cor_logit ~ ., data = z1) 
svm_spec <-
  svm_rbf(cost = tune(), rbf_sigma = tune()) %>%
  set_mode("regression") %>%
  set_engine("kernlab")
svm_wflow <- 
  workflow() %>% 
  add_model(svm_spec) %>% 
  add_recipe(svm_rec)



rf_model <- 
  rand_forest(trees = 1000) %>% 
  set_engine("ranger") %>% 
  set_mode("regression")

rf_wflow <- 
  workflow() %>% 
  add_formula(
    cor_logit ~ u+g+r+i+z+u1+g1+r1+i1+z1) %>% 
  add_model(rf_model) 

rf_fit <- rf_wflow %>% fit(data = z1_train)

estimate_perf <- function(model, dat) {
  # Capture the names of the objects used
  cl <- match.call()
  obj_name <- as.character(cl$model)
  data_name <- as.character(cl$dat)
  data_name <- gsub("z1_", "", data_name)
  
  # Estimate these metrics:
  reg_metrics <- metric_set(rmse, rsq)

  model %>%
    predict(dat) %>%
    bind_cols(dat %>% select(cor_logit)) %>%
    reg_metrics(cor_logit, .pred) %>%
    select(-.estimator) %>%
    mutate(object = obj_name, data = data_name)
}
estimate_perf(rf_fit, z1_train)
estimate_perf(rf_fit, z1_test)
