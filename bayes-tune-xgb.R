# https://www.r-bloggers.com/2022/01/using-bayesian-optimisation-to-tune-a-xgboost-model-in-r/?utm_source=phpList&utm_medium=email&utm_campaign=R-bloggers-daily&utm_content=HTML
library(pacman)
# p_load automatically installs packages if needed
p_load(xgboost, ParBayesianOptimization, mlbench, dplyr, skimr, recipes, resample)

# Load up some data
# data("BostonHousing2")

# Data summary
# skim(BostonHousing2)
z2 <- z1 %>% 
  select(u:z, u1:z1, cor_logit)
skim(z2)

# Predicting median house prices
rec <- recipe(cor_logit ~ ., data = z2) 

# Train the recipe on the data set
prep <- prep(rec, training = z2)

# Create the final model matrix
model_df <- bake(prep, new_data = z2)

# All levels have been one hot encoded and separate columns have been appended to the model matrix
colnames(model_df)

splits <- rsample::initial_split(model_df, prop = 0.7)

# Training set
train_df <- rsample::training(splits)

# Test set
test_df <- rsample::testing(splits)

dim(train_df)
## [1] 354  23

dim(test_df)
## [1] 152  23

# The xgboost interface accepts matrices 
X <- train_df %>%
  # Remove the target variable
  select(!cor_logit) %>%
  as.matrix()

# Get the target variable
y <- train_df %>%
  pull(cor_logit)

# Cross validation folds
folds <- list(fold1 = as.integer(seq(1, nrow(X), by = 5)),
              fold2 = as.integer(seq(2, nrow(X), by = 5)))

# Function must take the hyper-parameters as inputs
obj_func <- function(eta, max_depth, min_child_weight, subsample, lambda, alpha) {
  
  param <- list(
    
    # Hyter parameters 
    eta = eta,
    max_depth = max_depth,
    min_child_weight = min_child_weight,
    subsample = subsample,
    lambda = lambda,
    alpha = alpha,
    
    # Tree model 
    booster = "gbtree",
    
    # Regression problem 
    objective = "reg:squarederror",
    
    # Use the Mean Absolute Percentage Error
    eval_metric = "mape")
  
  xgbcv <- xgb.cv(params = param,
                  data = X,
                  label = y,
                  nround = 50,
                  folds = folds,
                  prediction = TRUE,
                  early_stopping_rounds = 5,
                  verbose = 0,
                  maximize = F)
  
  lst <- list(
    
    # First argument must be named as "Score"
    # Function finds maxima so inverting the output
    Score = -min(xgbcv$evaluation_log$test_mape_mean),
    
    # Get number of trees for the best performing model
    nrounds = xgbcv$best_iteration
  )
  
  return(lst)
}

bounds <- list(eta = c(0.001, 0.2),
               max_depth = c(1L, 10L),
               min_child_weight = c(1, 50),
               subsample = c(0.1, 1),
               lambda = c(1, 10),
               alpha = c(1, 10))

set.seed(1234)
bayes_out <- bayesOpt(FUN = obj_func, bounds = bounds, initPoints = length(bounds) + 2, iters.n = 3)

# Show relevant columns from the summary object 
bayes_out$scoreSummary[1:5, c(3:8, 13)]
##           eta max_depth min_child_weight subsample   lambda    alpha      Score
## 1: 0.13392137         8         4.913332 0.2105925 4.721124 3.887629 -0.0902970
## 2: 0.19400811         2        25.454160 0.9594105 9.329695 3.173695 -0.1402720
## 3: 0.16079775         2        14.035652 0.5118349 1.229953 5.093530 -0.1475580
## 4: 0.08957707         4        12.534842 0.3844404 4.358837 1.788342 -0.1410245
## 5: 0.02876388         4        36.586761 0.8107181 6.137100 6.039125 -0.3061535

# Get best parameters
data.frame(getBestPars(bayes_out))
##         eta max_depth min_child_weight subsample lambda alpha
## 1 0.1905414         8         1.541476 0.8729207      1     1

# Combine best params with base params
opt_params <- append(list(booster = "gbtree", 
                          objective = "reg:squarederror", 
                          eval_metric = "mae"), 
                     getBestPars(bayes_out))

# Run cross validation 
xgbcv <- xgb.cv(params = opt_params,
                data = X,
                label = y,
                nround = 100,
                folds = folds,
                prediction = TRUE,
                early_stopping_rounds = 5,
                verbose = 0,
                maximize = F)

# Get optimal number of rounds
nrounds = xgbcv$best_iteration

# Fit a xgb model
mdl <- xgboost(data = X, label = y, 
               params = opt_params, 
               maximize = F, 
               early_stopping_rounds = 5, 
               nrounds = nrounds, 
               verbose = 0)

# Evaluate performance 
actuals <- test_df$cor_logit
predicted <- test_df %>%
  select_at(mdl$feature_names) %>%
  as.matrix %>%
  predict(mdl, newdata = .)

# Compute MAPE
mean(abs(actuals - predicted)/actuals)
## [1] 0.008391282
R2(actuals, predicted)
RMSE(actuals, predicted)
results <- tibble(observations = actuals, predictions = predicted, residuals = actuals-predicted)

results %>% 
  ggplot(aes(predictions, observations)) + 
  geom_point(show.legend = F, size = 0.2) + geom_smooth(se = F) +
  scale_color_viridis_d() +
  theme_clean()
results %>% 
  ggplot(aes(predictions, residuals)) + 
  geom_point(show.legend = F, size = 0.2) + geom_smooth(se = F) +
  scale_color_viridis_d() +
  theme_clean()
results %>% 
  ggplot(aes(residuals)) + 
  geom_histogram(bins = 50) +
  scale_color_viridis_d() +
  theme_clean()
results %>% 
  ggplot(aes(sample = residuals)) + 
  stat_qq(size = 0.5) +
  geom_abline(slope = 1, intercept = 0, col = "firebrick4") +
  scale_color_viridis_d() +
  theme_clean()


grd <- expand.grid(
  eta = seq(0.001, 0.2, length.out = 5),
  max_depth = seq(2L, 10L, by = 1),
  min_child_weight = seq(1, 25, length.out = 3),
  subsample = c(0.25, 0.5, 0.75, 1),
  lambda = c(1, 5, 10),
  alpha = c(1, 5, 10))

dim(grd)

grd_out <- apply(grd, 1, function(par){
  
  par <- append(par, list(booster = "gbtree",objective = "reg:squarederror",eval_metric = "mae"))
  mdl <- xgboost(data = X, label = y, params = par, nrounds = 50, early_stopping_rounds = 5, maximize = F, verbose = 0)
  lst <- data.frame(par, score = mdl$best_score)
  
  return(lst)
})

grd_out <- do.call(rbind, grd_out)

best_par <- grd_out %>%
  data.frame() %>%
  arrange(score) %>%
  .[1,]

# Fit final model
params <- as.list(best_par[-length(best_par)])
xgbcv <- xgb.cv(params = params,
                data = X,
                label = y,
                nround = 100,
                folds = folds,
                prediction = TRUE,
                early_stopping_rounds = 5,
                verbose = 0,
                maximize = F)

nrounds = xgbcv$best_iteration

mdl <- xgboost(data = X, 
               label = y, 
               params = params, 
               maximize = F, 
               early_stopping_rounds = 5, 
               nrounds = nrounds, 
               verbose = 0)

# Evaluate on test set
act <- test_df$medv
pred <- test_df %>%
  select_at(mdl$feature_names) %>%
  as.matrix %>%
  predict(mdl, newdata = .)

mean(abs(act - pred)/act)
## [1] 0.009407723
