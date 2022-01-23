library(tidyverse)
library(GGally)
library(caret)
library(ggbiplot)
library(showtext)
library(ggokabeito)
library(viridis)
library(patchwork)
library(xgboost)

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

z1 <-
  list.files(path = "data/", pattern = "*.csv") %>% 
  paste0("data/", .) %>% 
  map_df(~read_csv(.)) %>% 
  # filter(cor > 0.5) %>% 
  mutate(cor_logit = gtools::logit(cor, min = -1, max = 1) %>% 
           scale())
z1$cor_logit <- z1$cor_logit[,1]

# hist1 <- z1 %>% ggplot(aes(cor)) +
#   geom_histogram(aes(y = ..density..), 
#                  bins = 200,
#                  fill = "gray70") +
#   theme_clean() +
#   coord_cartesian(xlim = c(0.8, 1)) +
#   geom_density() +
#   labs(x = "Pearson Correlation") +
#   theme(axis.text.y = element_blank())
# 
# hist2 <- z1 %>% ggplot(aes(cor_logit)) +
#   geom_histogram(aes(y = ..density..), 
#                  bins = 80,
#                  fill = "gray70") +
#   theme_clean() +
# #  coord_cartesian(xlim = c(0.8, 1)) +
#   geom_density() +
#   labs(x = "Logit Correlation") +
#   theme(axis.text.y = element_blank(),
#         axis.title.y = element_blank())
# 
# (hist1 + hist2)  + 
#   plot_annotation(tag_levels = 'A',
#                   tag_prefix = "(",
#                   tag_suffix = ")")

# my_fn <- function(data, mapping, method="loess", ...){
#   p <- ggplot(data = data, mapping = mapping) + 
#     geom_point(size = 0.2, alpha = 0.4, col = "black") + 
#     geom_smooth(method=method, ..., se = F, col = "darkblue")
#   p
# }
# 
# z %>% select(u1:z1, cor_logit, survey1) %>% 
#   ggpairs(mapping = aes(col = survey1), 
#           lower = list(continuous=wrap("points", 
#                                        size = 0.2, 
#                                        alpha=0.4)))
# 
# z %>% select(u1:z1, cor_logit, survey1) %>% 
#   ggpairs(mapping = aes(col = survey1), 
#           lower = list(continuous=wrap("points", 
#                                        size = 0.2, 
#                                        alpha=0.4)),
#           diag = list(continuous=wrap("densityDiag", alpha = 0.2)))
# 
# z %>% mutate(delta1 = (u-g)-(u1-g1),
#              delta2 = (g-r)-(g1-r1),
#              delta3 = (r-i)-(r1-i1),
#              delta4 = (i-z)-(i1-z1)) %>% 
#   filter(delta1 %>% abs < 0.5,
#          delta4 %>% abs < 0.5) %>% 
#   select(delta1:delta4, cor_logit, survey1) %>% 
#   ggpairs(mapping = aes(col = survey1), 
#           lower = list(continuous = my_fn),
#           diag = list(continuous=wrap("densityDiag", alpha = 0.2)))
# 
# z1 <- z %>% mutate(delta1 = (u-g)-(u1-g1),
#              delta2 = (g-r)-(g1-r1),
#              delta3 = (r-i)-(r1-i1),
#              delta4 = (i-z)-(i1-z1),
#              interval_class = cut_interval(cor_logit, n=10)) %>% 
#   filter(survey1 != "boss1")
# 
# prcomp(z1 %>% select(u:z, u1:z1),
#        center = T,
#        scale. = T) %>%
#   ggbiplot(
#     alpha = 0.05,
#     groups = z1$interval_class,
#     ellipse = T,
#     var.scale = 2
#   ) +
#   xlim(-5, 5) +
#   theme_clean()
z1 <- z1 %>% arrange(ra)
doubles <- duplicated(z1$specobjid)
z1 <- z1[!doubles,]
index <- createDataPartition(z1$cor_logit, p = 0.7, list = F) %>% 
  as.integer()
train <- z1[index,]
test <- z1[-index,]
trainCtrl <- trainControl(method = "boot632")
# rf_grid <- expand.grid(mtry = c(2, 3, 4),
#                        splitrule = c("gini", "extratrees"),
#                        min.node.size = c(1, 3, 5))

# Mon Apr 27 08:14:11 2020 ------------------------------
# train_x <- train %>% select(delta1, delta2, delta3, delta4)
train_x <- train %>% select(u:z, u1:z1)
train_y <- train %>% select(cor_logit)
# test_x <- test %>% select(delta1, delta2, delta3, delta4)
test_x <- test %>% select(u:z, u1:z1)
test_y <- test %>% select(cor_logit)


model_xgb <- readRDS("models/model_xgb-5591")
xgb.plot.tree(model = model_xgb$finalModel, trees = 42)

bst <- xgboost(data = train_x[1:200,] %>% as.matrix, label = train_y$cor_logit[1:200], max_depth = 6,
               eta = 1, nthread = 2, nrounds = 2)

model_xgb <- train(x = train_x,
                   y = train_y$cor_logit,
                   method = "xgbTree",
                   trControl = trainCtrl)
# Mon Apr 27 08:28:25 2020 ------------------------------


# model_rpart <- train(cor_logit ~ ., 
#                      data = train %>% select(u:z, u1:z1, survey1, cor_logit),
#                      method = "rpart",
#                      trControl = trainCtrl)
# 
# model_xgb <- train(cor_logit ~ delta1+delta2+delta3+delta4, 
#                    data = train, 
#                    method = "xgbTree", 
#                    trControl = trainCtrl)
# model_rf <- train(cor_logit ~ ., 
#                   data = train %>% select(u:z, u1:z1, cor_logit), 
#                   method = "rf", 
#                   trControl = trainCtrl)
# model_ranger <- train(
#   cor_logit ~ .,
#   data = train %>% select(u:z, u1:z1, cor_logit),
#   method = "ranger",
#   trControl = trainCtrl,
#   tuneGrid = rf_grid
# )

model_xgb$results %>% 
  ggplot(aes(RMSE, Rsquared)) + 
  geom_point() +
  theme_clean()

y_hat <- predict(model_xgb$finalModel, test %>% 
                   select(u:z, u1:z1) %>% 
                   as.matrix())

# y_hat <- predict(model_ranger$finalModel, test %>% 
#                    select(u:z, u1:z1))
# 
# 
# y_hat <- predict(model_rf$finalModel, test %>% 
#                  select(delta1, delta2, delta3, delta4))

results <- data.frame(
  predictions = y_hat,
  observations = test$cor_logit,
  survey = test$survey1,
  cor = test$cor
) %>%
  mutate(residuals = observations - predictions)

results %>% 
  ggplot(aes(predictions, observations)) + 
  geom_point(show.legend = F, alpha = 0.5, size = 2) + 
  geom_smooth(se = F, show.legend = F, method = "lm") +
  scale_color_viridis_d() +
  theme_clean()
results %>% 
  ggplot(aes(predictions, residuals)) + 
  geom_point(show.legend = F, alpha = 0.5, size = 2) + 
  geom_smooth(se = F, show.legend = F, method = "lm") +
  scale_color_viridis_d() +
  theme_clean()
results %>% 
  ggplot(aes(residuals)) + 
  geom_histogram(bins = 30) +
  scale_color_viridis_d() +
  theme_clean() +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank())
results %>% 
  ggplot(aes(sample = residuals)) + 
  stat_qq() +
  geom_abline(slope = 1, intercept = 0) +
  scale_color_viridis_d() +
  theme_clean()

R2(y_hat, test$cor_logit)
RMSE(y_hat, test$cor_logit)

# Wed Jan 19 17:10:44 2022 ------------------------------
# function to calculate rating. Uses Oisin's routine
M <- 0.5
rating <- function(gr, rr, ir, gt, rt, it) {
  delta.CS <- (gt - rt) - (gr - rr)
  delta.CL <- (rt - it) - (rr - ir)
  RS <- 1 - abs(delta.CS / M)
  RL <- 1 - abs(delta.CL / M)
  RS * RL
}
z1 <- z1 %>% 
  mutate(oisin = rating(g, r, i, g1, r1, i1))
cor(z1$oisin, z1$cor_logit)

