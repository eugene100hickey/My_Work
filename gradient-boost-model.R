
library(tidyverse)
library(caret)
library(caretEnsemble)
library(showtext)
library(ggokabeito)
library(viridis)
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

# z1 <-
#   list.files(path = "data/", pattern = "*.csv") %>% 
#   paste0("data/", .) %>% 
#   map_df(~read_csv(.)) %>% 
#   # filter(cor > 0.5) %>% 
#   mutate(cor_logit = gtools::logit(cor, min = -1, max = 1) %>% 
#            scale())
# z1$cor_logit <- z1$cor_logit[,1]
# z1 <- z1 %>% arrange(ra)
# doubles <- duplicated(z1$specobjid)
# z1 <- z1[!doubles,]

z1 <- readRDS("data/z_4500_clean_extra") %>% 
  drop_na()
z1_extra <- readRDS("data/z_4500_clean_extra_extra_unnormalised") %>% 
  mutate(cor_logit = gtools::logit(cor, min = -1, max = 1) %>%
           scale())
names(z1_extra) <- names(z1)

## Create Training & Test data
inTraining <- createDataPartition(z1$cor_logit, p = .75, list = FALSE)
training <- z1[ inTraining,] %>% select(u:z, u1:z1, cor_logit)
testing  <- z1[-inTraining,] %>% select(u:z, u1:z1, cor_logit)
x = training %>% select(u:z, u1:z1)
y = training$cor_logit

gbm1 <- readRDS("models/devgenius/gbm1")
plot(gbm1)
gbm1pred<-predict(gbm1, testing)
gbm1values<-data.frame(obs=testing$cor_logit, 
                       pred=gbm1pred, 
                       res=gbm1pred-testing$cor_logit)
defaultSummary(gbm1values)
theme_set(theme_bw())
ggplot(gbm1values, aes(obs,
                       pred, 
                       colour=res)) +
  geom_point(alpha=0.9) + 
  geom_smooth(se=FALSE,colour="red", linetype="dashed", size=0.5)+ 
  geom_abline(slope=1, linetype="dashed")
qqplot(gbm1values$pred, gbm1values$obs)

gbm1values %>% 
  ggplot(aes(res)) +
  geom_histogram(bins = 100) +
  theme_clean()

# Fri Mar 25 16:26:35 2022 ------------------------------

gbm1$results %>% 
  ggplot(aes(RMSE, Rsquared)) + 
  geom_point() +
  theme_clean()

y_hat <- predict(gbm1$finalModel, testing %>% 
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
  observations = testing$cor_logit,
#  survey = testing$survey1,
  cor = testing$cor
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

R2(y_hat, testing$cor_logit)
RMSE(y_hat, testing$cor_logit)

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


