# https://blog.devgenius.io/ensemble-models-using-caret-in-r-d54e4e646968

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

z1 <- readRDS("data/z_2500_no_doubles") %>% 
  drop_na()

tr <- trainControl(method = "repeatedcv",
                   number = 20,
                   repeats = 10)
tr1 <- trainControl(method = "boot632",number =1000)

set.seed(998)
# cl <- makePSOCKcluster(4)
# registerDoParallel(cl)
## Create Training & Test data
inTraining <- createDataPartition(z1$cor_logit, p = .75, list = FALSE)
training <- z1[ inTraining,] %>% select(u:z, u1:z1, cor_logit)
testing  <- z1[-inTraining,] %>% select(u:z, u1:z1, cor_logit)
x = training %>% select(u:z, u1:z1)
y = training$cor_logit

### First the Linear Regression using AIC step-wise selection.

lm1 <- train(cor_logit~., data=training, 
             method = "lmStepAIC",trControl=tr)
summary(lm1)
fitted <- predict(lm1)
lmpred1<-predict(lm1, testing)
lmvalues1<-data.frame(obs=testing$cor_logit, pred=lmpred1)
plot(varImp(lm1))
defaultSummary(lmvalues1)
lm1values<-data.frame(obs=testing$cor_logit, 
                      pred=lmpred1, 
                      res=lmpred1-testing$cor_logit)
theme_set(theme_bw())

ggplot(lm1values, aes(obs,pred, colour=res)) +
  geom_point(alpha=0.9) + 
  geom_smooth(se=FALSE,colour="red", 
              linetype="dashed", size=0.5)+ 
  geom_abline(slope=1, linetype="dashed")

### Next up is the random forest model. I am just using standard grid searches to get the model going.

tg <- data.frame(mtry = seq(2, 10, by =2))
rf1 <- train(cor_logit~., data = training, 
             method = "rf",trControl=tr, tuneGrid = tg) 
rf1$results
class(rf1)
attributes(rf1)
varImp(rf1)
arrange(rf1$results, RMSE) %>% head
rf1pred<-predict(rf1, testing)
rf1values<-data.frame(obs=testing$cor_logit, 
                      pred=rf1pred, 
                      res=rf1pred-testing$cor_logit)
defaultSummary(rf1values)
theme_set(theme_bw())
ggplot(rf1values, aes(obs,pred,colour=res)) +
  geom_point(alpha=0.9) + 
  geom_smooth(se=FALSE,colour="red", linetype="dashed", size=0.5)+ 
  geom_abline(slope=1, linetype="dashed")



### Then, the Multivariate Adaptive Regression Splines.

marsgrid<-expand.grid(.degree = 1:2, .nprune = 2:38)
set.seed(100)
mars1 <- train(cor_logit~., data = training, 
               method = "earth",
               preProcess = c("center", "scale"),
               trControl=tr, 
               tuneGrid = marsgrid)
plot(varImp(mars1))
defaultSummary(mars1)
mars1pred<-predict(mars1, testing)
mars1pred<-mars1pred[1:317,]
mars1values<-tibble(obs=testing$cor_logit,
                        pred=mars1pred, 
                        res=mars1pred-testing$cor_logit)
theme_set(theme_bw())
ggplot(mars1values, aes(obs, 
                        pred, 
                        colour=res)) +
  geom_point(alpha=0.9) + 
  geom_smooth(se=FALSE,colour="red", linetype="dashed", size=0.5)+ 
  geom_abline(slope=1, linetype="dashed") 
featurePlot(mars1values$obs, mars1values$pred)

testing$pred<-predict(mars1, testing)


## Penalized regression

pm1 <- train(cor_logit~., data = training, 
             preProcess = c("center", "scale"),
             method = "glmnet", trControl = tr)
arrange(pm1$results, RMSE) %>% head
pm1$bestTune
pm1pred<-predict(pm1, testing)
pm1values<-data.frame(obs=testing$cor_logit, 
                      pred=pm1pred, 
                      res=pm1pred-testing$cor_logit)
defaultSummary(pm1values)
theme_set(theme_bw())
ggplot(pm1values, aes(obs,pred, colour=res)) +
  geom_point(alpha=0.9) + 
  geom_smooth(se=FALSE,colour="red", linetype="dashed", size=0.5)+ 
  geom_abline(slope=1, linetype="dashed")

## Elastic Net.

enetgrid<-expand.grid(.lambda=c(0,0.01,.1),
                      .fraction = seq(0.5, 1 , length=20))
enet1 <- train(cor_logit~., data = training, 
               preProcess = c("center", "scale"),
               method = "enet", 
               tuneGrid=enetgrid,
               trControl = tr)
plot(enet1)
enet1pred<-predict(enet1, testing)
enet1values<-data.frame(obs=testing$cor_logit, 
                        pred=enet1pred, 
                        res=enet1pred-testing$cor_logit)
defaultSummary(enet1values)
theme_set(theme_bw())
ggplot(enet1values, aes(obs,
                        pred, 
                        colour=res)) +
  geom_point(alpha=0.9) + 
  geom_smooth(se=FALSE,colour="red", linetype="dashed", size=0.5)+ 
  geom_abline(slope=1, linetype="dashed")


## SVM

set.seed(100)
svm1 <- train(cor_logit~., data = training,
              preProcess = c("center", "scale"),
              method = "svmRadial",
              tuneLength=14,
              trControl=tr)
svm1
plot(svm1)
svm1pred<-predict(svm1, testing)
svm1values<-data.frame(obs=testing$cor_logit, 
                       pred=svm1pred, 
                       res=svm1pred-testing$cor_logit)
defaultSummary(svm1values)
theme_set(theme_bw())
ggplot(svm1values, aes(obs,
                       pred, 
                       colour=res)) +
  geom_point(alpha=0.9) + 
  geom_smooth(se=FALSE,colour="red", linetype="dashed", size=0.5)+ 
  geom_abline(slope=1, linetype="dashed")


##  Gradient Boosting

tg <- expand.grid(shrinkage = seq(0.1, 1, by = 0.2), 
                  interaction.depth = c(1, 3, 7, 10),
                  n.minobsinnode = c(2, 5, 10),
                  n.trees = c(100, 300, 500, 1000))
gbm1<- train(cor_logit~., data = training, 
             method = "gbm", trControl = tr, tuneGrid =tg, verbose = FALSE)
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

## COMPARE MODELS

models_compare <- resamples(list(
  REG=lm1,
  PEN=pm1,
  SVM=svm1,
  MARS=mars1,
  RF=rf1,
  EL=enet1,
  GB=gbm1))
summary(models_compare)
scales <- list(x=list(relation="free"), y=list(relation="free"))
bwplot(models_compare, scales=scales)
compare_models(enet1, mars1)


## ENSEMBLE MODEL

trainControl <- trainControl(method="repeatedcv", 
                             number=10, 
                             repeats=3,
                             savePredictions=TRUE)
algorithmList <- c('lm', 'gbm', 'svmRadial', 'earth')
set.seed(100)
models <- caretList(cor_logit~., data = z1 %>% select(u:z, u1:z1, cor_logit), 
                    trControl=trainControl, 
                    methodList=algorithmList) 
results <- resamples(models)
summary(results)
scales <- list(x=list(relation="free"), y=list(relation="free"))
bwplot(results, scales=scales)

# Combine Predictions from multiple models
set.seed(101)
stackControl <- trainControl(method="repeatedcv", 
                             number=10, 
                             repeats=3,
                             savePredictions=TRUE)

# Ensemble the predictions of `models` to form a new combined prediction based on glm
stack.glm <- caretStack(models, method="glm", trControl=stackControl)
print(stack.glm)
stack_predicteds <- predict(stack.glm, newdata=testing)
#Create data frame
stackvalues<-data.frame(obs=testing$cor_logit, 
                        pred=stack_predicteds, 
                        res=stack_predicteds-testing$cor_logit)
defaultSummary(stackvalues)


# Plot predictions
theme_set(theme_bw())
ggplot(stackvalues, aes(obs,pred, colour=res)) +
  geom_point(alpha=0.5) + 
  geom_smooth(method=lm, se=FALSE,colour="red", linetype="dashed", size=0.5)+ 
  geom_abline(slope=1, linetype="dashed")
