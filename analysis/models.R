library(tidyverse)
library(doParallel)
library(foreach)
library(recipes)
library(caret)

embeddings <- readRDS("get_videoembeddings/results/embeddings.RDS")
all <- readRDS("analysis/results/all_subj.RDS")
all <- left_join(all, embeddings, by = c("Video", "Frame"))

subj <- unique(all$ID)

all <- all %>% 
  #filter(ID %in% subj[1:2]) %>%
  mutate(ID = factor(ID),
         isInVid = factor(isInVid, labels = c("0" = "no_check", "1" = "check")),
         ITI = factor(ITI),
         RewardCond = factor(RewardCond))

#split in test and train set

set.seed(26041997)

all <- all %>%
  group_by(ID, ITI, RewardCond) %>%
  mutate(is_test = rep(c(0,0,0,0,1), length.out = n()),
         is_test = sample(is_test), .before = time) 

test <- all %>%
  filter(is_test == 1) %>%
  select(-is_test) %>%
  select(-Block, -Trials, -Frame, -Video, -Img)

train <- all %>%
  filter(is_test == 0) %>%
  select(-is_test) %>%
  select(-Block, -Trials, -Frame, -Video, -Img)

#make folds
n_folds <-  10

train <- train %>%
  group_by(ID, ITI, RewardCond) %>%
  mutate(folds = rep(1:n_folds, length.out = n()),
         folds = sample(folds))

folds <- vector('list', n_folds)

for(i in 1:n_folds){
  folds[[i]] <- which(train$folds!=i)
}

train <- train %>%
  select(-folds)


#now I split train and test set into datasets containing and not containing embeddings
train_img_content <- train
train_no_img_content <- train %>% select(-c(V1:V512))
test_img_content <- test
test_no_img_content <- test %>% select(-c(V1:V512))

train_list <- list(no_img_content = train_no_img_content,
                   img_content = train_img_content)

test_list <- list(no_img_content = test_no_img_content,
                   img_content = test_img_content)

saveRDS(train_list, "analysis/results/train_data.RDS")
saveRDS(test_list, "analysis/results/test_data.RDS")

#creating blueprints
blueprint_img_content <- recipe(x = train_img_content ,
                                roles = c(rep("predictor", 4), "outcome", rep("predictor", 512))) %>%
  step_dummy(all_factor_predictors(), one_hot=TRUE) %>% #dummycode all factors
  step_normalize(all_numeric_predictors()) #normalize all numeric predictors

blueprint_no_img_content <- recipe(x = test_no_img_content ,
                                roles = c(rep("predictor", 4), "outcome")) %>%
  step_dummy(all_factor_predictors(), one_hot=TRUE) %>%
  step_normalize(all_numeric_predictors())

cv <- trainControl(method          = "cv",
                   index           = folds,
                   classProbs      = TRUE,
                   summaryFunction = mnLogLoss)

#setting up tuning grids for ridge regression, lasso regression and elastic net
#the values were obtained after running these models several times and observing the logloss
#the tuning grid was then adjusted so that we are able to zoom in.

#Ridge
tuning_rr <- expand.grid(alpha = 0,
                      lambda = seq(0, 0.01, by = 0.0002))

#Lasso
tuning_lr <- expand.grid(alpha = 1,
                         lambda = seq(0, 0.01, by = 0.0002))

#Elastic
tuning_en <- expand.grid(alpha = seq(0.001, 0.999, by = 0.05),
                         lambda = seq(0, 0.001, by = 0.000025))

#make it a parallel process
cl <- makeCluster(10)
registerDoParallel(cl)

#list of ridge and lasso regression and elastic net for image content
logistic_img_content <- list(rr = caret::train(blueprint_img_content, 
                                               data      = train_img_content, 
                                               method    = "glmnet",
                                               family    = "binomial",
                                               metric    = "logLoss",
                                               trControl = cv,
                                               tuneGrid  = tuning_rr),
                             lr = caret::train(blueprint_img_content, 
                                               data      = train_img_content, 
                                               method    = "glmnet",
                                               family    = "binomial",
                                               metric    = "logLoss",
                                               trControl = cv,
                                               tuneGrid  = tuning_lr),
                             en = caret::train(blueprint_img_content, 
                                               data      = train_img_content, 
                                               method    = "glmnet",
                                               family    = "binomial",
                                               metric    = "logLoss",
                                               trControl = cv,
                                               tuneGrid  = tuning_en))

saveRDS(logistic_img_content, "analysis/results/caret_img_content.RDS")

#list of ridge and lasso regression and elastic net without image content
logistic_no_img_content <- list(rr = caret::train(blueprint_no_img_content, 
                                                  data      = train_no_img_content, 
                                                  method    = "glmnet",
                                                  family    = "binomial",
                                                  metric    = "logLoss",
                                                  trControl = cv,
                                                  tuneGrid  = tuning_rr),
                                lr = caret::train(blueprint_no_img_content, 
                                                  data      = train_no_img_content, 
                                                  method    = "glmnet",
                                                  family    = "binomial",
                                                  metric    = "logLoss",
                                                  trControl = cv,
                                                  tuneGrid  = tuning_lr),
                                en = caret::train(blueprint_no_img_content, 
                                                  data      = train_no_img_content, 
                                                  method    = "glmnet",
                                                  family    = "binomial",
                                                  metric    = "logLoss",
                                                  trControl = cv,
                                                  tuneGrid  = tuning_en))

saveRDS(logistic_no_img_content, "analysis/results/caret_no_img_content.RDS")

stopCluster(cl)




