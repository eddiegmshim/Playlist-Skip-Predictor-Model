# XGBOOST MODEL BUILDING
{
install.packages('ggplot2')
install.packages('tidyverse')
install.packages('magrittr')
install.packages('dplyr')
install.packages('xgboost')
install.packages('Ckmeans.1d.dp')
install.packages('DiagrammeR')
install.packages('devtools')
install_git("git://github.com/jpmml/r2pmml.git")
install.packages('lubridate')

require(ggplot2)
require(xgboost)
require(magrittr)
require(dplyr)
require(tidyverse)
require(ggplot2)
require(Ckmeans.1d.dp)
require(DiagrammeR)
require(devtools)
require(r2pmml)
require(lubridate)
}

setwd('~/data challenge/')


#MODEL DATA WRANGLING   -----------------------------------------------
{
model_data = working_data %>% mutate(
  
  #CREATE variable took_rec, 0 if didn't listen for at least 5% length of track; 1 else
  took_rec = ifelse(percentage_listened<.05,0,1),
  
  #delete non-intuitive variables without intuitive signal
  client_version = NULL,
  track_id = NULL,
  listener_id = NULL,
  percentage_listened = NULL, #this is the variable we are trying to predict. should not be independent variable
  track_duration = NULL, #already covered by track_duration_seconds
  listen_duration = NULL, #already covered by listen_duration_seconds
  listener_prev_month_listening_time = NULL, #already covered by listener_prev_month_listening_time_hours
  
  track_duration_seconds = NULL, #the dependent variable, took_rec is derived from this metric
  listen_duration_seconds = NULL, #the dependent variable, took_rec is derived from this metric
  
  listener_prev_month_avg_daily_tracks_listened = as.numeric(listener_prev_month_avg_daily_tracks_listened),
  
  ts = as.POSIXct(ts), #transform ts from POSIXct into hour, day, and year so the algorithm can digest (datatype factor)
  ts_hour = as.factor(as.POSIXlt(ts)$hour),
  ts_day = as.factor(weekdays(ts)),
  ts_year = as.factor(year(as.Date(ts))),
  ts = NULL,
  
  track_upload_date = as.POSIXct(track_upload_date),#transform track_upload date into day (datatype factor)
  track_upload_day = as.factor(weekdays(track_upload_date)),
  track_upload_date = NULL,
  
  listener_signup_date = as.POSIXct(listener_signup_date), #transform listener_signup_date into day (datatype factor)
  listener_signup_day = as.factor(weekdays(listener_signup_date)),
  listener_signup_date = NULL
  
)

model_data = model_data[!is.na(model_data$took_rec),] #deleting rows with NA took_rec. only 10 rows so likely issue with initial data

}

#CROSS VALIDATION / PARAMETER & MODEL SELECTION SELECTION -----------------------------------------------
{
#these are the variables you want out of cross validation
best_param = list()
best_seednumber = 1
best_logloss = Inf
best_logloss_index = 0

#TO FIND OPTIMAL PARAMETERS HANDED TO XGBOOST

#using k-fold cross validation
for (iter in 1:5) {
  #parameters handed to xgboost
  param = list(objective = 'binary:logistic',
               eval_metric = 'logloss',
               eta = runif(1,.01,.3), 
               gamma = runif(1,0,0.6),
               subsample = runif(1,.6,.9),
               colsample_bytree = runif(1,.5,.9),
               min_child_weight = sample(1:40,1),
               max_delta_step = sample(1:10,1)
  )
  
  cv.nround = 50 #number of rounds (how many trees to aggregate and boost together)
  cv.nfold = 5 #number of folds in k-fold cross validation
  seed.number = sample.int(10000,1)[[1]] #switch random seeds so we dont trend on a false pattern due to RNG
  set.seed(seed.number)
  
  #run k-fold cross validation
  xgbcv = xgb.cv(data = dtrain, params = param, nthread = 6,
                 nfold = cv.nfold, nrounds = cv.nround,
                 verbose = T, early.stop.round = 5, maximize = FALSE)
  
  xgbcv_data = as.data.frame(xgbcv$evaluation_log)
  
  #use min logloss as a metric to weigh accuracy versus parsimony (maximum likelihood estimation framework)
  min_logloss = min(xgbcv_data$test_logloss_mean)
  min_logloss_index = which.min(xgbcv_data$test_logloss_mean)
  
  #if model fared better than others, save the params of that model
  if(min_logloss < best_logloss){
    best_logloss = min_logloss
    best_logloss_index = min_logloss_index
    best_seednumber = seed.number
    best_param = param
  }
}
}

# # TO FIND OPTIMAL NROUNDS ----------------------------------------------- (DIDNT END UP USING THIS, ALREADY INCORPORATED ABOVE)
{
# cv.nrounds = 1000
# testcv = xgb.cv(data=dtrain, params = best_param, nthread = 2, nfold = 2, nrounds = cv.nrounds)
# 
# testcv_data = as.data.frame(testcv$evaluation_log)
# min_logloss = min(testcv_data$test_logloss_mean)
# min_logloss_index = which.min(testcv_data$test_logloss_mean)
# min_logloss
# min_logloss_index


# # graph of logloss over nrounds. should look like a convex curve with an optimal minimum point. 
# # make sure nrounds is big enough for full shape to develop
# x = c(1:cv.nrounds)
# y = min_logloss
# qplot(x,y)
}

#XGBOOST MODEL BUILDING  ----------------------------------------------- 
{
#take optimized parameters from cross validation
params = best_param
nrounds = best_logloss_index
set.seed(best_seednumber)

#split data into test and training sets
index = 1:nrow(model_data)
test_index = sample(index, trunc(length(index) / 4))
train = list(x = data.matrix(model_data[-test_index, ] %>% select(-took_rec)),
             y = as.numeric(model_data$took_rec[-test_index]))
test = list(x = data.matrix(model_data[test_index, ] %>% select(-took_rec)),
            y = as.numeric(model_data$took_rec[test_index]))
dtrain = xgb.DMatrix(data = train$x, label = train$y)
dtest = xgb.DMatrix(data = test$x, label = test$y)
watchlist = list(train = dtrain, test = dtest)

#build xgboost model
working_model = xgb.train(params, nrounds = nrounds, dtrain, watchlist)
importance = xgb.importance(feature_names = dimnames(test$x)[[2]], model = working_model)
list('model' = working_model, 'importance' = importance)

#[FIGURE 4] #plot importance 
xgb.ggplot.importance(
  importance_matrix = importance,
  top_n = 10,
  meausre = NULL,
  rel_to_first = FALSE,
  n_clusters = c(1:10)
)
# xgb.plot.tree(model = working_model)


#PREDICTIONS AND ERROR ON TEST
label = getinfo(dtest, 'label')
pred = predict(working_model, dtest)
prediction = as.numeric(pred > 0.5)
err = mean(prediction != test$y)


#PREDICTIONS AND ERROR ON TRAIN
pred_train = predict(working_model, dtrain)
prediction_train = as.numeric(pred_train > 0.5)
err_train = mean(prediction_train != train$y)

full_test = model_data[test_index,]
full_train = model_data[-test_index,]

full_test$took_rec = NULL
full_train$took_rec = NULL


#RESULTS
full_test$pred = pred
full_test$prediction = prediction
full_test$real = test$y

full_train$pred = pred_train
full_train$prediction = prediction_train
full_train$real = train$y

#compile all results into output dataframe
output = rbind(full_test, full_train)
output$combined_result = as.factor(paste(output$prediction, 'versus', output$real))

#CONFUSION MATRIX (tells you how many right versus how many wrong)
summary(output$combined_result)
}


#WRITE OUTPUTS INTO CSV FILES
write.csv(output, file = '~/data challenge/output.csv')

#PRINTING OUT THE TREES
xgb.dump(model = working_model, '~/data challenge/tree_dump.csv', with_stats =  TRUE, fmap = 'xgb.fmap')

xgb.dump(working_model, 'xgb.model.dump', with_stats = TRUE)
head((xgb.dump(working_model, with_stats = TRUE, fmap = 'fmap.txt')),20)



