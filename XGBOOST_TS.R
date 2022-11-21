library(forecast)
library(fpp)
library(dplyr)
library(tidymodels)
library(stringr)
library(lubridate)
library(zoo)
library(workflows)
library(parsnip)
library(recipes)
library(yardstick)
library(glmnet)
library(tidyverse)
library(tidyquant)
library(timetk)

setwd("D:\\Coding_Projects\\TS-EnergyForecast\\")
energy=read.csv("train.csv")
test=read.csv('test.csv')
summary(energy)
summary(test)

round(colMeans(is.na(energy)),2) #2% of energy is missing
round(colMeans(is.na(test)),2)  # no missing values

#feature engineering of datetime
test$energy=NA
energy$data='train' #creating placeholders
test$data='test' 
table=rbind(energy,test) 

table = table %>%  
  mutate(datetime=as_datetime(datetime))
glimpse(table)
table=table %>% 
  mutate(month=month(datetime),
         year=year(datetime),
         day_of_month=mday(datetime),
         day_of_year=yday(datetime),
         day_of_week=wday(datetime),
         hour=hour(datetime))
round(colMeans(is.na(table)),2) #2% of energy is missing

#imputing missing values by imputing neighbouring points
table=table %>% 
  tidyr::fill(energy,month,year,day_of_month,day_of_year,day_of_week,hour,.direction='downup') #imputing missing values from neihbouring points
round(colMeans(is.na(table)),2) #checking it

#separating to train and test and removing datetime and rowid
energy=table %>% filter(data=='train') %>% select(-data,-datetime,-row_id)
test=table %>% filter(data=='test') %>% select(-data,-energy,-datetime,-row_id)

#checking if datetime is continous or not
#time_stamps=seq(ymd_hms('2008-03-01 00:00:00'),
                #ymd_hms('2018-12-31 23:00:00'),
                #by='1 hour') # 94992 obs = same as datetime

#plot(zoo(energy$energy,order.by=energy$datetime))



#ML doesnt use seq data. but with lag columns, we can use ML models
# making lag features from response variable
for (i in c(1,2,3,4,24,25,26,27,48,49,50,51)){
  name=paste0('energy_lag_',i)
  energy[,name]=lag(energy$energy,i)
}

#spliitng into train and validation 
energy_train=energy[1:(94992-10000),] #arbitary number 10,000
energy_test=energy[(94992-10000):94992,]
round(colMeans(is.na(energy_test)),2)
energy_train=energy_train %>% na.omit() #ommiting the missing values due to lag

xgb_spec=boost_tree(
  trees=500, #better value = 1000. longer time
  tree_depth=tune(),
  min_n=tune(),
  loss_reduction=tune(),
  sample_size=tune(),
  mtry=tune(),
  learn_rate=tune(),) %>% 
  set_engine("xgboost") %>% 
  set_mode("regression")

xgb_grid=grid_latin_hypercube(
  tree_depth(),
  min_n(),
  loss_reduction(),
  sample_size=sample_prop(),
  finalize(mtry(),energy_train[,1:19]), #no. of features except datetime and response
  learn_rate(),
  size=10) # number of combinations created. better number is 30-50

xgb_grid

xgb_wf=workflow() %>% 
  add_formula(energy~.) %>%
  add_model(xgb_spec)
xgb_wf

set.seed(123)
energy_folds = vfold_cv(energy_train[,1:19],v=5) #v=10 ideal choice

set.seed(234)
xgb_res=tune_grid(
  xgb_wf,
  resamples=energy_folds,
  grid=xgb_grid,
  control=control_grid(verbose=TRUE)
)

collect_metrics(xgb_res)

xgb_res %>% 
  collect_metrics() %>% 
  filter(.metric=="rmse") %>% 
  select(mean,mtry:sample_size) %>% 
  pivot_longer(mtry:sample_size,
               values_to="value",
               names_to = "parameter"
  )%>% 
  ggplot(aes(value,mean,color=parameter)) +
  geom_point(alpha=0.8,show.legend = FALSE)+
  facet_wrap(~parameter,scales='free_x')+
  labs(x=NULL , y='rmse')

show_best(xgb_res,"rmse")

best_rmse=select_best(xgb_res,'rmse') # for model 10. increase vfolds
best_rmse


final_xgb = finalize_workflow(
  xgb_wf,
  best_rmse
)

final_xgb

library(vip)
final_xgb_fit=final_xgb %>% 
  fit(data=energy_train[,1:19]) %>% 
  pull_workflow_fit()

final_xgb_fit %>%  vip(geom="point") # checks feature importance 

test_forecast_xgb=predict(final_xgb_fit,new_data=energy_test[,2:19])

rmse_xgb=mean((energy_test$energy - test_forecast_xgb$.pred)^2) %>% sqrt()
rmse_xgb #RMSE:53.63 for 19variables

#### final prediction on test set #######
test_pred=predict(final_xgb_fit,new_data = test) %>% select(.pred_1)
colnames(test_pred)='energy'
write.csv(test_pred,'Submission.csv',row.names = F)

future_predictions_tbl <- xgb_wf %>% 
  fit(data = energy) %>%
  predict(test) %>%
  bind_cols(test)

# Extract bikes index
idx <- energy_train %>% tk_index()

idx_future <- idx %>% tk_make_future_timeseries(n_future = 180)
future_tbl <- tibble(date = idx_future) 
future_tbl

future_predictions_tbl <- xgb_wf %>% 
  fit(data = bikes_tbl) %>%
  predict(future_tbl) %>%
  bind_cols(future_tbl)