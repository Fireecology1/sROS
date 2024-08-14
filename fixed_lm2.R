#Fixed form modelling
#Use to force a coefficient

#E.g., use for 25% surface ROS model
# ROS = 0.25 * ws
# sample data (actual sROS data) included in folder

#Manual code for model evaluation functions
rmse.manual <- function(actual, pred) {
  sqrt(mean(actual-pred)^2)
}

mae.manual <- function(actual, pred) {
  mean(abs(actual-pred))
}

mape.manual <- function(actual, pred) {
  mean(abs((actual-pred)/actual))
}

ef.r.squared <- function(actual, pred) {
  1- sum((actual - pred)^2) / sum((actual - mean(actual))^2)
}

aic.manual <- function(model) {
  n=length(model$residuals)
  rss=sum(model$residuals^2)
  k=length(coef(model)) + 1
  n* log(rss/n) + 2 * k
}

#save(rmse.manual, file='C:/Dan/_Fire_tools/r_scripts_functions/rmse.rda')
#save(mae.manual, file='C:/Dan/_Fire_tools/r_scripts_functions/mae.rda')
#save(mape.manual, file='C:/Dan/_Fire_tools/r_scripts_functions/mape.rda')
#save(rmse.manual, file='C:/Dan/_Fire_tools/r_scripts_functions/rmse.rda')
#save(ef.r.squared, file='C:/Dan/_Fire_tools/r_scripts_functions/Efr2.rda')
#save(aic.manual, file='C:/Dan/_Fire_tools/r_scripts_functions/aic.rda')

#e.g. mae.manual(actual=df.m25$ROS, pred=df.m25$pred25)