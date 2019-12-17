# train the learning model
library(xgboost)

# M4metalearning package contains the functions 
# for training new models and making the predictions
library(M4metalearning)

# the M4metaresults package is just a 'data' package containing the trained model
library(M4metaresults)

# forecasting function for Y_, S_, B_, R_
forecmodule <- function(h, r1, r2, r3, r4) {
  
  # we subtract the last h observations to use it as 'true future' values
  # and keep the rest as the input series in our method
   x <- head(r1, -h) 
  x2 <- head(r2, -h)
  x3 <- head(r3, -h)
  x4 <- head(r4, -h)
  
  x <- ts(x, frequency = 4)
  x2 <- ts(x2, frequency = 4)
  x3 <- ts(x3, frequency = 4)
  x4 <- ts(x4, frequency = 4)
  
  #forecasting with FFORMA using pretrained model 
  #just the input series and the desired forecasting horizon
  forec_result <- forecast_meta_M4(model_M4, x, h = h)
  forec_result2 <- forecast_meta_M4(model_M4, x2, h = h)
  forec_result3 <- forecast_meta_M4(model_M4, x3, h = h)
  forec_result4 <- forecast_meta_M4(model_M4, x4, h = h)
  
  # actual is real values os time series
  # predicted is output of forecast_meta_M4
  actual <- tail(truex, h)
  predicted <- forec_result$mean
  
  actual2 <- tail(truex2, h)
  predicted2 <- forec_result2$mean
  
  actual3 <- tail(truex3, h)
  predicted3 <- forec_result3$mean
  
  actual4 <- tail(truex4, h)
  predicted4 <- forec_result4$mean
  
  # form the list of outputs of our function, that contains predicted values 
  # and original time series (for plotting graphs)
  pred = list(predicted, predicted2, predicted3, predicted4, x, x2, x3, x4
  )
  
  return(pred)
}
