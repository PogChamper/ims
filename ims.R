# includes data from a lot of time series textbooks
library(tsdl)

#choose 3 timeseries from tstl with frequency = 4 which will be Y, s, B and R vectors 
tsdl_daily_365 <- subset(tsdl, 4)
truex <- tsdl_daily_365[[1]]
truex2 <- tsdl_daily_365[[2]]
truex3 <- tsdl_daily_365[[3]]
truex4 <- tsdl_daily_365[[5]]

# choose the forecasting horizon
h = 4

#call the forecasting function
source("forecmodule.r")

# call the optimization function
source("optmodule.r")

# determination of the stock balance in the initial moment of time 
theta = 500

# loop that simulate a real-time forecasting process
for (i in 1:4) {
  # on every step of our loop we add 4 values in predictable time series, 
  # its like updating information in real world
  r1 <- head(truex, 48 + i*4)
  r2 <- head(truex2, 48 + i*4)
  r3 <- head(truex3, 48 + i*4)
  r4 <- head(truex4, 48 + i*4)
  
  
  r1 <- ts(r1, frequency = 4)
  r2 <- ts(r2, frequency = 4)
  r3 <- ts(r3, frequency = 4)
  r4 <- ts(r4, frequency = 4)
  
  #using our forecasting function
  k = forecmodule(h = h, r1 = r1, r2 = r2, r3 = r3, r4 = r4)
  
  y_pred <- round(k[[1]])
  s_pred <- 90*k[[2]]
  b_pred = k[[3]]
  r_pred <- k[[4]]/160
  
  print(k[[1]]) #y
  print(k[[2]]) #s
  print(k[[3]]) #b
  print(k[[4]]) #r
  
  #using our optimization function
  sol = optmodule(y_pred = y_pred, s = s_pred, r = r_pred, b = b_pred, theta = theta)
  theta = sol[[2]]
  
  #print output in as [x1, ... xh, y1, ... yh]
  print(sol[[1]])
  
  #plotting graphs
  plot(ts(r1, frequency = 1), type = "l", main = "Demand forecasting", xlab = "days", ylab = "volume of solid")
  lines(c(ts(k[[5]], frequency = 1), k[[1]]), col = "blue")
  lines(ts(r1, frequency = 1))
  
  plot(ts(r2, frequency = 1), type = "l", main = "Selling cost forecasting", xlab = "days", ylab = "cost")
  lines(c(ts(k[[6]], frequency = 1), k[[2]]), col = "blue")
  lines(ts(r2, frequency = 1))
  
  plot(ts(r3, frequency = 1), type = "l", main = "Purchase cost forecasting", xlab = "days", ylab = "cost")
  lines(c(ts(k[[7]], frequency = 1), k[[3]]), col = "blue")
  lines(ts(r3, frequency = 1))
  
  plot(ts(r4, frequency = 1), type = "l", main = "Storage cost forecasting", xlab = "days", ylab = "cost")
  lines(c(ts(k[[8]], frequency = 1), k[[4]]), col = "blue")
  lines(ts(r4, frequency = 1))

}

