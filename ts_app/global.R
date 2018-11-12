library(readr)
library(xts)
library(dygraphs)
library(tidyr)
library(forecast)
library(dplyr)
library(astsa)
library(tseries)
library(TSA)
library(ggplot2)
library(Metrics)
library(shinydashboard)
library(zoo)
library(DT)

#ggplot2 theme
theme_set(theme_bw())

#setting Rstudio margins to avoid errors from sarima
par(mar=c(1,1,1,1))

air_reserve = read_csv("data/air_visit_data.csv")
air_reserve_full = air_reserve %>% group_by(as.Date(air_reserve$visit_date)) %>% summarise(visitors = sum(visitors))
colnames(air_reserve_full)[1] = 'Date'

#data aggregation
bc_lambda = BoxCox.lambda(air_reserve_full$visitors)
air_reserve_full$bc_visitors = BoxCox(air_reserve_full$visitors, lambda = bc_lambda)
air_reserve_holdout = tail(air_reserve_full, n = 30)
air_reserve = head(air_reserve_full, n =448)
air_reserve$rolling_week = rollmean(zoo(air_reserve$visitors), 7, fill = NA*7)

#Note the data has a "holiday anomalies in that must be investigated further, large growth from July 2016, large dip in Jan 2017)



air_reserve_xts = xts(air_reserve, order.by = air_reserve$Date)

air_reserve_ts = ts(air_reserve)
ts_diff = diff(air_reserve_ts, differences = 1, lag = 7)



#acf shows weekly seasonality, we can further verify this below
#The power spectrum is the discrete Fourier transform of the autocovariance function of an appropriately smoothed version of the original series, we use a periodogram to examine if there is any seasonality here


p = periodogram(
  air_reserve_ts[,c(2)],log='yes',plot=TRUE,ylab="Periodogram", xlab="Frequency",lwd=2)
dd = data.frame(freq=p$freq, spec=p$spec)
order = dd[order(-dd$spec),]
top5 = head(order, 5)
time_conv = 1/top5$freq

X = ts_diff[,c(3)]

ar_max = ar(ts_diff[,c(3)], method ='yule-walker', aic=T, order.max=NULL)


#experimenting with sarima code but having trouble understanding results using sarima 
#arima_max = sarima(air_reserve_ts[,c(3)], p = 15,d = 1, q = 0, P=0,D=1,Q=1,S =7)
#arima_1 = sarima(air_reserve_ts[,c(3)], p = 1,d = 1, q = 0, P=0,D=1,Q=1,S =7)
#arima_8 = sarima(air_reserve_ts[,c(3)], p = 8,d = 1, q = 0, P=0,D=1,Q=1,S =7)

arima_max <-sarima(air_reserve_ts[,c(3)], p = 15,d = 1, q = 0, P=0,D=1,Q=1,S =7, Model = F, details =F)

#Inverse Box-Cox
invBoxCox <- function(x, lambda)
  if (lambda == 0) exp(x) else (lambda*x + 1)^(1/lambda)

fore_max=predict(arima_max$fit, 30)
preds_ <- InvBoxCox(fore_max$pred, bc_lambda)
xts_holdout <- xts(cbind(air_reserve_holdout, as.numeric(preds_)), order.by = air_reserve_holdout$Date)
pred_errors = as.data.frame(air_reserve_holdout$visitors-preds_)
