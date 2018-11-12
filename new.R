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

air_reserve = read_csv("C:/Users/Drace/Desktop/time_series_proj-master/air_visit_data.csv")
air_reserve = air_reserve %>% group_by(as.Date(air_reserve$visit_date)) %>% summarise(visitors = sum(visitors))
colnames(air_reserve)[1] = 'Date'

#checking variance
qqnorm(air_reserve$visitors)
#observes skew
bc_lambda = BoxCox.lambda(air_reserve$visitors)
air_reserve$bc_visitors = BoxCox(air_reserve$visitors, lambda = bc_lambda)

air_reserve_holdout = tail(air_reserve, n = 30)
air_reserve = head(air_reserve, n =448)
air_reserve_xts = xts(air_reserve, order.by = air_reserve$Date)

#Visuals
dygraph(air_reserve_xts[,c(2,3)]) %>% dySeries('visitors', label = 'Total Visitors')  %>%dySeries(
  'bc_visitors', label = 'Box-Cox Transformed Total Visitors') %>% dyRangeSelector(
    height = 20) %>%dyOptions(
    logscale=F) %>%
  dyHighlight(highlightSeriesOpts = list(strokeWidth = 1))
  
  
dygraph(air_reserve_xts[,c(3)]) %>% dySeries(
    'bc_visitors', label = 'Box-Cox Transformed Total Visitors') %>%dyRangeSelector(height = 20) %>%
    dyHighlight(highlightSeriesOpts = list(strokeWidth = 1)) %>% dyOptions(logscale=F)

dygraph(visitors, main = "Rolling Five Days") %>% 
  dyRoller(rollPeriod = 5)

#acf/pacf plots
air_reserve_ts = ts(air_reserve)
acf(air_reserve_ts[,c(3)], main=ACF)
pacf(air_reserve_ts[,c(3)], main = PACF)

#acf shows weekly seasonality, we can further verify this below
#The power spectrum is the discrete Fourier transform of the autocovariance function of an appropriately smoothed version of the original series, we use a periodogram to examine if there is any seasonality here

p = periodogram(
  air_reserve_ts[,c(2)],log='yes',plot=TRUE,ylab="Periodogram", xlab="Frequency",lwd=2)
dd = data.frame(freq=p$freq, spec=p$spec)
order = dd[order(-dd$spec),]
top5 = head(order, 5)
time_conv = 1/top5$freq
time_conv

#tests on non transformed time series
adf.test(air_reserve_ts[,c(2)])
kpss.test(air_reserve_ts[,c(3)])

#fail to reject the null

#seasonality was detected for increments of 120 days, roughly 1/3rd of a year. Also weekly seasonality was detected as well

#taking difference at lag 7
ts_diff = diff(air_reserve_ts, differences = 1, lag = 7)
acf(ts_diff[,c(3)], main='ACF')
pacf(ts_diff[,c(3)], main='PACF')
#Augmented Dickey-Fuller for evaluation of this time series
adf.test(ts_diff[,c(3)])
kpss.test(ts_diff[,c(3)])
#the p-value states that there is evidence this time series is stationary


#I suspect AR(1) will already
ar_max = ar(ts_diff[,c(3)], method ='yule-walker', aic=T, order.max=NULL)
ar_max$aic
plot(ar_max$aic, xlab = 'Orders', ylab = 'AIC', main = 'AIC Orders')

#summary(ar_max)
#Evaluating the model with AIC, we have suggestions for order 8 and 15 as simpler models with good performance

#Testing a few models to evaluate differences
ar_1 = ar(ts_diff[,c(3)], order.max = 1, method ='yule-walker', aic = F)
ar_8 = ar(ts_diff[,c(3)], order.max = 8,  method ='yule-walker', aic = F)

#Plots to evaluate whether all inverse roots lie within the unit circle:
autoplot(ar_max)
autoplot(ar_1)
autoplot(ar_2)

#model evaluations
#Variance explained by models
1- ar_max$var.pred
1- ar_1$var.pred
1- ar_8$var.pred

#Running Box-Ljung-Pierce test to evaluate whether residuals:
Box.test(ar_max$resid)
Box.test(ar_1$resid)
Box.test(ar_8$resid)

ggplot(ar_max$resid, aes(x=as.numeric(ar_max$resid))) + geom_density(
  alpha =.2) + geom_histogram(fill='lightblue', aes(y=..density..), alpha=.5) +labs(
    title="Distribution of Residuals",x="Residual Value", y = "Density")

resids_max_df = as.data.frame(ar_max$resid)
resids_1 = as.data.frame(ar_1$resid)
resids_8 = as.data.frame(ar_8$resid)
#residuals of all three have evidence to suggest they are not white noise according to Box-Ljung-Pierce test


#experimenting with sarima code but having trouble understanding results using sarima 
arima_max = sarima(air_reserve_ts[,c(3)], p = 15,d = 1, q = 0, P=0,D=1,Q=1,S =7)
arima_1 = sarima(air_reserve_ts[,c(3)], p = 1,d = 1, q = 0, P=0,D=1,Q=1,S =7)
arima_8 = sarima(air_reserve_ts[,c(3)], p = 8,d = 1, q = 0, P=0,D=1,Q=1,S =7)



#Inverse Box-Cox
invBoxCox <- function(x, lambda)
  if (lambda == 0) exp(x) else (lambda*x + 1)^(1/lambda)

fore_max=predict(arima_max$fit, 30)
preds_ <- InvBoxCox(fore_max$pred, bc_lambda)
xts_holdout <- xts(cbind(air_reserve_holdout, as.numeric(preds_)), order.by = air_reserve_holdout$Date)
pred_errors = as.data.frame(air_reserve_holdout$visitors-preds_)


pred_errors = as.data.frame(air_reserve_holdout$bc_visitors-fore_max$pred)
ggplot(pred_errors) + geom_point(aes(x = seq_along(pred_errors$x), y=abs(as.numeric(pred_errors$x))))+ labs(
  title = "Forecast Errors", x='Forward Delta', y= 'Error')                                 