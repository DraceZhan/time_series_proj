library(readr)
library(xts)
library(dygraphs)
library(tidyr)
library(forecast)
library(dplyr)
library(astsa)
library(tseries)
library(TSA)

air_reserve = read_csv("C:/Users/Drace/Desktop/time_series_proj-master/air_visit_data.csv")
air_reserve = air_reserve %>% group_by(as.Date(air_reserve$visit_date)) %>% summarise(visitors = sum(visitors))
colnames(air_reserve)[1] = 'Date'
BoxCox.lambda(air_reserve$visitors)
air_reserve$bc_visitors = BoxCox(air_reserve$visitors, lambda = 0)
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
acf(air_reserve_ts[,c(3)])
pacf(air_reserve_ts[,c(3)])

#acf shows weekly seasonality, we can further verify this below
#The power spectrum is the discrete Fourier transform of the autocovariance function of an appropriately smoothed version of the original series, we use a periodogram to examine if there is any seasonality here

p = periodogram(
  air_reserve_ts[,c(2)],log='yes',plot=TRUE,ylab="Periodogram", xlab="Frequency",lwd=2)
dd = data.frame(freq=p$freq, spec=p$spec)
order = dd[order(-dd$spec),]
top5 = head(order, 5)
time_conv = 1/top5$freq
time_conv

#seasonality was detected for increments of 120 days, roughly 1/3rd of a year. Also weekly seasonality was detected as well

#taking difference at lag 7
ts_diff = diff(air_reserve_ts, differences = 1, lag = 7)
acf(ts_diff[,c(3)])
pacf(ts_diff[,c(3)])
