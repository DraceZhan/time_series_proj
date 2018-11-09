library(readr)
library(xts)
library(dygraphs)
library(tidyr)
library(forecast)
library(dplyr)
library(astsa)

air_reserve = read_csv('../random/mta.csv')

air_reserve %>% group_by(air_store_id) %>% tally(sort = T) %>% top_n(1)

air_reserve_top_rest = subset(air_reserve, air_store_id == (
  air_reserve %>% group_by(air_store_id) %>% tally(sort = T) %>% top_n(1))[[1]])

top_rest_xts = xts(
  air_reserve_top_rest$reserve_visitors, 
  order.by=as.POSIXct(air_reserve_top_rest$visit_datetime))