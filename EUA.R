library(EMD)
library(tidyverse)
library(tidyr)
library(reshape2)
library(forecast)
library(neuralnet)
library(pls)
library(strucchange)
library(vars)
library(lubridate)
library(ggthemes)

###########################################
# Data processing
###########################################
EUA_euro = read_csv('EUA_12-20.csv') %>%
  rename(price_euro = last_price)
EUA_euro <- EUA_euro[,2:3]

euro_usd = read_csv('euro-dollar.csv') %>%
  rename(rate = value)
EUA_usd = merge(EUA_euro,euro_usd)
EUA_usd$price_usd = EUA_usd$price_euro*EUA_usd$rate

EUA = EUA_usd[,c(1,4)]
n = nrow(EUA)

# write.csv(EUA,'EUA.csv')

ggplot(EUA, aes(date, price_usd)) +
  geom_line(color='#73808c') + 
  xlab('Time (12/2012 - 02/2020)') +
  ylab('EUA price (US$ per ton)') +
  ggtitle('European Emission Allowances (EUA) Price') +
  theme_clean()

EUA_train = EUA[1:1700,]
EUA_test = EUA[1701:1814,]
n_train = nrow(EUA_train)
n_test = nrow(EUA_test)

ggplot(EUA_train, aes(x = date)) +
  geom_line(aes(y=price_usd))

###########################################
# EMD 
###########################################

try <- emd(EUA_train$price_usd, EUA_train$date, boundary="none")
imfs = as.data.frame(try$im)
imfs = imfs %>% add_column(residue = try$residue,
                           date = EUA_train$date, price = EUA_train$price_usd) %>%
  rename(imf1 = V1,
         imf2 = V2,
         imf3 = V3,
         imf4 = V4,
         imf5 = V5,
         imf6 = V6,
         imf7 = V7)

imfs_adj = melt(subset(imfs,select=c(date,imf1,imf2,imf3,imf4,imf5,imf6,imf7,residue,price)),id.var="date")
ggplot(imfs_adj, aes(x = date, y = value)) +
  geom_line(aes(color = variable), show.legend = F) +
  facet_grid(variable ~ ., scales = "free_y") +
  xlab('Time') +
  ylab('Value') +
  ggtitle('Emperical Mode Decomposition')

###########################################
# Fine-to-coarse reconstruction
###########################################

zeros = rep(0,n_train)
s1_avg = imfs$imf1
t.test(zeros,s1_avg)
s2_avg = (imfs$imf1+imfs$imf2)/2
t.test(zeros,s2_avg)
s3_avg = (imfs$imf1+imfs$imf2+imfs$imf3)/3
t.test(zeros,s3_avg)
s4_avg = (imfs$imf1+imfs$imf2+imfs$imf3+imfs$imf4)/4
t.test(zeros,s4_avg)
s5_avg = (imfs$imf1+imfs$imf2+imfs$imf3+imfs$imf4+imfs$imf5)/5
t.test(zeros,s5_avg)
s6_avg = (imfs$imf1+imfs$imf2+imfs$imf3+imfs$imf4+imfs$imf5+imfs$imf6)/6
t.test(zeros,s6_avg)

imfs$high_f = imfs$imf1 + imfs$imf2 + imfs$imf3 + imfs$imf4
imfs$low_f = imfs$imf6 + imfs$imf5 + imfs$imf7
imfs$trend = imfs$residue
ggplot(imfs, aes(x = date)) +
  geom_line(aes(y = price, color = 'price')) +
  geom_line(aes(y = trend, color = 'trend')) +
  geom_line(aes(y = high_f, color = 'high frequency')) +
  geom_line(aes(y = low_f, color = 'low frequency')) +
  scale_color_manual("", breaks=c('price','trend','high frequency','low frequency'), values = c('#e3e6e8','#70a970','#73808c','#ff8c66')) +
  xlab('Time') +
  ylab('EUA price (US$ per ton)') +
  ggtitle('European Emission Allowances (EUA) Price') +
  theme_clean()
  

imfs_adj2 = melt(subset(imfs,select=c(date,high_f,low_f,trend,price)),id.var="date")
ggplot(imfs_adj2, aes(x = date, y = value)) +
  geom_line(aes(color = variable), show.legend = F) +
  facet_grid(variable ~ ., scales = "free_y")

prediction = read_csv('prediction.csv') %>%
  mutate(date = as.Date(date,format='%d/%m/%y'))
ggplot(prediction, aes(x = date)) +
  geom_line(aes(y = trend_pred, color = 'trend')) +
  geom_line(aes(y = high_f_pred, color = 'high frequency')) +
  geom_line(aes(y = low_f_pred, color = 'low frequency')) +
  scale_color_manual("", breaks=c('trend','high frequency','low frequency'), values = c('#e3e6e8','#70a970','#ff8c66')) +
  xlab('Time') +
  ylab('Value (US$ per ton)') +
  ggtitle('Prediction of high frequency, low frequency and trend') +
  theme_clean()

###########################################
# Prediction
###########################################

low_fit = imfs$low_f %>% auto.arima(stationary = F, 
                                    seasonal = T, 
                                    test = c("kpss", "adf", "pp"),  
                                    ic = c("aicc", "aic", "bic"), 
                                    allowdrift = T, 
                                    allowmean = T, 
                                    approximation = T,
                                    trace = F)
low_prediction = forecast(low_fit,h=n_test)
# plot(low_prediction)
low_future = low_prediction$mean


high_fit = imfs$high_f %>% auto.arima(stationary=F,
                                      seasonal = T,
                                      test = c("kpss", "adf", "pp"),
                                      ic = c("aicc", "aic", "bic"),
                                      allowdrift = F,
                                      allowmean = T,
                                      approximation = T,
                                      trace = F)
high_prediction = forecast(high_fit,h=n_test)
# plot(high_prediction)
high_future = high_prediction$mean

trend_fit2 <- lm(y~poly(x, 2), data.frame(x=1:1700, y=imfs$residue))
trend_prediction2 <- predict(trend_fit2, data.frame(x=1701:1814), se=T)
trend2 = trend_prediction2$fit
# plot(trend2)

ARIMA_future = low_future+high_future+trend2

model = data.frame(cbind(EUA_test$date, trend2, low_future,high_future)) %>%
  rename(date = EUA_test.date,
         trend = trend2,
         high_frequency = high_future,
         low_frequency = low_future)
model$date = as.Date(model$date)
model$prediction = ARIMA_future
model = melt(subset(model,select=c(date, trend, low_frequency,high_frequency, prediction)),id.var="date")
model %>% ggplot(aes(x=date,y=value)) +
  geom_line(aes(color=variable), show.legend = F) +
  facet_grid(variable ~ ., scales = "free_y") +
  xlab('Time (09/2019 - 02/2020)') +
  ylab('Predicted value (US$)') + 
  ggtitle('Carbon Price Prediction Sep 2019 - Feb 2020')


past = EUA_train$price_usd
EUA$ARIMA = c(past, ARIMA_future)
EUA %>% ggplot(aes(x=date)) +
  geom_line(aes(y=EUA$ARIMA, color='prediction')) +
  geom_line(aes(y=EUA$price_usd, color='actual')) +
  scale_color_manual("", breaks=c('prediction','actual'), values = c('#73808c','#ff8c66')) +
  theme(legend.position = 'bottom') +
  xlab('Time') +
  ylab('EUA price (US$ per ton)') +
  ggtitle('EUA Sep 2019 - Feb 2020: Actual Price and Prediction') +
  theme_clean()

###########################################
# short term prediction
###########################################

time = read.csv('date.csv')
time = time[1:2594,1] 
time = parse_date_time(time, 'AdBY')
time = data.frame(time) %>% rename(date=time)
short_time = time[1:1879,]
short = data.frame(short_time) %>% rename(date = short_time)

short_high = forecast(high_fit,h=n_test+65)
short_high = short_high$mean
short_low = forecast(low_fit,h=n_test+65)
short_low = short_low$mean
short_trend = predict(trend_fit2, data.frame(x=1706:1884), se=T)
short_trend = short_trend$fit
short_future = short_high + short_low + short_trend
short$price = c(past,short_future)
short$actual = c(EUA$price_usd,repeat(NA))
short %>% ggplot(aes(x=date,y=price)) +
  geom_line(color='#73808c')+
  xlab('Time (12/2012 - 04/2020)') +
  ylab('EUA price (US$ per ton)') +
  ggtitle('Carbon Price Short Term Prediction') +
  theme_clean() 


###########################################
# long term prediction
###########################################

long_high = forecast(high_fit,h=n_test+780)
long_high = long_high$mean
long_low = forecast(low_fit,h=n_test+780)
long_low = long_low$mean
long_trend = predict(trend_fit2, data.frame(x=1706:2599), se=T)
long_trend = long_trend$fit
long_future = long_high + long_low + long_trend
time$price = c(past,long_future)
time %>% ggplot(aes(x=date)) +
  geom_line(aes(y=price), color='#73808c') + 
  xlab('Time (12/2012 - 01/2023)') +
  ylab('EUA price (US$ per ton)') +
  ggtitle('Carbon Price Prediction') +
  theme_clean()

###########################################
# Carbon VaR
###########################################

correlation = read_csv('correlation.csv') %>% janitor::clean_names()
correlation$date = as.Date(correlation$date, format='%m/%d/%Y')
correlation = merge(correlation,euro_usd)
correlation = correlation %>%
  mutate(eua = eua*rate,
         basic_resources = basic_resources*rate,
         industrial = industrial*rate,
         retail = retail*rate,
         technology = technology*rate,
         utilities = utilities*rate)
correlation = correlation[,c(1,2,4,7,8,9,11)]

eua_mean = mean(correlation$eua)
eua_sd = sd(correlation$eua)
resources_mean = mean(correlation$basic_resources)
resources_sd = sd(correlation$basic_resources)
industrial_mean = mean(correlation$industrial)
industrial_sd = sd(correlation$industrial)
retail_mean = mean(correlation$retail)
retail_sd = sd(correlation$retail)
technology_mean = mean(correlation$technology)
technology_sd = sd(correlation$technology)
utilities_mean = mean(correlation$utilities)
utilities_sd = sd(correlation$utilities)

correlation_origin = correlation

correlation = correlation %>%
  mutate(eua = scale(eua),
         basic_resources = scale(basic_resources),
         industrial = scale(industrial),
         retail = scale(retail),
         technology = scale(technology),
         utilities = scale(utilities))

r_resources = cor(correlation$eua,correlation$basic_resources)
r_industrial = cor(correlation$eua,correlation$industrial)
r_retail = cor(correlation$eua,correlation$retail)
r_technology = cor(correlation$eua,correlation$technology)
r_utilities = cor(correlation$eua,correlation$utilities)

eua_short = (26.8787 - eua_mean)/eua_sd - 1.5209
eua_long = (74.0934 - eua_mean)/eua_sd - 1.5209
resources_short = 
  (r_resources*eua_short - 2.0383)*resources_sd + resources_mean
resources_long = 
  (r_resources*eua_long - 2.0383)*resources_sd + resources_mean
industrial_short = 
  (r_industrial*eua_short - 1.8818)*industrial_sd + industrial_mean
industrial_long = 
  (r_industrial*eua_long - 1.8818)*industrial_sd + industrial_mean
retail_short = 
  (r_retail*eua_short - 2.0875)*retail_sd + retail_mean
retail_long = 
  (r_retail*eua_long - 2.0875)*retail_sd + retail_mean
technology_short = 
  (r_technology*eua_short - 1.3597)*technology_sd + technology_mean
technology_long = 
  (r_technology*eua_long - 1.3597)*technology_sd + technology_mean
utilities_short = 
  (r_utilities*eua_short - 0.5063)*utilities_sd + utilities_mean
utilities_long = 
  (r_utilities*eua_long - 0.5063)*utilities_sd + utilities_mean

resources_short_change = resources_short/correlation_origin[153,3]-1
resources_short_change
resources_long_change = resources_long/correlation_origin[153,3]-1
resources_long_change
industrial_short_change = industrial_short/correlation_origin[153,4]-1
industrial_short_change
industrial_long_change = industrial_long/correlation_origin[153,4]-1
industrial_long_change
retail_short_change = retail_short/correlation_origin[153,5]-1
retail_short_change
retail_long_change = retail_long/correlation_origin[153,5]-1
retail_long_change
technology_short_change = technology_short/correlation_origin[153,6]-1
technology_short_change
technology_long_change = technology_long/correlation_origin[153,6]-1
technology_long_change
utilities_short_change = utilities_short/correlation_origin[153,7]-1
utilities_short_change
utilities_long_change = utilities_long/correlation_origin[153,7]-1
utilities_long_change


sector = c(correlation_origin[153,2],correlation_origin[153,3],correlation_origin[153,4],correlation_origin[153,5],correlation_origin[153,6],correlation_origin[153,7])
sector_short = c(26.8787,resources_short,industrial_short,retail_short,technology_short,utilities_short)
sector_short_change = c(26.8787/correlation_origin[153,2]-1,resources_short_change,industrial_short_change,retail_short_change,technology_short_change,utilities_short_change)
sector_long = c(74.0934,resources_long,industrial_long,retail_long,technology_long,utilities_long)
sector_long_change = c(74.0934/correlation_origin[153,2]-1,resources_long_change,industrial_long_change,retail_long_change,technology_long_change,utilities_long_change)
a = as.data.frame(cbind(sector,sector_short,sector_short_change,sector_long,sector_long_change))
