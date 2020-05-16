library(EMD)
library(tidyverse)
library(reshape2)
library(forecast)
library(neuralnet)
library(pls)
library(strucchange)
library(vars)

###########################################
# Data processing
###########################################
EUA_euro = read_csv('EUA_18-20.csv') %>%
  rename(date = Time,
         price_euro = Last)
n = nrow(EUA_euro)-1
EUA_euro <- EUA_euro[n:1,c(1,5)]
EUA_euro$date <- as.Date(EUA_euro$date, format = '%m/%d/%y')

euro_usd = read_csv('euro-dollar.csv') %>%
  rename(rate = value)
EUA_usd = merge(EUA_euro,euro_usd)
EUA_usd$price_usd = EUA_usd$price_euro*EUA_usd$rate

EUA = EUA_usd[,c(1,4)]
# write.csv(EUA,'EUA.csv')

ggplot(EUA, aes(date, price_usd)) +
  geom_line()

EUA_train = EUA[1:490,]
EUA_test = EUA[491:515,]
n_train = nrow(EUA_train)
n_test = nrow(EUA_test)

ggplot(EUA_train, aes(date, price_usd)) +
  geom_line()

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
         imf5 = V5)

imfs_adj = melt(subset(imfs,select=c(date,imf1,imf2,imf3,imf4,imf5,residue,price)),id.var="date")
ggplot(imfs_adj, aes(x = date, y = value)) +
  geom_line(aes(color = variable), show.legend = F) +
  facet_grid(variable ~ ., scales = "free_y")

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

imfs$high_f = imfs$imf1 + imfs$imf2 + imfs$imf3
imfs$low_f = imfs$imf4 + imfs$imf5
imfs$trend = imfs$residue
ggplot(imfs, aes(x = date)) +
  geom_line(aes(y = high_f), linetype = 'dotted') +
  geom_line(aes(y = low_f), linetype = 'longdash') +
  geom_line(aes(y = trend), linetype = 'dashed') +
  geom_line(aes(y = price)) +
  labs(y = 'price (USD)')

imfs_adj2 = melt(subset(imfs,select=c(date,high_f,low_f,trend,price)),id.var="date")
ggplot(imfs_adj2, aes(x = date, y = value)) +
  geom_line(aes(color = variable), show.legend = F) +
  facet_grid(variable ~ ., scales = "free_y")

###########################################
# ARIMA prediction
###########################################

low_fit = imfs$low_f %>% auto.arima(stationary=F, 
                                seasonal = T, 
                                test = c("kpss", "adf", "pp"),  
                                ic = c("aicc", "aic", "bic"), 
                                allowdrift = T, 
                                allowmean = F, 
                                approximation = T,
                                trace = T)
low_prediction = forecast(low_fit,h=n_test)
# plot(low_prediction)
low_future = low_prediction$mean

high_fit = imfs$high_f %>% auto.arima(stationary=F,
                                    seasonal = T,
                                    test = c("kpss", "adf", "pp"),
                                    ic = c("aicc", "aic", "bic"),
                                    allowdrift = T,
                                    allowmean = T,
                                    approximation = T,
                                    trace = T)
high_prediction = forecast(high_fit,h=n_test)
# plot(high_prediction)
high_future = high_prediction$mean

# trend_fit = imfs$trend %>% auto.arima(stationary=F,
#                                       seasonal = F,
#                                       test = c("kpss", "adf", "pp"),
#                                       ic = c("aicc", "aic", "bic"),
#                                       allowdrift = T,
#                                       allowmean = T,
#                                       approximation = T,
#                                       trace = T)
# trend_prediction = forecast(trend_fit,h=n_test)
# plot(trend_prediction)
# trend = trend_prediction$mean

trend_fit2 <- lm(y~poly(x, 3), data.frame(x=1:490, y=imfs$residue))
trend_prediction2 <- predict(trend_fit2, data.frame(x=491:515), se=T)
trend2 = trend_prediction2$fit
# plot(trend2)

# ARIMA_future = low_future+high_future+trend
ARIMA_future = low_future+high_future+trend2
past = EUA_train$price_usd
EUA$ARIMA = c(past, ARIMA_future)
EUA %>% ggplot(aes(x=date)) +
  geom_line(aes(y=EUA$price_usd)) +
  geom_line(aes(y=EUA$ARIMA), linetype='dotted')



# nn <- neuralnet(high_norm ~ brent, oil, linear.output = FALSE, hidden=6)
# plot(nn)
# prediction(nn)




###########################################
# EMD PREDICTION
###########################################

# varpred = VAR(try$imf, p = 1,
#              type = c("const", "trend", "both", "none"),
#              season = NULL, exogen = NULL, lag.max = NULL,
#              ic = c("AIC", "HQ", "SC", "FPE"))
# imf_pred = predict(varpred, n.ahead=25)
# plot(imf_pred)
# 
# trend25 <- lm(y~poly(x, 3), data.frame(x=1:490, y=imfs$residue))
# tp25 <- predict(trend25, data.frame(x=491:515), se=T)
# 
# EMD_future = emd.pred(imf_pred, tp25, ci = 0.95, figure = TRUE)
# EUA$EMD = c(past, EMD_future$fcst)
# EUA %>% ggplot(aes(x=date)) +
#   geom_line(aes(y=EUA$price_usd)) +
#   geom_line(aes(y=EUA$EMD), linetype='dotted')

