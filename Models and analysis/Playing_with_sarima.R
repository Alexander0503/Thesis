# PLAYING WITH ARIMA/SARIMA/TBATS modeling

aView(months_by_name)
View(df_monthly_rate)
library(lubridate)
library(nlme)              
library(car)
library(Epi)
library(epiDisplay)
library(forecast)
library(astsa)

# intend to present number as they are. 
options(scipen = 5)

#create copy and call it ITS monthly df rate
View(df_monthly_rate_its)

# create date variable
date_variable_by_day= seq(from = ymd("2018-01-01"), to = ymd("2021-12-31"), by="days")

# make ts object 
chest_pain_ts = ts(df_monthly_rate$chest_pain_visits_rate, frequency = 12, start = 2018)
plot(chest_pain_ts)

library(zoo)
# view the acf/pacf plots un-differenced data
par(mfrow=c(1,2)) # to produce 2 plots in one figure


# acf plot with seasonal difrencing 
acf2(coredata(diff(chest_pain_ts,12)),max.lag = 24) # we still have ar 1 process. 

# acf plot with regular difrencing 
acf2(coredata(diff(chest_pain_ts)),max.lag = 24) # it look ok with differencing one time.  

tsdisplay(chest_pain_ts)

# check how many differences are required to make ts stationary.
ndiffs(chest_pain_ts) # 0,  as regular difference
nsdiffs(chest_pain_ts) # 0,  as seasonal difference 
# But the result of this is not coreect becase we notice we need to do 1 difrencing.


###############################
#Building ARIMA/SARIMA model
##############################
chest_pain_arima_model_1=auto.arima(chest_pain_ts, xreg =cbind(df_monthly_rate$trend_first,df_monthly_rate$level_first, df_monthly_rate$trend_second, df_monthly_rate$level_second),
                                    trace = T, seasonal = T, stepwise = F, approximation = F, ic = "aic", max.d = 1, max.D = 1) # ARIMA(1,0,0) AIC=410.31

summary(chest_pain_arima_model_1) # 


chest_pain_arima_model_2=auto.arima(chest_pain_ts, xreg =cbind(df_monthly_rate$trend_first,df_monthly_rate$level_first, df_monthly_rate$trend_second, df_monthly_rate$level_second),
                                    trace = T, seasonal = T, stepwise = F, approximation = F, ic = "aic",D = 1) #ARIMA(0,0,0)(1,1,0)[12]


summary(chest_pain_arima_model_2) # AIC=320.99 

chest_pain_arima_model_2a = Arima(window(chest_pain_ts,end = c(2020,1)),
                                  order = c(0,0,0), seasonal = list(order=c(1,1,0),period=12))

chest_pain_arima_fc_model_2a = forecast(chest_pain_arima_model_2a, h=12, level = F)

chest_pain_arima_model_2b = arima0(window(chest_pain_ts,end = c(2021,1)),
                                   order = c(0,0,0), seasonal = list(order=c(1,1,0),period=12))


chest_pain_arima_model_2b = Arima(window(chest_pain_ts,end = c(2021,1)),
                                  order = c(0,0,0), seasonal = list(order=c(1,1,0),period=12))



##########################################################################################

######################
# Fortnightly results
######################


all_visits_by_forth=as.data.frame(table(erdatawf$forthnight_counter))

# make x as numeric so we can use the scatter plot, because its only need a numeric values. (before conversion is char)
all_visits_by_forth$Var1 = as.numeric(all_visits_by_forth$Var1)

# make a ts object of forth
all_visits_by_forth_ts = ts(all_visits_by_forth$Freq, start = 2018, frequency = 365.25/14)
plot(all_visits_by_forth_ts)
autoplot(all_visits_by_forth_ts)

# all visits Create a regular data frame weekly until 27 Jan 2020 (Pre period)
all_visits_forth_pre = all_visits_by_forth[-c(55:105),]
all_visits_forth_pre_ts = ts(all_visits_forth_pre$Freq, start = 2018, frequency = 365.25/14)
plot(all_visits_forth_pre_ts)
#####################
# MI visits
#####################

# create MI forth visits
mi_visits_forth=as.data.frame(table(erdatawf$forthnight_counter,erdatawf$myocardial_char_desc))
mi_visits_forth$Var1 = as.numeric(mi_visits_forth$Var1)

# create ts object of MI visits
mi_visits_forth_ts = ts(mi_visits_forth$Freq, start = 2018, frequency = 365.25/14)
plot(mi_visits_forth_ts)

# Create a MI visits split data frame of 2 weekly until 27 Jan 2020 (Pre period)
mi_visits_forth_pre = mi_visits_forth[-c(55:105),]
mi_visits_forth_pre_ts = ts(mi_visits_forth_pre$Freq, start = 2018, frequency = 365.25/14)

#############################################################
# check the seasonality and trend with the auto stl function
#############################################################
plot(mstl(all_visits_by_forth_ts_1)) 
plot(mstl(mi_visits_forth_ts )) 

################################
# outliers detection 
library(tsoutliers)
library(outliers)
###############################

# TC - temporary changes, AO - Additive outlier, LS - level shift, Io - Innovation outlier, SLS - Seasonal level shift
# for all visits
tso(all_visits_by_forth_ts) # detected one point of TC 

# plot the outliers of all visits
plot(tso(all_visits_by_forth_ts)) 


#############################################
#ACF and PACF
############################################
par(mfrow=c(1,2))

# All visits
Pacf(all_visits_by_forth_ts) # for AR process
Acf(all_visits_by_forth_ts) # for MA process.in

# MI visits
Pacf(mi_visits_forth_ts) # for AR process
Acf(monthly_myocardial_ts) # for MA process.

dev.off()


# scatter plot with regression line
ggscatter(data = all_visits_by_forth, x="Var1", y= "Freq",
          add = "reg.line", conf.int =T, 
          cor.coef = T, cor.method = "pearson")

# another way of scatter plot and reggression line
plot(all_visits_by_forth$Var1, all_visits_by_forth$Freq, data = all_visits_by_forth)

# add a regression line to the scatter plot.
abline(lm(Freq~Var1, data = all_visits_by_forth))

# pearson correlation test, "cor" in other words is the R correlation 
cor.test(y = all_visits_by_forth$Freq,
         x=all_visits_by_forth$Var1, 
         method = "pearson")

y1 = 1:105
x = as.integer(year_month_all_visit_count$all_visits)

plot(y=all_visits_by_forth$Freq,x=y1,
     ylab= "All visits (chest pain and MI)",
     ylim = c(3000,7000),
     xlab="14 days interval",
     type = "o",
     col="red",
     xaxt="n")

#add x-axis months labels
axis(1, 1:105, labels = all_visits_by_forth$Var1)

points(x, y,
       col="red",
       pch=20, 
       type= "l")

points(all_visits_by_forth$Freq,
       col="red",
       pch=22, 
       type= "b")

# add intervention line 
abline(v=54.5, lty=2)


#######################
# TBATS model
######################
# Build the tbats model of space time model the take account for slowly change in the seasonality. by using the fourier order. 

forth_all_visits_model_tbats_1 = tbats(all_visits_forth_pre_ts) 
fc_forth_allvisits_tbats_1 = forecast(forth_all_visits_model_tbats_1  , h=51, level = 95) # forecasting for 51 2 weekly interval until 9 Jan 2022. with 95% CI
summary(fc_forth_allvisits_tbats_1) #AIC: 816.8334
plot(fc_forth_allvisits_tbats_1, lwd=2, ylim=c(3000,9500))


lines(abline(v= 2020.05, lty = "dotted")) # this is right for 2 weekly interval
lines(all_visits_by_forth_ts, lwd=2)
points(all_visits_by_forth_ts,
       col="red",
       pch=1, 
       type= "b")

################################################################
#TBATS model
# compare the fitted model all visits
plot(all_visits_by_forth_ts, col = "blue", lwd =2)
points(all_visits_by_forth_ts,
       col="blue",
       pch=1, 
       type= "b")

lines(forth_all_visits_model_tbats_1$fitted.values, col = "pink")
points(forth_all_visits_model_tbats_1$fitted,
       col="red",
       pch=1, 
       type= "b")

#########################################################
#TBATS model
# Mi visits 

forth_mi_model_tbats_1 = tbats(mi_visits_forth_pre_ts) 
fc_forth_mi_tbats_1 = forecast(forth_mi_model_tbats_1 , h=51, level = 95) # forecasting for 51 2 weekly interval until 9 Jan 2022. with 95% CI
summary(fc_forth_mi_tbats_1) #AIC: 563.9661

plot(fc_forth_mi_tbats_1, lwd=2)
lines(abline(v= 2020.05, lty = "dotted")) # this is right for 2 weekly interval
lines(mi_visits_forth_ts, lwd=2)
points(mi_visits_forth_ts,
       col="red",
       pch=1, 
       type= "b")

##################################################

# ARIMA/SARIMA model all visits

#building auto arima model all visits
arima_forth_all_vists=auto.arima(all_visits_forth_pre_ts, trace = T, seasonal = T, stepwise = F, approximation = F, ic = "aic") # chosen ARIMA(0,1,2)                 
fc_arima_forth_all_visits = forecast(arima_forth_all_vists, level = c(95), h=51) 
summary(arima_forth_all_vists) # AIC=752.6


# #building auto arima model, include D=1, all-visits
arima_forth_all_vists_1=auto.arima(all_visits_forth_pre_ts, trace = T, seasonal = T, stepwise = F, approximation = F, D=1) # chosen ARIMA(0,0,0)(0,1,0)[26] with drift 
fc_arima_forth_all_visits_1 = forecast(arima_forth_all_vists_1 , level = c(95), h=51) # we ask forecast for 51 two interval week
summary(arima_forth_all_vists_1) #AIC=420.57 

# plot the model
plot(fc_arima_forth_all_visits_1,lwd=2, ylim=c(3200,8000))
lines(abline(v= 2020.05, lty = "dotted"))
lines(all_visits_by_forth_ts, lwd=2)
points(all_visits_by_forth_ts,
       col="red",
       pch=1, 
       type= "b")

######################################
# ARIMA/SARIMA model MI visits

#building auto arima model MI visits
arima_forth_mi_vists=auto.arima(mi_visits_forth_pre_ts, trace = T, seasonal = T, stepwise = F, approximation = F, ic = "aic") # ARIMA(0,0,0)(0,1,0)[26] with drift                   
fc_arima_forth_mi_visits = forecast(arima_forth_mi_vists, level = c(95), h=51) 
summary(arima_forth_mi_vists) #AIC=269.83 


# #building auto arima model, include D=1, MI-visits
arima_forth_mi_vists_1=auto.arima(mi_visits_forth_pre_ts, trace = T, seasonal = T, stepwise = F, approximation = F, D=1) # chosen ARIMA(0,0,0)(0,1,0)[26] with drift  
fc_arima_forth_mi_visits_1 = forecast(arima_forth_mi_vists_1 , level = c(95), h=51) # we ask forecast for 51 two interval week
summary(arima_forth_mi_vists_1)#AIC=269.83

# plot the model
plot(fc_arima_forth_mi_visits_1,lwd=2)
lines(abline(v= 2020.05, lty = "dotted"))
lines(mi_visits_forth_ts, lwd=2)
points(mi_visits_forth_ts,
       col="red",
       pch=1, 
       type= "b")

par(mfrow=c(1,2))
dev.off()


############################################################
