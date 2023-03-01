# Createt by Alexander Kagan

Sys.setlocale("LC_TIME", "English")

View(df_monthly_rate)

###############################################################################################################################
# Description of the CORARMA options in GLS: 

#corAR1	autoregressive process of order 1.
#corARMA	autoregressive moving average process, with arbitrary orders for the autoregressive and moving average components.
#corCAR1	continuous autoregressive process (AR(1) process for a continuous time covariate).
#corCompSymm	compound symmetry structure corresponding to a constant correlation.
#corExp	exponential spatial correlation.
#corGaus	Gaussian spatial correlation.
#corLin	linear spatial correlation.
#corRatio	Rational quadratics spatial correlation.
#corSpher	spherical spatial correlation.
#corSymm	general correlation matrix, with no additional structure.


#####################################
# check normal distribution. null hyphotesis - normal distribution. 
shapiro.test(df_monthly_rate$acs_rate_male_age_grp_2_65_above) # p-value =  

############################
# initial plot with ggplot2
###########################
library(ggplot2)
library(hrbrthemes)
library(dplyr)
library(scales)
library(ggpubr)

df_monthly_rate$acs_rate_male_age_grp_2_65_above

initial_plot_acs_males_65_above = ggplot(data = df_monthly_rate, aes(x=time_by_month, y= acs_rate_male_age_grp_2_65_above))+
  
  # choose the line type, color, and size.
  geom_line(color = "purple2", show.legend = T, linewidth = 1)+
  
  #add points
  geom_point(shape= 1, color = "purple2", size = 2.9)+
  
  #choose the theme
  theme_bw()+
  
  #control the x-axis
  
  scale_x_date(date_breaks = "1 month", labels=date_format("%b-%y"))+
  
  #add angle to x-axis
  theme(axis.text.x = element_text(angle = 90, hjust = 0.5))+
  
  #put the text of the dates in the x-axis to more in the middle
  theme(axis.text.x = element_text(vjust = 0.5))+
  
  # limit the y-axis
  ylim(c(50,130))+
  
  # add labels
  labs(y= "Visit rate per 100,000 people/month", x="Month and year")+
  
  #set title 
  ggtitle("ACS in males ages 65+, monthly visit rate per 100,000 people, 2018-2021")+
  
  #change the position of the title 1 = right align, defult is left, 0.5 = center
  theme(plot.title = element_text(hjust = 0.5))+ 
  
  # #control and limit the x-axis 
  coord_cartesian(xlim = c(as.Date.character("2018-02-15"),as.Date.character("2021-11-01")))+
  
  # add intervention lines at 15 Jab 2020 and 15 Jan 2021
  geom_vline(xintercept =as.Date.character("2020-2-15"), linetype= "dashed", linewidth = 0.9)+
  geom_vline(xintercept =as.Date.character("2021-1-15"),linetype= "dashed",linewidth=0.9 )+
  
  #increase the size of the x y text size
  theme(axis.title.x = element_text(size =  11), 
        axis.title.y = element_text(size =  11))+
  
  # move away the title of x and y
  theme(axis.title.x = element_text(margin = margin(t=20)),
        axis.title.y = element_text(margin = margin(r=20)))

####################################
initial_plot_acs_males_65_above
####################################

#####################################
# create ts for the full time period
#####################################
library(forecast)
ACS_rate_males_65_above_ts_all_period = ts(df_monthly_rate$acs_rate_male_age_grp_2_65_above, start = 2018, frequency = 12)

df_monthly_rate$acs_rate_male_age_grp_2_65_above

#############################################################################
#Create a seasonal plot of the series 

ACS_seasonal_plot_males_65_above = ggseasonplot(ACS_rate_males_65_above_ts_all_period) +
  geom_line(linewidth=1.5) +
  theme_bw() +
  ggtitle("Seasonal plot: ACS in males ages 65+, monthly visit rate per 100,000 people, 2018-2021")+
  ylab("Monthly visit rate per 100,000")+
  #change the position of the title 1 = right align, defult is left, 0.5 = center
  theme(plot.title = element_text(hjust = 0.5))+
  geom_point(col="black")+
  labs(col="Year")+
  guides(color = guide_legend(override.aes = list(size = 22)))

######################################################
# Stationarity and autocorrelation test of the series 
######################################################
library(tseries)# also for bgt, bds.test and terasvirta
library(lmtest)

#stationarity test of the series
adf.test(ACS_rate_males_65_above_ts_all_period) # h1 = stationary. 

#autocorelation test of the series
bgtest(acs_rate_male_age_grp_2_65_above~time, data = df_monthly_rate, order=6) # H0 = no autocorelation at any order until 12

#autocorelation box test 
Box.test(ACS_rate_males_65_above_ts_all_period) # (H0 = no autocorelatione of the series)
#we have autocorelation of the series. 

##################################################################
# ACS sex difference between the ages 65 and above with 2-y-axis.
##################################################################
library(scales)
library(ggplot2)
library(ggthemes)
library(hrbrthemes)


                                            #what is in left side           what is in the right side
scaleFactor = max(df_monthly_rate$acs_rate_female_age_grp_2_65_above) / max(df_monthly_rate$acs_rate_male_age_grp_2_65_above)

female_males_above_65_2y_plot = ggplot(df_monthly_rate,aes(x=time_by_month)) +  
  #choose the theme
  theme_bw()+
  #set title 
  ggtitle("ACS sex differences at ages 65 and above, monthly visit rate per 100,000 people, 2018-2021\n")+
  
  #change the position of the title 1 = right align, defult is left, 0.5 = center
  theme(plot.title = element_text(hjust = 0.5))+ 
  
  theme(axis.text.x = element_text(angle = 90, hjust = 0.5))+
  
  #control the x-axis
  scale_x_date(date_breaks = "1 month", labels=date_format("%b-%y"))+
  
  #add angle to x-axis
  theme(axis.text.x = element_text(angle = 90, hjust = 0.5))+
  
  #put the text of the dates in the x-axis to more in the middle
  theme(axis.text.x = element_text(vjust = 0.5))+
  
  # what is in left side female 
  geom_line(aes(y=acs_rate_female_age_grp_2_65_above), col="goldenrod", linewidth=1)+
  geom_point(aes(y=acs_rate_female_age_grp_2_65_above),shape= 1, color = "goldenrod", size = 2.9)+
  
  # what is in right side male 
  geom_line(aes(y=acs_rate_male_age_grp_2_65_above * scaleFactor), col="purple2", linewidth=1, linetype = "longdash")+
  geom_point(aes(y=acs_rate_male_age_grp_2_65_above * scaleFactor),shape= 1, color = "purple2", size = 2.9)+
  
                                              # left side                                                                             # left side
  scale_y_continuous(name = "Females ACS rate per 100,000 people/month \n", sec.axis=sec_axis(~./scaleFactor, name ="Males ACS rate per 100,000 people/month\n"))+
  theme(
    axis.title.y.left=element_text(color ="goldenrod"),
    axis.text.y.left=element_text(color="goldenrod"),
    axis.title.y.right=element_text(color="purple2"),
    axis.text.y.right=element_text(color="purple2")
  ) +
  
  # add intervention lines at 15 Jab 2020 and 15 Jan 2021
  geom_vline(xintercept =as.Date.character("2020-2-15"), linetype= "dashed", size = 0.9)+
  geom_vline(xintercept =as.Date.character("2021-1-15"),linetype= "dashed",size=0.9 ) +
  xlab("Month and year")

female_males_above_65_2y_plot

#############################################################################################    
# preliminary analysis -  lm() function for a standard OLS regression with a time series
#specification, this will form the basis for cheking the autocorrelation. 
#the Intercept will be calculated by R.
########################################################################################
library(nlme)              
library(car)
library(Epi)
library(epiDisplay)

model_lm_acs_males_65_above = lm(acs_rate_male_age_grp_2_65_above~time+ level_first+level_second+ trend_first+trend_second+V1+V2+V3+V4, 
                                 data = df_monthly_rate)

# check the model
tab_model(model_lm_acs_males_65_above, show.obs = F, show.aic = T) # 0.8, AIC =326.611.
summary(model_lm_acs_males_65_above)


# simple lm with seasonal dummy variables:
model_lm_acs_males_65_above_dummy = lm(acs_rate_male_age_grp_2_65_above~time+ level_first+level_second+ trend_first+trend_second+Year_dummy, 
                                       data = df_monthly_rate)

tab_model(model_lm_acs_males_65_above_dummy, show.obs = F, show.aic = T) # 0.85, AIC 325.9

####################################################
# Assessing Autocorrelation of the simple lm model
###################################################
library(forecast)
library(lmtest)
dev.off()
# durbin-watson test, 12 time periods
dwt(model_lm_acs_males_65_above, max.lag=12, alternative = "two.sided") # the most significant lag was lag 12. 

Box.test(residuals(model_lm_acs_males_65_above)) # not have any correlation p-value = 0.5718

# Plot the residuals from option 2 - display automatically also the ACF and PACF
tsdisplay(residuals(model_lm_acs_males_65_above))


#check residuals function from forecast package option 2 - the function also automaticly creates a Breusch-Godfrey test
checkresiduals(model_lm_acs_males_65_above) # Breaush-Godfrey test, we have a correlation. p-value p-value = 0.002236

par(mfrow=c(1,2)) # to produce 2 plots in one figure
# produce the plots
acf(residuals(model_lm_acs_males_65_above), lag.max = 12) # ignore the first lag
pacf(residuals(model_lm_acs_males_65_above), lag.max = 12) 

acf(residuals(model_lm_acs_males_65_above_dummy), lag.max = 12) # ignore the first lag
pacf(residuals(model_lm_acs_males_65_above_dummy), lag.max = 12) 


# library(lmtest) for bptest - for homo (=h0)/heteroscasitcity=(h1) test the model
bptest(model_lm_acs_males_65_above) # p-value =0.2448
# the model is homoscadastic - the residuals are distributed with equal variance 


#######################################################################################
#check the order with auto arima function
#before setting the order to the final model
# i need to do one ts object before pandemic and other after? or just to whole period? 
#######################################################################################
library(tsModel)#for the harmonic terms

# is we set seasonal T for the whole period
acs_male_65_above_rate_sarima = auto.arima(ACS_rate_males_65_above_ts_all_period, trace = T, seasonal = T, D=1 ,stepwise = F, approximation = F, ic = "aic") # It choose the ARIMA(1,0,0) 
summary(acs_male_65_above_rate_sarima) # SARIMA ARIMA(0,0,2)(1,1,0)[12] with drift   AIC 270.2

#############################################################################
# Build and fit the final model
library(tseries)  
library(tsModel)#for the harmonic terms
library(lme4)
library(sjPlot)
par(mfrow=c(1,2)) #

###########################################################################################
# the model i choose 
model_acs_male_65_above_final_p2q0 = gls(data = df_monthly_rate, 
                                         acs_rate_male_age_grp_2_65_above~time + level_first +trend_first + level_second + trend_second + harmonic(month_number,2,12), #  
                                         correlation = corARMA(p=2,q=0, form = ~time), method = "ML")


acf(residuals(model_acs_male_65_above_final_p2q0), lag.max = 12) # ignore the first lag, . for MA process  
pacf(residuals(model_acs_male_65_above_final_p2q0), lag.max = 12) 
tab_model(model_acs_male_65_above_final_p2q0, show.aic = T, show.obs = F) # 0.76, AIC 330. 
adf.test(residuals(model_acs_male_65_above_final_p2q0)) # p-value = 0.05688
qqPlot(residuals(model_acs_male_65_above_final_p2q0))
checkresiduals(model_acs_male_65_above_final_p2q0)
Box.test(residuals(model_acs_male_65_above_final_p2q0), type = 'Ljung-Box') #we got 0.07957 so it's ok.
dev.off()

###########################################################################################
# Check other model 

model_acs_male_65_above_final_p13q0= update(model_acs_male_65_above_final_p1q0, correlation = corARMA(p=13,q=0)) # 
summary(model_acs_male_65_above_final_p13q0) #   316.2751 
acf(residuals(model_acs_male_65_above_final_p13q0)) # ignore the first lag, . for MA process  
pacf(residuals(model_acs_male_65_above_final_p13q0))
adf.test(residuals(model_acs_male_65_above_final_p13q0))

anova(model_acs_male_65_above_final_p13q0, model_acs_male_65_above_final_p1q0) # not present p-value so we decide only by aic p0q1 is better

##########################################################################################################################
#compare other models 
model_acs_male_65_above_final_p0q0 = gls(data = df_monthly_rate, 
                                         acs_rate_male_age_grp_2_65_above~time + level_first +trend_first + level_second + trend_second + harmonic(month_number,2,12), #  
                                         correlation=NULL, method = "ML")


summary(model_acs_male_65_above_final_p0q0) #   330.9065 
acf(residuals(model_acs_male_65_above_final_p0q0), lag.max = 12) # ignore the first lag, . for MA process  
pacf(residuals(model_acs_male_65_above_final_p0q0), lag.max = 12) 
anova(model_acs_male_65_above_final_p1q0, model_acs_male_65_above_final_p0q0)#p-value 0.5541 no significant difference. 
adf.test(residuals(model_acs_male_65_above_final_p0q0, k=1)) #p-value = 0.05437
tab_model(model_acs_male_65_above_final_p0q0)

#################################################################################################
library(ggplot2)
library(ggthemes)
library(hrbrthemes)


final_plot_males_65_above_p2q0 = ggplot(data = NULL, aes(x,y))+
  
  # add intervention lines at 15 Jab 2020 and 15 Jan 2021
  geom_vline(xintercept =as.Date.character("2020-2-15"), linetype= "dashed", size = 0.5, col="black")+
  geom_vline(xintercept =as.Date.character("2021-1-15"),linetype= "dashed",size=0.5, col="black" )+
  
  #add points
  geom_point(data = df_monthly_rate, aes(x=time_by_month, y= acs_rate_male_age_grp_2_65_above),shape= 19, color = "#4862b4", size = 2, alpha = 0.7)+
  
  #control the x-axis
  scale_x_date(date_breaks = "1 month", labels=date_format("%b-%y"))+
  
  
  # #control and limit the x-axis and y-axis 
  coord_cartesian(xlim = c(as.Date.character("2018-01-01"),as.Date.character("2021-12-31")),expand = T)+
  
  #y-limit
  ylim(c(55,130))+
  
  # add labels
  labs(y= "Visits rate per 100,000 people/month\n", x="Month and year\n")+
  
  #set title 
  ggtitle("ACS in males ages 65+, monthly visit rate per 100,000 people, 2018-2021\n")+
  
  #increase the size of the x y text size
  theme(axis.title.x = element_text(size =  11), 
        axis.title.y = element_text(size =  11))+
  
  
  # pre-period model predicted. until here its work. seasonal adjustment
  geom_line(aes(y=fitted(model_acs_male_65_above_final_p2q0)[1:26], 
                x=df_monthly_rate$time_by_month[1:26]) 
            ,color = "sandybrown", linetype ="solid",size = 0.5) + 
  
  
  # now the un-udjusted segment line of the pre-pandemic period. 
  geom_segment(aes(x=as.Date.character("2018-01-01"),xend =as.Date.character("2020-02-01"), 
                   y=model_acs_male_65_above_final_p2q0$coefficients[1]+model_acs_male_65_above_final_p2q0$coefficients[2]*1,
                   yend =model_acs_male_65_above_final_p2q0$coefficients[1]+model_acs_male_65_above_final_p2q0$coefficients[2]*26 
  ), col = "steelblue", size=1)+ 
  
  #first counterfractual of pandemic period 1
  # first counterfractual for the first intervention - we have 12 data point from 27 to 37. 11+1 (because we include the latst point also). 
  geom_segment(aes(x= as.Date.character("2020-03-01"),xend =as.Date.character("2021-01-01"), 
                   y= model_acs_male_65_above_final_p2q0$coefficients[1]+model_acs_male_65_above_final_p2q0$coefficients[2]*27, #y1 - checked the y and it's look ok.
                   
                   yend = model_acs_male_65_above_final_p2q0$coefficients[1]+model_acs_male_65_above_final_p2q0$coefficients[2]*37 # y2 - checked the y and it's look ok.
  ),linetype='dashed', col="steelblue",size = 0.7)+ 
  
  
  # geom line for the pandemic period 1, seasonal adjustment
  geom_line(aes(y=fitted(model_acs_male_65_above_final_p2q0)[27:37], 
                x=df_monthly_rate$time_by_month[27:37]) 
            ,color = "sandybrown", linetype ="solid",size = 0.5)+  
  
  
  # now the un-udjusted segment line of the pandemic period 1 - it's work
  geom_segment(aes(x=as.Date.character("2020-03-01"),
                   xend =as.Date.character("2021-01-01"), 
                   y=model_acs_male_65_above_final_p2q0$coefficients[1]+model_acs_male_65_above_final_p2q0$coefficients[2]*27+ # 27 is start of the first interruption
                     model_acs_male_65_above_final_p2q0$coefficients[3]+model_acs_male_65_above_final_p2q0$coefficients[4]*1,
                   
                   yend=model_acs_male_65_above_final_p2q0$coefficients[1]+
                     model_acs_male_65_above_final_p2q0$coefficients[2]*37+ # 37 is end of first interruption (the last point in the period of first intervention)
                     model_acs_male_65_above_final_p2q0$coefficients[3]+
                     model_acs_male_65_above_final_p2q0$coefficients[4]*11) # point from the first intervention (include that pint) until 
               #the second intervention (not include the first point of the sec intevention)
               , col = "steelblue", size=1)+ 
  
  
  # the seasonal ajdustment line for pandemic 2 period. 
  geom_line(aes(y=fitted(model_acs_male_65_above_final_p2q0)[38:48], 
                x=df_monthly_rate$time_by_month[38:48]) 
            ,color = "sandybrown", linetype ="solid",size = 0.5)+
  
  
  # second counterfractual for the second intervention
  geom_segment(aes(x=as.Date.character("2021-02-01"), #x1
                   xend = as.Date.character("2022-01-01"), 
                   y=model_acs_male_65_above_final_p2q0$coefficients[1]+model_acs_male_65_above_final_p2q0$coefficients[2]*38 + 
                     model_acs_male_65_above_final_p2q0$coefficients[3]+model_acs_male_65_above_final_p2q0$coefficients[4]*12, #y1 =  # 12 = counts from the first intervention point to the start points (include that point) until the second intevention point (include that point) 
                   # 48 represent all the data points we have in the series.
                   yend=model_acs_male_65_above_final_p2q0$coefficients[1]+model_acs_male_65_above_final_p2q0$coefficients[2]*48+ 
                     
                     model_acs_male_65_above_final_p2q0$coefficients[3]+model_acs_male_65_above_final_p2q0$coefficients[4]*22),# = y2 = #22 counts from the first intervention(include that point) till the end of second intervention (include that point)
               linetype='dashed', col="steelblue", size = 0.7)+
  
  # now the un-udjusted segment line of the pandemic period 2 
  geom_segment(aes(x=as.Date.character("2021-02-01"),
                   xend =as.Date.character("2022-01-01"), 
                   y=model_acs_male_65_above_final_p2q0$coefficients[1]+model_acs_male_65_above_final_p2q0$coefficients[2]*38+
                     model_acs_male_65_above_final_p2q0$coefficients[3]+model_acs_male_65_above_final_p2q0$coefficients[4]*12+
                     model_acs_male_65_above_final_p2q0$coefficients[5]+model_acs_male_65_above_final_p2q0$coefficients[6],
                   
                   yend=model_acs_male_65_above_final_p2q0$coefficients[1]+model_acs_male_65_above_final_p2q0$coefficients[2]*48+
                     model_acs_male_65_above_final_p2q0$coefficients[3]+model_acs_male_65_above_final_p2q0$coefficients[4]*22+
                     model_acs_male_65_above_final_p2q0$coefficients[5]+model_acs_male_65_above_final_p2q0$coefficients[6]*11) # point from the first intervention (include that pint) until 
               #the second intervention (not include the first point of the sec intevention)
               , col = "steelblue", size=1)+
  
  
  theme_classic()+
  
  #add angle to x-axis
  theme(axis.text.x = element_text(angle = 90, hjust = 0.5))+
  
  #put the text of the dates in the x-axis to more in the middle
  theme(axis.text.x = element_text(vjust = 0.5))  +
  
  #change the position of the title 1 = right align, defult is left, 0.5 = center
  theme(plot.title = element_text(hjust = 0.5))

#Continue of the first counterfractual
#geom_segment(aes(x= as.Date.character("2021-02-01"),xend =as.Date.character("2021-12-31"), 
#                y= model_acs_male_65_above_final_p2q0$coefficients[1]+model_acs_male_65_above_final_p2q0$coefficients[2]*38, #y1 - checked the y and it's look ok.
#               
#               yend = model_acs_male_65_above_final_p2q0$coefficients[1]+model_acs_male_65_above_final_p2q0$coefficients[2]*48 # y2 - checked the y and it's look ok.
#),linetype='dashed', col="lightblue")

final_plot_males_65_above_p2q0 

#####################################################################################################################################
ibrary(tseries)  
library(tsModel)#for the harmonic terms
library(lme4)
library(car)

# Check the residuals of the model
checkresiduals(model_acs_male_65_above_final_p2q0)


###########################
# outliers detection 
library(tsoutliers)
library(outliers)
###########################

tso(ACS_rate_males_65_above_ts_all_period)# with ARIMA(0,1,1)(1,1,0)[12] errors  AIC=254.22   
plot(tso(ACS_rate_males_65_above_ts_all_period)) #

###############################################################
# Mar 2020 (point 27) - MONTH 03  #
#####################################  
# Calculate the incidence rate ratio   
library(plotrix)

#FITTED
acs_male_65_above_pred_03_2020 = fitted(model_acs_male_65_above_final_p2q0)[27]

# counterfractual
acs_male_65_above_cfac_03_2020 = model_acs_male_65_above_final_p2q0$coefficients[1]+ model_acs_male_65_above_final_p2q0$coefficients[2]*27 #   


#absolute change per 100,000 visits 
acs_male_65_above_absolute_change_03_2020 = acs_male_65_above_pred_03_2020 - acs_male_65_above_cfac_03_2020 #
std.error(acs_male_65_above_pred_03_2020,acs_male_65_above_cfac_03_2020)
#absolute change in people in 2020 by population above 24. 
acs_male_65_above_absolute_change_in_people_03_2020 = (acs_male_65_above_pred_03_2020-acs_male_65_above_cfac_03_2020)/100000*5235400 # 

# Relative change in percentage 
acs_male_65_above_relative_change_03_2020=(acs_male_65_above_pred_03_2020 - acs_male_65_above_cfac_03_2020)/(acs_male_65_above_cfac_03_2020)*100 #  

#############################################################################################################  
#APRIL 2020 (point 28) MONTH 04

#FITTED
acs_male_65_above_pred_04_2020 = fitted(model_acs_male_65_above_final_p2q0)[28]

# counterfractual
acs_male_65_above_cfac_04_2020 = model_acs_male_65_above_final_p2q0$coefficients[1]+ model_acs_male_65_above_final_p2q0$coefficients[2]*28 #   


#absolute change per 100,000 visits 
acs_male_65_above_absolute_change_04_2020 = acs_male_65_above_pred_04_2020 - acs_male_65_above_cfac_04_2020 #  

#absolute change in people in 2020 by population above 24. 
acs_male_65_above_absolute_change_in_people_04_2020 = (acs_male_65_above_pred_04_2020-acs_male_65_above_cfac_04_2020)/100000*5235400 # 

# Relative change in percentage 
acs_male_65_above_relative_change_04_2020=(acs_male_65_above_pred_04_2020 - acs_male_65_above_cfac_04_2020)/(acs_male_65_above_cfac_04_2020)*100 # 

###################################################################################
#MAY 2020(point 29) - MONTH 05

#FITTED
acs_male_65_above_pred_05_2020 = fitted(model_acs_male_65_above_final_p2q0)[29]

# counterfractual
acs_male_65_above_cfac_05_2020 = model_acs_male_65_above_final_p2q0$coefficients[1]+ model_acs_male_65_above_final_p2q0$coefficients[2]*29 #   


#absolute change per 100,000 visits 
acs_male_65_above_absolute_change_05_2020 = acs_male_65_above_pred_05_2020 - acs_male_65_above_cfac_05_2020 #  

#absolute change in people in 2020 by population above 24. 
acs_male_65_above_absolute_change_in_people_05_2020 = (acs_male_65_above_pred_05_2020-acs_male_65_above_cfac_05_2020)/100000*5235400 # 

# Relative change in percentage 
acs_male_65_above_relative_change_05_2020=(acs_male_65_above_pred_05_2020 - acs_male_65_above_cfac_05_2020)/(acs_male_65_above_cfac_05_2020)*100 # 


########################################################################################
# JUNE 2020(point 30) - MONTH 06
#FITTED
acs_male_65_above_pred_06_2020 = fitted(model_acs_male_65_above_final_p2q0)[30]

# counterfractual
acs_male_65_above_cfac_06_2020 = model_acs_male_65_above_final_p2q0$coefficients[1]+ model_acs_male_65_above_final_p2q0$coefficients[2]*30 #   


#absolute change per 100,000 visits 
acs_male_65_above_absolute_change_06_2020 = acs_male_65_above_pred_06_2020 - acs_male_65_above_cfac_06_2020 #  

#absolute change in people in 2020 by population above 24. 
acs_male_65_above_absolute_change_in_people_06_2020 = (acs_male_65_above_pred_06_2020-acs_male_65_above_cfac_06_2020)/100000*5235400 # 

# Relative change in percentage 
acs_male_65_above_relative_change_06_2020=(acs_male_65_above_pred_06_2020 - acs_male_65_above_cfac_06_2020)/(acs_male_65_above_cfac_06_2020)*100 # 

###############################################################################  
# JULY 2020 (point 31) - MONTH 07
#FITTED
acs_male_65_above_pred_07_2020 = fitted(model_acs_male_65_above_final_p2q0)[31]

# counterfractual
acs_male_65_above_cfac_07_2020 = model_acs_male_65_above_final_p2q0$coefficients[1]+ model_acs_male_65_above_final_p2q0$coefficients[2]*31 #   


#absolute change per 100,000 visits 
acs_male_65_above_absolute_change_07_2020 = acs_male_65_above_pred_07_2020 - acs_male_65_above_cfac_07_2020 #  

#absolute change in people in 2020 by population above 24. 
acs_male_65_above_absolute_change_in_people_07_2020 = (acs_male_65_above_pred_07_2020-acs_male_65_above_cfac_07_2020)/100000*5235400 # 

# Relative change in percentage 
acs_male_65_above_relative_change_07_2020=(acs_male_65_above_pred_07_2020 - acs_male_65_above_cfac_07_2020)/(acs_male_65_above_cfac_07_2020)*100 # 

############################################################################################   
# AUGUST 2020 (point 32) - MONTH 08

#FITTED
acs_male_65_above_pred_08_2020 = fitted(model_acs_male_65_above_final_p2q0)[32]

# counterfractual
acs_male_65_above_cfac_08_2020 = model_acs_male_65_above_final_p2q0$coefficients[1]+ model_acs_male_65_above_final_p2q0$coefficients[2]*32 #   


#absolute change per 100,000 visits 
acs_male_65_above_absolute_change_08_2020 = acs_male_65_above_pred_08_2020 - acs_male_65_above_cfac_08_2020 #  

#absolute change in people in 2020 by population above 24. 
acs_male_65_above_absolute_change_in_people_08_2020 = (acs_male_65_above_pred_08_2020-acs_male_65_above_cfac_08_2020)/100000*5235400 # 

# Relative change in percentage 
acs_male_65_above_relative_change_08_2020=(acs_male_65_above_pred_08_2020 - acs_male_65_above_cfac_08_2020)/(acs_male_65_above_cfac_08_2020)*100 # 
###############################################################################################################################

# SEPTEMBER 2020 (point 33) - MONTH 09

#FITTED
acs_male_65_above_pred_09_2020 = fitted(model_acs_male_65_above_final_p2q0)[33]

# counterfractual
acs_male_65_above_cfac_09_2020 = model_acs_male_65_above_final_p2q0$coefficients[1]+ model_acs_male_65_above_final_p2q0$coefficients[2]*33 #   


#absolute change per 100,000 visits 
acs_male_65_above_absolute_change_09_2020 = acs_male_65_above_pred_09_2020 - acs_male_65_above_cfac_09_2020 #  

#absolute change in people in 2020 by population above 24. 
acs_male_65_above_absolute_change_in_people_09_2020 = (acs_male_65_above_pred_09_2020-acs_male_65_above_cfac_09_2020)/100000*5235400 # 

# Relative change in percentage 
acs_male_65_above_relative_change_09_2020=(acs_male_65_above_pred_09_2020 - acs_male_65_above_cfac_09_2020)/(acs_male_65_above_cfac_09_2020)*100  

##########################################################################################################################
# OCTOBER 2020 (point 34) - MONTH 10
#FITTED
acs_male_65_above_pred_10_2020 = fitted(model_acs_male_65_above_final_p2q0)[34]

# counterfractual
acs_male_65_above_cfac_10_2020 = model_acs_male_65_above_final_p2q0$coefficients[1]+ model_acs_male_65_above_final_p2q0$coefficients[2]*34 #   


#absolute change per 100,000 visits 
acs_male_65_above_absolute_change_10_2020 = acs_male_65_above_pred_10_2020 - acs_male_65_above_cfac_10_2020 #  

#absolute change in people in 2020 by population above 24. 
acs_male_65_above_absolute_change_in_people_10_2020 = (acs_male_65_above_pred_10_2020-acs_male_65_above_cfac_10_2020)/100000*5235400 # 

# Relative change in percentage 
acs_male_65_above_relative_change_10_2020=(acs_male_65_above_pred_10_2020 - acs_male_65_above_cfac_10_2020)/(acs_male_65_above_cfac_10_2020)*100  
##################################################################################################
# NOVEMBER 2020 (point 35) - MONTH 11 

#FITTED
acs_male_65_above_pred_11_2020 = fitted(model_acs_male_65_above_final_p2q0)[35]

# counterfractual
acs_male_65_above_cfac_11_2020 = model_acs_male_65_above_final_p2q0$coefficients[1]+ model_acs_male_65_above_final_p2q0$coefficients[2]*35 #   


#absolute change per 100,000 visits 
acs_male_65_above_absolute_change_11_2020 = acs_male_65_above_pred_11_2020 - acs_male_65_above_cfac_11_2020 #  

#absolute change in people in 2020 by population above 24. 
acs_male_65_above_absolute_change_in_people_11_2020 = (acs_male_65_above_pred_11_2020-acs_male_65_above_cfac_11_2020)/100000*5235400 # 

# Relative change in percentage 
acs_male_65_above_relative_change_11_2020=(acs_male_65_above_pred_11_2020 - acs_male_65_above_cfac_11_2020)/(acs_male_65_above_cfac_11_2020)*100

#############################################################################################################  
# DECEMBER 2020 (point 36) - MONTH 12

#FITTED
acs_male_65_above_pred_12_2020 = fitted(model_acs_male_65_above_final_p2q0)[36]

# counterfractual
acs_male_65_above_cfac_12_2020 = model_acs_male_65_above_final_p2q0$coefficients[1]+ model_acs_male_65_above_final_p2q0$coefficients[2]*36 #   


#absolute change per 100,000 visits 
acs_male_65_above_absolute_change_12_2020 = acs_male_65_above_pred_12_2020 - acs_male_65_above_cfac_12_2020 #  

#absolute change in people in 2020 by population above 24. 
acs_male_65_above_absolute_change_in_people_12_2020 = (acs_male_65_above_pred_12_2020-acs_male_65_above_cfac_12_2020)/100000*5235400 # 

# Relative change in percentage 
acs_male_65_above_relative_change_12_2020=(acs_male_65_above_pred_12_2020 - acs_male_65_above_cfac_12_2020)/(acs_male_65_above_cfac_12_2020)*100  
###########################################################################################################################
# January 2021 (point 37) - MONTH 01

#FITTED
acs_male_65_above_pred_01_2021 = fitted(model_acs_male_65_above_final_p2q0)[37]

# counterfractual
acs_male_65_above_cfac_01_2021 = model_acs_male_65_above_final_p2q0$coefficients[1]+ model_acs_male_65_above_final_p2q0$coefficients[2]*37 #   


#absolute change per 100,000 visits 
acs_male_65_above_absolute_change_01_2021 = acs_male_65_above_pred_01_2021 - acs_male_65_above_cfac_01_2021 #  

#absolute change in people in 2021 by population above 24. 
acs_male_65_above_absolute_change_in_people_01_2021 = (acs_male_65_above_pred_01_2021-acs_male_65_above_cfac_01_2021)/100000*5329862 # 

# Relative change in percentage 
acs_male_65_above_relative_change_01_2021=(acs_male_65_above_pred_01_2021 - acs_male_65_above_cfac_01_2021)/(acs_male_65_above_cfac_01_2021)*100 

#############################################################################################################################  
# From February 2021 (point 38) - Month 02 - second interruption points
#FITTED
acs_male_65_above_pred_02_2021 = fitted(model_acs_male_65_above_final_p2q0)[38]

# counterfractual
acs_male_65_above_cfac_02_2021 = model_acs_male_65_above_final_p2q0$coefficients[1]+ model_acs_male_65_above_final_p2q0$coefficients[2]*38+
  model_acs_male_65_above_final_p2q0$coefficients[3]+model_acs_male_65_above_final_p2q0$coefficients[4]*12 #   


#absolute change per 100,000 visits 
acs_male_65_above_absolute_change_02_2021 = acs_male_65_above_pred_02_2021 - acs_male_65_above_cfac_02_2021 #  

#absolute change in people in 2021 by population above 24. 
acs_male_65_above_absolute_change_in_people_02_2021 = (acs_male_65_above_pred_02_2021-acs_male_65_above_cfac_02_2021)/100000*5329862 # 

# Relative change in percentage 
acs_male_65_above_relative_change_02_2021=(acs_male_65_above_pred_02_2021 - acs_male_65_above_cfac_02_2021)/(acs_male_65_above_cfac_02_2021)*100 

#############################################################################
# From March 2021 (point 39) - Month 03 

#FITTED
acs_male_65_above_pred_03_2021 = fitted(model_acs_male_65_above_final_p2q0)[39]

# counterfractual
acs_male_65_above_cfac_03_2021 = model_acs_male_65_above_final_p2q0$coefficients[1]+ model_acs_male_65_above_final_p2q0$coefficients[2]*39+
  model_acs_male_65_above_final_p2q0$coefficients[3]+model_acs_male_65_above_final_p2q0$coefficients[4]*13 #   


#absolute change per 100,000 visits 
acs_male_65_above_absolute_change_03_2021 = acs_male_65_above_pred_03_2021 - acs_male_65_above_cfac_03_2021 #  

#absolute change in people in 2021 by population above 24. 
acs_male_65_above_absolute_change_in_people_03_2021 = (acs_male_65_above_pred_03_2021-acs_male_65_above_cfac_03_2021)/100000*5329862 # 

# Relative change in percentage 
acs_male_65_above_relative_change_03_2021=(acs_male_65_above_pred_03_2021 - acs_male_65_above_cfac_03_2021)/(acs_male_65_above_cfac_03_2021)*100 

#####################################################################################################
# From April 2021 (point 40) - Month 04 

#FITTED
acs_male_65_above_pred_04_2021 = fitted(model_acs_male_65_above_final_p2q0)[40]

# counterfractual
acs_male_65_above_cfac_04_2021 = model_acs_male_65_above_final_p2q0$coefficients[1]+ model_acs_male_65_above_final_p2q0$coefficients[2]*40+
  model_acs_male_65_above_final_p2q0$coefficients[3]+model_acs_male_65_above_final_p2q0$coefficients[4]*14 #   


#absolute change per 100,000 visits 
acs_male_65_above_absolute_change_04_2021 = acs_male_65_above_pred_04_2021 - acs_male_65_above_cfac_04_2021 #  

#absolute change in people in 2021 by population above 24. 
acs_male_65_above_absolute_change_in_people_04_2021 = (acs_male_65_above_pred_04_2021-acs_male_65_above_cfac_04_2021)/100000*5329862 # 

# Relative change in percentage 
acs_male_65_above_relative_change_04_2021=(acs_male_65_above_pred_04_2021 - acs_male_65_above_cfac_04_2021)/(acs_male_65_above_cfac_04_2021)*100 

##########################################################################################################
# From MAY 2021 (point 41) - Month 05 

#FITTED
acs_male_65_above_pred_05_2021 = fitted(model_acs_male_65_above_final_p2q0)[41]

# counterfractual
acs_male_65_above_cfac_05_2021 = model_acs_male_65_above_final_p2q0$coefficients[1]+ model_acs_male_65_above_final_p2q0$coefficients[2]*41+
  model_acs_male_65_above_final_p2q0$coefficients[3]+model_acs_male_65_above_final_p2q0$coefficients[4]*15 #   


#absolute change per 100,000 visits 
acs_male_65_above_absolute_change_05_2021 = acs_male_65_above_pred_05_2021 - acs_male_65_above_cfac_05_2021 #  

#absolute change in people in 2021 by population above 24. 
acs_male_65_above_absolute_change_in_people_05_2021 = (acs_male_65_above_pred_05_2021-acs_male_65_above_cfac_05_2021)/100000*5329862 # 

# Relative change in percentage 
acs_male_65_above_relative_change_05_2021=(acs_male_65_above_pred_05_2021 - acs_male_65_above_cfac_05_2021)/(acs_male_65_above_cfac_05_2021)*100 
############################################################################################################  
# From JUNE 2021 (point 42) - Month 06 

#FITTED
acs_male_65_above_pred_06_2021 = fitted(model_acs_male_65_above_final_p2q0)[42]

# counterfractual
acs_male_65_above_cfac_06_2021 = model_acs_male_65_above_final_p2q0$coefficients[1]+ model_acs_male_65_above_final_p2q0$coefficients[2]*42+
  model_acs_male_65_above_final_p2q0$coefficients[3]+model_acs_male_65_above_final_p2q0$coefficients[4]*16 #   


#absolute change per 100,000 visits 
acs_male_65_above_absolute_change_06_2021 = acs_male_65_above_pred_06_2021 - acs_male_65_above_cfac_06_2021 #  

#absolute change in people in 2021 by population above 24. 
acs_male_65_above_absolute_change_in_people_06_2021 = (acs_male_65_above_pred_06_2021-acs_male_65_above_cfac_06_2021)/100000*5329862 # 

# Relative change in percentage 
acs_male_65_above_relative_change_06_2021=(acs_male_65_above_pred_06_2021 - acs_male_65_above_cfac_06_2021)/(acs_male_65_above_cfac_06_2021)*100  

############################################################################################################################
# From JULY 2021 (point 43) - Month 07 

#FITTED
acs_male_65_above_pred_07_2021 = fitted(model_acs_male_65_above_final_p2q0)[43]

# counterfractual
acs_male_65_above_cfac_07_2021 = model_acs_male_65_above_final_p2q0$coefficients[1]+ model_acs_male_65_above_final_p2q0$coefficients[2]*43+
  model_acs_male_65_above_final_p2q0$coefficients[3]+model_acs_male_65_above_final_p2q0$coefficients[4]*17 #   


#absolute change per 100,000 visits 
acs_male_65_above_absolute_change_07_2021 = acs_male_65_above_pred_07_2021 - acs_male_65_above_cfac_07_2021 #  

#absolute change in people in 2021 by population above 24. 
acs_male_65_above_absolute_change_in_people_07_2021 = (acs_male_65_above_pred_07_2021-acs_male_65_above_cfac_07_2021)/100000*5329862 # 

# Relative change in percentage 
acs_male_65_above_relative_change_07_2021=(acs_male_65_above_pred_07_2021 - acs_male_65_above_cfac_07_2021)/(acs_male_65_above_cfac_07_2021)*100      

###################################################################################################################
# From AUGUST 2021 (point 44) - Month 08 

#FITTED
acs_male_65_above_pred_08_2021 = fitted(model_acs_male_65_above_final_p2q0)[44]

# counterfractual
acs_male_65_above_cfac_08_2021 = model_acs_male_65_above_final_p2q0$coefficients[1]+ model_acs_male_65_above_final_p2q0$coefficients[2]*44+
  model_acs_male_65_above_final_p2q0$coefficients[3]+model_acs_male_65_above_final_p2q0$coefficients[4]*18 #   


#absolute change per 100,000 visits 
acs_male_65_above_absolute_change_08_2021 = acs_male_65_above_pred_08_2021 - acs_male_65_above_cfac_08_2021 #  

#absolute change in people in 2021 by population above 24. 
acs_male_65_above_absolute_change_in_people_08_2021 = (acs_male_65_above_pred_08_2021-acs_male_65_above_cfac_08_2021)/100000*5329862 # 

# Relative change in percentage 
acs_male_65_above_relative_change_08_2021=(acs_male_65_above_pred_08_2021 - acs_male_65_above_cfac_08_2021)/(acs_male_65_above_cfac_08_2021)*100     

#############################################################################################################################
# From SEPTEMBER 2021 (point 45) - Month 09 

#FITTED
acs_male_65_above_pred_09_2021 = fitted(model_acs_male_65_above_final_p2q0)[45]

# counterfractual
acs_male_65_above_cfac_09_2021 = model_acs_male_65_above_final_p2q0$coefficients[1]+ model_acs_male_65_above_final_p2q0$coefficients[2]*45+
  model_acs_male_65_above_final_p2q0$coefficients[3]+model_acs_male_65_above_final_p2q0$coefficients[4]*19 #   


#absolute change per 100,000 visits 
acs_male_65_above_absolute_change_09_2021 = acs_male_65_above_pred_09_2021 - acs_male_65_above_cfac_09_2021 #  

#absolute change in people in 2021 by population above 24. 
acs_male_65_above_absolute_change_in_people_09_2021 = (acs_male_65_above_pred_09_2021-acs_male_65_above_cfac_09_2021)/100000*5329862 # 

# Relative change in percentage 
acs_male_65_above_relative_change_09_2021=(acs_male_65_above_pred_09_2021 - acs_male_65_above_cfac_09_2021)/(acs_male_65_above_cfac_09_2021)*100    

###########################################################################################################################
# From OCTOBER 2021 (point 46) - Month 10 

#FITTED
acs_male_65_above_pred_10_2021 = fitted(model_acs_male_65_above_final_p2q0)[46]

# counterfractual
acs_male_65_above_cfac_10_2021 = model_acs_male_65_above_final_p2q0$coefficients[1]+ model_acs_male_65_above_final_p2q0$coefficients[2]*46+
  model_acs_male_65_above_final_p2q0$coefficients[3]+model_acs_male_65_above_final_p2q0$coefficients[4]*20 #   


#absolute change per 100,000 visits 
acs_male_65_above_absolute_change_10_2021 = acs_male_65_above_pred_10_2021 - acs_male_65_above_cfac_10_2021 #  

#absolute change in people in 2021 by population above 24. 
acs_male_65_above_absolute_change_in_people_10_2021 = (acs_male_65_above_pred_10_2021-acs_male_65_above_cfac_10_2021)/100000*5329862 # 

# Relative change in percentage 
acs_male_65_above_relative_change_10_2021=(acs_male_65_above_pred_10_2021 - acs_male_65_above_cfac_10_2021)/(acs_male_65_above_cfac_10_2021)*100    

##########################################################################################################
# From NOVEMBER 2021 (point 47) - Month 11 

#FITTED
acs_male_65_above_pred_11_2021 = fitted(model_acs_male_65_above_final_p2q0)[47]

# counterfractual
acs_male_65_above_cfac_11_2021 = model_acs_male_65_above_final_p2q0$coefficients[1]+ model_acs_male_65_above_final_p2q0$coefficients[2]*47+
  model_acs_male_65_above_final_p2q0$coefficients[3]+model_acs_male_65_above_final_p2q0$coefficients[4]*21 #   


#absolute change per 100,000 visits 
acs_male_65_above_absolute_change_11_2021 = acs_male_65_above_pred_11_2021 - acs_male_65_above_cfac_11_2021 #  

#absolute change in people in 2021 by population above 24. 
acs_male_65_above_absolute_change_in_people_11_2021 = (acs_male_65_above_pred_11_2021-acs_male_65_above_cfac_11_2021)/100000*5329862 # 

# Relative change in percentage 
acs_male_65_above_relative_change_11_2021=(acs_male_65_above_pred_11_2021 - acs_male_65_above_cfac_11_2021)/(acs_male_65_above_cfac_11_2021)*100  

###############################################################################################################################  
# From DECEMBER 2021 (point 48) - Month 12 

#FITTED
acs_male_65_above_pred_12_2021 = fitted(model_acs_male_65_above_final_p2q0)[48]

# counterfractual
acs_male_65_above_cfac_12_2021 = model_acs_male_65_above_final_p2q0$coefficients[1]+ model_acs_male_65_above_final_p2q0$coefficients[2]*48+
  model_acs_male_65_above_final_p2q0$coefficients[3]+model_acs_male_65_above_final_p2q0$coefficients[4]*22 #   


#absolute change per 100,000 visits 
acs_male_65_above_absolute_change_12_2021 = acs_male_65_above_pred_12_2021 - acs_male_65_above_cfac_12_2021 #  

#absolute change in people in 2021 by population above 24. 
acs_male_65_above_absolute_change_in_people_12_2021 = (acs_male_65_above_pred_12_2021-acs_male_65_above_cfac_12_2021)/100000*5329862 # 

# Relative change in percentage 
acs_male_65_above_relative_change_12_2021=(acs_male_65_above_pred_12_2021 - acs_male_65_above_cfac_12_2021)/(acs_male_65_above_cfac_12_2021)*100   

#############################################################################################################################

# make the data frame with the results
acs_male_65_above_results_for_every_month = data.frame(fitted_value_acs_male_65_above_pandemic_1 = c(acs_male_65_above_pred_03_2020,acs_male_65_above_pred_04_2020,acs_male_65_above_pred_05_2020,
                                                                                                     acs_male_65_above_pred_06_2020,acs_male_65_above_pred_07_2020,acs_male_65_above_pred_08_2020,
                                                                                                     acs_male_65_above_pred_09_2020,acs_male_65_above_pred_10_2020,acs_male_65_above_pred_11_2020,
                                                                                                     acs_male_65_above_pred_12_2020,acs_male_65_above_pred_01_2021),
                                                       
                                                       fitted_value_acs_male_65_above_pandemic_2 = c(acs_male_65_above_pred_02_2021,
                                                                                                     acs_male_65_above_pred_03_2021,acs_male_65_above_pred_04_2021,acs_male_65_above_pred_05_2021,
                                                                                                     acs_male_65_above_pred_06_2021,acs_male_65_above_pred_07_2021,acs_male_65_above_pred_08_2021,
                                                                                                     acs_male_65_above_pred_09_2021,acs_male_65_above_pred_10_2021,acs_male_65_above_pred_11_2021,
                                                                                                     acs_male_65_above_pred_12_2021),
                                                       
                                                       
                                                       
                                                       acs_male_65_above_caunter_pred_pandemic_1 = c(acs_male_65_above_cfac_03_2020,acs_male_65_above_cfac_04_2020,acs_male_65_above_cfac_05_2020,
                                                                                                     acs_male_65_above_cfac_06_2020,acs_male_65_above_cfac_07_2020,acs_male_65_above_cfac_08_2020,
                                                                                                     acs_male_65_above_cfac_09_2020,acs_male_65_above_cfac_10_2020,acs_male_65_above_cfac_11_2020,
                                                                                                     acs_male_65_above_cfac_12_2020,acs_male_65_above_cfac_01_2021),
                                                       
                                                       acs_male_65_above_caunter_pred_pandemic_2 = c(acs_male_65_above_cfac_02_2021,
                                                                                                     acs_male_65_above_cfac_03_2021,acs_male_65_above_cfac_04_2021,acs_male_65_above_cfac_05_2021,
                                                                                                     acs_male_65_above_cfac_06_2021,acs_male_65_above_cfac_07_2021,acs_male_65_above_cfac_08_2021,
                                                                                                     acs_male_65_above_cfac_09_2021,acs_male_65_above_cfac_10_2021,acs_male_65_above_cfac_11_2021,
                                                                                                     acs_male_65_above_cfac_12_2021),
                                                       
                                                       acs_male_65_above_absolute_change_pandemic_1 = c(acs_male_65_above_absolute_change_03_2020,acs_male_65_above_absolute_change_04_2020,acs_male_65_above_absolute_change_05_2020,
                                                                                                        acs_male_65_above_absolute_change_06_2020,acs_male_65_above_absolute_change_07_2020,acs_male_65_above_absolute_change_08_2020,
                                                                                                        acs_male_65_above_absolute_change_09_2020,acs_male_65_above_absolute_change_10_2020,acs_male_65_above_absolute_change_11_2020,
                                                                                                        acs_male_65_above_absolute_change_12_2020,acs_male_65_above_absolute_change_01_2021),
                                                       
                                                       acs_male_65_above_absolute_change_pandemic_2 = c(acs_male_65_above_absolute_change_02_2021,
                                                                                                        acs_male_65_above_absolute_change_03_2021,acs_male_65_above_absolute_change_04_2021,acs_male_65_above_absolute_change_05_2021,
                                                                                                        acs_male_65_above_absolute_change_06_2021,acs_male_65_above_absolute_change_07_2021,acs_male_65_above_absolute_change_08_2021,
                                                                                                        acs_male_65_above_absolute_change_09_2021,acs_male_65_above_absolute_change_10_2021,acs_male_65_above_absolute_change_11_2021,
                                                                                                        acs_male_65_above_absolute_change_12_2021),
                                                       
                                                       
                                                       acs_male_65_above_absolute_change_in_people_pandemic_1 = c(acs_male_65_above_absolute_change_in_people_03_2020,acs_male_65_above_absolute_change_in_people_04_2020,acs_male_65_above_absolute_change_in_people_05_2020,
                                                                                                                  acs_male_65_above_absolute_change_in_people_06_2020,acs_male_65_above_absolute_change_in_people_07_2020,acs_male_65_above_absolute_change_in_people_08_2020,
                                                                                                                  acs_male_65_above_absolute_change_in_people_09_2020,acs_male_65_above_absolute_change_in_people_10_2020,acs_male_65_above_absolute_change_in_people_11_2020,
                                                                                                                  acs_male_65_above_absolute_change_in_people_12_2020,acs_male_65_above_absolute_change_in_people_01_2021),
                                                       
                                                       acs_male_65_above_absolute_change_in_people_pandemic_2 = c(acs_male_65_above_absolute_change_in_people_02_2021,
                                                                                                                  acs_male_65_above_absolute_change_in_people_03_2021,acs_male_65_above_absolute_change_in_people_04_2021,acs_male_65_above_absolute_change_in_people_05_2021,
                                                                                                                  acs_male_65_above_absolute_change_in_people_06_2021,acs_male_65_above_absolute_change_in_people_07_2021,acs_male_65_above_absolute_change_in_people_08_2021,
                                                                                                                  acs_male_65_above_absolute_change_in_people_09_2021,acs_male_65_above_absolute_change_in_people_10_2021,acs_male_65_above_absolute_change_in_people_11_2021,
                                                                                                                  acs_male_65_above_absolute_change_in_people_12_2021),
                                                       
                                                       acs_male_65_above_relative_change_pandemic_1 = c(acs_male_65_above_relative_change_03_2020,acs_male_65_above_relative_change_04_2020,acs_male_65_above_relative_change_05_2020,
                                                                                                        acs_male_65_above_relative_change_06_2020,acs_male_65_above_relative_change_07_2020,acs_male_65_above_relative_change_08_2020,
                                                                                                        acs_male_65_above_relative_change_09_2020,acs_male_65_above_relative_change_10_2020,acs_male_65_above_relative_change_11_2020,
                                                                                                        acs_male_65_above_relative_change_12_2020,acs_male_65_above_relative_change_01_2021), 
                                                       
                                                       
                                                       acs_male_65_above_relative_change_pandemic_2 = c(acs_male_65_above_relative_change_02_2021,
                                                                                                        acs_male_65_above_relative_change_03_2021,acs_male_65_above_relative_change_04_2021,acs_male_65_above_relative_change_05_2021,
                                                                                                        acs_male_65_above_relative_change_06_2021,acs_male_65_above_relative_change_07_2021,acs_male_65_above_relative_change_08_2021,
                                                                                                        acs_male_65_above_relative_change_09_2021,acs_male_65_above_relative_change_10_2021,acs_male_65_above_relative_change_11_2021,
                                                                                                        acs_male_65_above_relative_change_12_2021), 
                                                       
                                                       months_pandemic_1 = c("March 2020", "April 2020", "May 2020", "June 2020", "July 2020", "August 2020", "September 2020", "October 2020",
                                                                             "November 2020", "December 2020", "Januar 2021"),
                                                       
                                                       months_pandemic_2 = c("February 2021","March 2021", "April 2021", "May 2021", "June 2021", 
                                                                             "July 2021", "August 2021", "September 2021", "October 2021", "November 2021", "December 2021"))   




library(writexl)
write_xlsx(acs_male_65_above_results_for_every_month , "C:/PROGRAMS/data/tables/results_acs_male_65_above.xlsx")  
####################
upper_ci95 = +1.96  
lower_ci95 = -1.96


#Absolute change

# Sd for the pandemic paerid 1 - absolute change 
Abs_change_pandemic_1_sd = sd(acs_male_65_above_results_for_every_month$acs_male_65_above_absolute_change_pandemic_1) # 1.793788

# Sd for the pandemic paerid 2 - absolute change 
Abs_change_pandemic_2_sd = sd(acs_male_65_above_results_for_every_month$acs_male_65_above_absolute_change_pandemic_2)  # 1.570142 

#sd.error for the pandemic paerid 1 - absolute change 
Abs_change_pandemic_1_sd_err = std.error(acs_male_65_above_results_for_every_month$acs_male_65_above_absolute_change_pandemic_1) # 0.5408474

#sd.error for the pandemic paerid 2 - absolute change 
Abs_change_pandemic_2_sd_err = std.error(acs_male_65_above_results_for_every_month$acs_male_65_above_absolute_change_pandemic_2) # 0.4734155

###################################################################################################
# Relative change

# Sd for the pandemic paerid 1 - relative change 
rel_change_pandemic_1_sd = sd(acs_male_65_above_results_for_every_month$acs_male_65_above_relative_change_pandemic_1) # 6.979705

# Sd for the pandemic paerid 2 - relative change 
rel_change_pandemic_2_sd = sd(acs_male_65_above_results_for_every_month$acs_male_65_above_relative_change_pandemic_2)  # 6.302726

#sd.error for the pandemic paerid 1 - relative change 
rel_change_pandemic_1_sd_err =std.error(acs_male_65_above_results_for_every_month$acs_male_65_above_relative_change_pandemic_1) # 2.10446

#sd.error for the pandemic paerid 2 - relative change 
rel_change_pandemic_2_sd_err = std.error(acs_male_65_above_results_for_every_month$acs_male_65_above_relative_change_pandemic_2) # 1.900343

#########################################################################################################
# absolute change confidence interval for the months - pandemic period 1
#########################################################################  

####################################
#lower absolute ci pandemic period1
####################################
# March 2020 - 03 lower 
acs_male_65_above_absolute_change_03_2020_lower_ci  = acs_male_65_above_absolute_change_03_2020+lower_ci95*Abs_change_pandemic_1_sd_err

#  April 2020 - 04 lower 
acs_male_65_above_absolute_change_04_2020_lower_ci  = acs_male_65_above_absolute_change_04_2020+lower_ci95*Abs_change_pandemic_1_sd_err

# May 2020 - 05 lower 
acs_male_65_above_absolute_change_05_2020_lower_ci  = acs_male_65_above_absolute_change_05_2020+lower_ci95*Abs_change_pandemic_1_sd_err

# May 2020 - 06 lower 
acs_male_65_above_absolute_change_06_2020_lower_ci  = acs_male_65_above_absolute_change_06_2020+lower_ci95*Abs_change_pandemic_1_sd_err

# JULY 2020 - 07 lower 
acs_male_65_above_absolute_change_07_2020_lower_ci  = acs_male_65_above_absolute_change_07_2020+lower_ci95*Abs_change_pandemic_1_sd_err

# August 2020 - 08 lower 
acs_male_65_above_absolute_change_08_2020_lower_ci  = acs_male_65_above_absolute_change_08_2020+lower_ci95*Abs_change_pandemic_1_sd_err

# September 2020 - 09 lower 
acs_male_65_above_absolute_change_09_2020_lower_ci  = acs_male_65_above_absolute_change_09_2020+lower_ci95*Abs_change_pandemic_1_sd_err

# October 2020 - 10 lower 
acs_male_65_above_absolute_change_10_2020_lower_ci  = acs_male_65_above_absolute_change_10_2020+lower_ci95*Abs_change_pandemic_1_sd_err

# November 2020 - 11 lower 
acs_male_65_above_absolute_change_11_2020_lower_ci  = acs_male_65_above_absolute_change_11_2020+lower_ci95*Abs_change_pandemic_1_sd_err

# December 2020 - 12 lower 
acs_male_65_above_absolute_change_12_2020_lower_ci  = acs_male_65_above_absolute_change_12_2020+lower_ci95*Abs_change_pandemic_1_sd_err

# January 2021 - 01 lower 
acs_male_65_above_absolute_change_01_2021_lower_ci  = acs_male_65_above_absolute_change_01_2021+lower_ci95*Abs_change_pandemic_1_sd_err

##########################################
# lower absolute ci pandemic period 2
##########################################
# February 2021 - 02 lower 
acs_male_65_above_absolute_change_02_2021_lower_ci  = acs_male_65_above_absolute_change_02_2021+lower_ci95*Abs_change_pandemic_2_sd_err

# March 2021 - 03 lower 
acs_male_65_above_absolute_change_03_2021_lower_ci  = acs_male_65_above_absolute_change_03_2021+lower_ci95*Abs_change_pandemic_2_sd_err

# April  2021 - 04 lower 
acs_male_65_above_absolute_change_04_2021_lower_ci  = acs_male_65_above_absolute_change_04_2021+lower_ci95*Abs_change_pandemic_2_sd_err

# May  2021 - 05 lower 
acs_male_65_above_absolute_change_05_2021_lower_ci  = acs_male_65_above_absolute_change_05_2021+lower_ci95*Abs_change_pandemic_2_sd_err

# June  2021 - 06 lower 
acs_male_65_above_absolute_change_06_2021_lower_ci  = acs_male_65_above_absolute_change_06_2021+lower_ci95*Abs_change_pandemic_2_sd_err

# July  2021 - 07 lower 
acs_male_65_above_absolute_change_07_2021_lower_ci  = acs_male_65_above_absolute_change_07_2021+lower_ci95*Abs_change_pandemic_2_sd_err

# August  2021 - 08 lower 
acs_male_65_above_absolute_change_08_2021_lower_ci  = acs_male_65_above_absolute_change_08_2021+lower_ci95*Abs_change_pandemic_2_sd_err

# September  2021 - 09 lower 
acs_male_65_above_absolute_change_09_2021_lower_ci  = acs_male_65_above_absolute_change_09_2021+lower_ci95*Abs_change_pandemic_2_sd_err

# October  2021 - 10 lower 
acs_male_65_above_absolute_change_10_2021_lower_ci  = acs_male_65_above_absolute_change_10_2021+lower_ci95*Abs_change_pandemic_2_sd_err

# November  2021 - 11 lower 
acs_male_65_above_absolute_change_11_2021_lower_ci  = acs_male_65_above_absolute_change_11_2021+lower_ci95*Abs_change_pandemic_2_sd_err

# December  2021 - 12 lower 
acs_male_65_above_absolute_change_12_2021_lower_ci  = acs_male_65_above_absolute_change_12_2021+lower_ci95*Abs_change_pandemic_2_sd_err


#####################################
#upper absolute ci pandemic period 1 
#####################################

# March 2020 - 03 upper
acs_male_65_above_absolute_change_03_2020_upper_ci = acs_male_65_above_absolute_change_03_2020+upper_ci95*Abs_change_pandemic_1_sd_err

# April 2020 - 04 upper
acs_male_65_above_absolute_change_04_2020_upper_ci = acs_male_65_above_absolute_change_04_2020+upper_ci95*Abs_change_pandemic_1_sd_err

# May 2020 - 05 upper
acs_male_65_above_absolute_change_05_2020_upper_ci = acs_male_65_above_absolute_change_05_2020+upper_ci95*Abs_change_pandemic_1_sd_err

# June 2020 - 06 upper
acs_male_65_above_absolute_change_06_2020_upper_ci = acs_male_65_above_absolute_change_06_2020+upper_ci95*Abs_change_pandemic_1_sd_err

# JULY 2020 - 07 upper
acs_male_65_above_absolute_change_07_2020_upper_ci = acs_male_65_above_absolute_change_07_2020+upper_ci95*Abs_change_pandemic_1_sd_err

# August 2020 - 08 upper
acs_male_65_above_absolute_change_08_2020_upper_ci = acs_male_65_above_absolute_change_08_2020+upper_ci95*Abs_change_pandemic_1_sd_err

# September 2020 - 09 upper
acs_male_65_above_absolute_change_09_2020_upper_ci = acs_male_65_above_absolute_change_09_2020+upper_ci95*Abs_change_pandemic_1_sd_err

# October 2020 - 10 upper
acs_male_65_above_absolute_change_10_2020_upper_ci = acs_male_65_above_absolute_change_10_2020+upper_ci95*Abs_change_pandemic_1_sd_err

# November 2020 - 11 upper
acs_male_65_above_absolute_change_11_2020_upper_ci = acs_male_65_above_absolute_change_11_2020+upper_ci95*Abs_change_pandemic_1_sd_err

# December 2020 - 12 upper
acs_male_65_above_absolute_change_12_2020_upper_ci = acs_male_65_above_absolute_change_12_2020+upper_ci95*Abs_change_pandemic_1_sd_err

# January 2021 - 01 upper
acs_male_65_above_absolute_change_01_2021_upper_ci = acs_male_65_above_absolute_change_01_2021+upper_ci95*Abs_change_pandemic_1_sd_err

#####################################
#upper absolute ci pandemic period 2 
#####################################
# February 2021 - 02 upper
acs_male_65_above_absolute_change_02_2021_upper_ci = acs_male_65_above_absolute_change_02_2021+upper_ci95*Abs_change_pandemic_2_sd_err

# March 2021 - 03 upper
acs_male_65_above_absolute_change_03_2021_upper_ci = acs_male_65_above_absolute_change_03_2021+upper_ci95*Abs_change_pandemic_2_sd_err

# April 2021 - 04 upper
acs_male_65_above_absolute_change_04_2021_upper_ci = acs_male_65_above_absolute_change_04_2021+upper_ci95*Abs_change_pandemic_2_sd_err

# May 2021 - 05 upper
acs_male_65_above_absolute_change_05_2021_upper_ci = acs_male_65_above_absolute_change_05_2021+upper_ci95*Abs_change_pandemic_2_sd_err

# June 2021 - 06 upper
acs_male_65_above_absolute_change_06_2021_upper_ci = acs_male_65_above_absolute_change_06_2021+upper_ci95*Abs_change_pandemic_2_sd_err

# July 2021 - 07 upper
acs_male_65_above_absolute_change_07_2021_upper_ci = acs_male_65_above_absolute_change_07_2021+upper_ci95*Abs_change_pandemic_2_sd_err

# August 2021 - 08 upper
acs_male_65_above_absolute_change_08_2021_upper_ci = acs_male_65_above_absolute_change_08_2021+upper_ci95*Abs_change_pandemic_2_sd_err

# September 2021 - 09 upper
acs_male_65_above_absolute_change_09_2021_upper_ci = acs_male_65_above_absolute_change_09_2021+upper_ci95*Abs_change_pandemic_2_sd_err

# October 2021 - 10 upper
acs_male_65_above_absolute_change_10_2021_upper_ci = acs_male_65_above_absolute_change_10_2021+upper_ci95*Abs_change_pandemic_2_sd_err

# November 2021 - 11 upper
acs_male_65_above_absolute_change_11_2021_upper_ci = acs_male_65_above_absolute_change_11_2021+upper_ci95*Abs_change_pandemic_2_sd_err


# December 2021 - 12 upper
acs_male_65_above_absolute_change_12_2021_upper_ci = acs_male_65_above_absolute_change_12_2021+upper_ci95*Abs_change_pandemic_2_sd_err

###################################################################################################################################
# Relative change confidence interval for the months - pandemic period 1
#########################################################################  
# March 2020 - 03 upper
acs_male_65_above_relative_change_03_2020_upper_ci = acs_male_65_above_relative_change_03_2020+upper_ci95*rel_change_pandemic_1_sd_err

# March 2020 - 03 lower 
acs_male_65_above_relative_change_03_2020_lower_ci  = acs_male_65_above_relative_change_03_2020+lower_ci95*rel_change_pandemic_1_sd_err

# April 2020 - 04 upper
acs_male_65_above_relative_change_04_2020_upper_ci = acs_male_65_above_relative_change_04_2020+upper_ci95*rel_change_pandemic_1_sd_err

#  April 2020 - 04 lower 
acs_male_65_above_relative_change_04_2020_lower_ci  = acs_male_65_above_relative_change_04_2020+lower_ci95*rel_change_pandemic_1_sd_err

# May 2020 - 05 upper
acs_male_65_above_relative_change_05_2020_upper_ci = acs_male_65_above_relative_change_05_2020+upper_ci95*rel_change_pandemic_1_sd_err

# May 2020 - 05 lower 
acs_male_65_above_relative_change_05_2020_lower_ci  = acs_male_65_above_relative_change_05_2020+lower_ci95*rel_change_pandemic_1_sd_err

# June 2020 - 06 upper
acs_male_65_above_relative_change_06_2020_upper_ci = acs_male_65_above_relative_change_06_2020+upper_ci95*rel_change_pandemic_1_sd_err

# May 2020 - 06 lower 
acs_male_65_above_relative_change_06_2020_lower_ci  = acs_male_65_above_relative_change_06_2020+lower_ci95*rel_change_pandemic_1_sd_err

# JULY 2020 - 07 upper
acs_male_65_above_relative_change_07_2020_upper_ci = acs_male_65_above_relative_change_07_2020+upper_ci95*rel_change_pandemic_1_sd_err

# JULY 2020 - 07 lower 
acs_male_65_above_relative_change_07_2020_lower_ci  = acs_male_65_above_relative_change_07_2020+lower_ci95*rel_change_pandemic_1_sd_err

# August 2020 - 08 upper
acs_male_65_above_relative_change_08_2020_upper_ci = acs_male_65_above_relative_change_08_2020+upper_ci95*rel_change_pandemic_1_sd_err

# August 2020 - 08 lower 
acs_male_65_above_relative_change_08_2020_lower_ci  = acs_male_65_above_relative_change_08_2020+lower_ci95*rel_change_pandemic_1_sd_err

# September 2020 - 09 upper
acs_male_65_above_relative_change_09_2020_upper_ci = acs_male_65_above_relative_change_09_2020+upper_ci95*rel_change_pandemic_1_sd_err

# September 2020 - 09 lower 
acs_male_65_above_relative_change_09_2020_lower_ci  = acs_male_65_above_relative_change_09_2020+lower_ci95*rel_change_pandemic_1_sd_err

# October 2020 - 10 upper
acs_male_65_above_relative_change_10_2020_upper_ci = acs_male_65_above_relative_change_10_2020+upper_ci95*rel_change_pandemic_1_sd_err

# October 2020 - 10 lower 
acs_male_65_above_relative_change_10_2020_lower_ci  = acs_male_65_above_relative_change_10_2020+lower_ci95*rel_change_pandemic_1_sd_err

# November 2020 - 11 upper
acs_male_65_above_relative_change_11_2020_upper_ci = acs_male_65_above_relative_change_11_2020+upper_ci95*rel_change_pandemic_1_sd_err

# November 2020 - 11 lower 
acs_male_65_above_relative_change_11_2020_lower_ci  = acs_male_65_above_relative_change_11_2020+lower_ci95*rel_change_pandemic_1_sd_err

# December 2020 - 12 upper
acs_male_65_above_relative_change_12_2020_upper_ci = acs_male_65_above_relative_change_12_2020+upper_ci95*rel_change_pandemic_1_sd_err

# December 2020 - 12 lower 
acs_male_65_above_relative_change_12_2020_lower_ci  = acs_male_65_above_relative_change_12_2020+lower_ci95*rel_change_pandemic_1_sd_err

# January 2021 - 01 upper
acs_male_65_above_relative_change_01_2021_upper_ci = acs_male_65_above_relative_change_01_2021+upper_ci95*rel_change_pandemic_1_sd_err

# January 2021 - 01 lower 
acs_male_65_above_relative_change_01_2021_lower_ci  = acs_male_65_above_relative_change_01_2021+lower_ci95*rel_change_pandemic_1_sd_err

############################################################################################################################
# Relative change confidence interval for the months - pandemic period 2
######################################################################### 

# February 2021 - 02 upper
acs_male_65_above_relative_change_02_2021_upper_ci = acs_male_65_above_relative_change_02_2021+upper_ci95*rel_change_pandemic_2_sd_err

# February 2021 - 02 lower 
acs_male_65_above_relative_change_02_2021_lower_ci  = acs_male_65_above_relative_change_02_2021+lower_ci95*rel_change_pandemic_2_sd_err

# March 2021 - 03 upper
acs_male_65_above_relative_change_03_2021_upper_ci = acs_male_65_above_relative_change_03_2021+upper_ci95*rel_change_pandemic_2_sd_err

# March 2021 - 03 lower 
acs_male_65_above_relative_change_03_2021_lower_ci  = acs_male_65_above_relative_change_03_2021+lower_ci95*rel_change_pandemic_2_sd_err

# April 2021 - 04 upper
acs_male_65_above_relative_change_04_2021_upper_ci = acs_male_65_above_relative_change_04_2021+upper_ci95*rel_change_pandemic_2_sd_err

# April  2021 - 04 lower 
acs_male_65_above_relative_change_04_2021_lower_ci  = acs_male_65_above_relative_change_04_2021+lower_ci95*rel_change_pandemic_2_sd_err


# May 2021 - 05 upper
acs_male_65_above_relative_change_05_2021_upper_ci = acs_male_65_above_relative_change_05_2021+upper_ci95*rel_change_pandemic_2_sd_err

# May  2021 - 05 lower 
acs_male_65_above_relative_change_05_2021_lower_ci  = acs_male_65_above_relative_change_05_2021+lower_ci95*rel_change_pandemic_2_sd_err

# June 2021 - 06 upper
acs_male_65_above_relative_change_06_2021_upper_ci = acs_male_65_above_relative_change_06_2021+upper_ci95*rel_change_pandemic_2_sd_err

# June  2021 - 06 lower 
acs_male_65_above_relative_change_06_2021_lower_ci  = acs_male_65_above_relative_change_06_2021+lower_ci95*rel_change_pandemic_2_sd_err

# July 2021 - 07 upper
acs_male_65_above_relative_change_07_2021_upper_ci = acs_male_65_above_relative_change_07_2021+upper_ci95*rel_change_pandemic_2_sd_err

# July  2021 - 07 lower 
acs_male_65_above_relative_change_07_2021_lower_ci  = acs_male_65_above_relative_change_07_2021+lower_ci95*rel_change_pandemic_2_sd_err

# August 2021 - 08 upper
acs_male_65_above_relative_change_08_2021_upper_ci = acs_male_65_above_relative_change_08_2021+upper_ci95*rel_change_pandemic_2_sd_err

# August  2021 - 08 lower 
acs_male_65_above_relative_change_08_2021_lower_ci  = acs_male_65_above_relative_change_08_2021+lower_ci95*rel_change_pandemic_2_sd_err

# September 2021 - 09 upper
acs_male_65_above_relative_change_09_2021_upper_ci = acs_male_65_above_relative_change_09_2021+upper_ci95*rel_change_pandemic_2_sd_err

# September  2021 - 09 lower 
acs_male_65_above_relative_change_09_2021_lower_ci  = acs_male_65_above_relative_change_09_2021+lower_ci95*rel_change_pandemic_2_sd_err

# October 2021 - 10 upper
acs_male_65_above_relative_change_10_2021_upper_ci = acs_male_65_above_relative_change_10_2021+upper_ci95*rel_change_pandemic_2_sd_err

# October  2021 - 10 lower 
acs_male_65_above_relative_change_10_2021_lower_ci  = acs_male_65_above_relative_change_10_2021+lower_ci95*rel_change_pandemic_2_sd_err

# November 2021 - 11 upper
acs_male_65_above_relative_change_11_2021_upper_ci = acs_male_65_above_relative_change_11_2021+upper_ci95*rel_change_pandemic_2_sd_err

# November  2021 - 11 lower 
acs_male_65_above_relative_change_11_2021_lower_ci  = acs_male_65_above_relative_change_11_2021+lower_ci95*rel_change_pandemic_2_sd_err

# December 2021 - 12 upper
acs_male_65_above_relative_change_12_2021_upper_ci = acs_male_65_above_relative_change_12_2021+upper_ci95*rel_change_pandemic_2_sd_err

# December  2021 - 12 lower 
acs_male_65_above_relative_change_12_2021_lower_ci  = acs_male_65_above_relative_change_12_2021+lower_ci95*rel_change_pandemic_2_sd_err

#################################################################################################################
# Make a data frame of the results
###################################

acs_male_65_above_results_2_for_every_month_df = data.frame(months_from_march2020 = c("March 2020", "April 2020", "May 2020", "June 2020", "July 2020", "August 2020", "September 2020", "October 2020",
                                                                                      "November 2020", "December 2020", "Januar 2021","February 2021","March 2021", "April 2021", "May 2021", "June 2021", 
                                                                                      "July 2021", "August 2021", "September 2021", "October 2021", "November 2021","December 2021"),   
                                                            
                                                            
                                                            absolute_change = c(acs_male_65_above_absolute_change_03_2020,acs_male_65_above_absolute_change_04_2020,acs_male_65_above_absolute_change_05_2020,
                                                                                acs_male_65_above_absolute_change_06_2020,acs_male_65_above_absolute_change_07_2020,acs_male_65_above_absolute_change_08_2020,
                                                                                acs_male_65_above_absolute_change_09_2020,acs_male_65_above_absolute_change_10_2020,acs_male_65_above_absolute_change_11_2020,
                                                                                acs_male_65_above_absolute_change_12_2020,acs_male_65_above_absolute_change_01_2021,acs_male_65_above_absolute_change_02_2021,
                                                                                acs_male_65_above_absolute_change_03_2021,acs_male_65_above_absolute_change_04_2021,acs_male_65_above_absolute_change_05_2021,
                                                                                acs_male_65_above_absolute_change_06_2021,acs_male_65_above_absolute_change_07_2021,acs_male_65_above_absolute_change_08_2021,
                                                                                acs_male_65_above_absolute_change_09_2021,acs_male_65_above_absolute_change_10_2021,acs_male_65_above_absolute_change_11_2021,
                                                                                acs_male_65_above_absolute_change_12_2021), 
                                                            
                                                            absolute_change_lower_95_ci = c(acs_male_65_above_absolute_change_03_2020_lower_ci,acs_male_65_above_absolute_change_04_2020_lower_ci,acs_male_65_above_absolute_change_05_2020_lower_ci,
                                                                                            acs_male_65_above_absolute_change_06_2020_lower_ci, acs_male_65_above_absolute_change_07_2020_lower_ci,acs_male_65_above_absolute_change_08_2020_lower_ci,
                                                                                            acs_male_65_above_absolute_change_09_2020_lower_ci,acs_male_65_above_absolute_change_10_2020_lower_ci,acs_male_65_above_absolute_change_11_2020_lower_ci,
                                                                                            acs_male_65_above_absolute_change_12_2020_lower_ci,acs_male_65_above_absolute_change_01_2021_lower_ci, acs_male_65_above_absolute_change_02_2021_lower_ci,
                                                                                            acs_male_65_above_absolute_change_03_2021_lower_ci, acs_male_65_above_absolute_change_04_2021_lower_ci, acs_male_65_above_absolute_change_05_2021_lower_ci,
                                                                                            acs_male_65_above_absolute_change_06_2021_lower_ci, acs_male_65_above_absolute_change_07_2021_lower_ci, acs_male_65_above_absolute_change_08_2021_lower_ci,
                                                                                            acs_male_65_above_absolute_change_09_2021_lower_ci, acs_male_65_above_absolute_change_10_2021_lower_ci, acs_male_65_above_absolute_change_11_2021_lower_ci, 
                                                                                            acs_male_65_above_absolute_change_12_2021_lower_ci),
                                                            
                                                            absolute_change_upper_95_ci = c(acs_male_65_above_absolute_change_03_2020_upper_ci,acs_male_65_above_absolute_change_04_2020_upper_ci,acs_male_65_above_absolute_change_05_2020_upper_ci,
                                                                                            acs_male_65_above_absolute_change_06_2020_upper_ci,acs_male_65_above_absolute_change_07_2020_upper_ci,acs_male_65_above_absolute_change_08_2020_upper_ci,
                                                                                            acs_male_65_above_absolute_change_09_2020_upper_ci,acs_male_65_above_absolute_change_10_2020_upper_ci,acs_male_65_above_absolute_change_11_2020_upper_ci,
                                                                                            acs_male_65_above_absolute_change_12_2020_upper_ci,acs_male_65_above_absolute_change_01_2021_upper_ci,acs_male_65_above_absolute_change_02_2021_upper_ci,
                                                                                            acs_male_65_above_absolute_change_03_2021_upper_ci,acs_male_65_above_absolute_change_04_2021_upper_ci,acs_male_65_above_absolute_change_05_2021_upper_ci,
                                                                                            acs_male_65_above_absolute_change_06_2021_upper_ci,acs_male_65_above_absolute_change_07_2021_upper_ci,acs_male_65_above_absolute_change_08_2021_upper_ci,
                                                                                            acs_male_65_above_absolute_change_09_2021_upper_ci,acs_male_65_above_absolute_change_10_2021_upper_ci,acs_male_65_above_absolute_change_11_2021_upper_ci,
                                                                                            acs_male_65_above_absolute_change_12_2021_upper_ci),
                                                            
                                                            relative_change = c(acs_male_65_above_relative_change_03_2020,acs_male_65_above_relative_change_04_2020,acs_male_65_above_relative_change_05_2020,
                                                                                acs_male_65_above_relative_change_06_2020,acs_male_65_above_relative_change_07_2020,acs_male_65_above_relative_change_08_2020,
                                                                                acs_male_65_above_relative_change_09_2020,acs_male_65_above_relative_change_10_2020,acs_male_65_above_relative_change_11_2020,
                                                                                acs_male_65_above_relative_change_12_2020,acs_male_65_above_relative_change_01_2021,acs_male_65_above_relative_change_02_2021,
                                                                                acs_male_65_above_relative_change_03_2021,acs_male_65_above_relative_change_04_2021,acs_male_65_above_relative_change_05_2021,
                                                                                acs_male_65_above_relative_change_06_2021,acs_male_65_above_relative_change_07_2021,acs_male_65_above_relative_change_08_2021,
                                                                                acs_male_65_above_relative_change_09_2021,acs_male_65_above_relative_change_10_2021,acs_male_65_above_relative_change_11_2021,
                                                                                acs_male_65_above_relative_change_12_2021),
                                                            
                                                            relative_change_lower_95_ci = c(acs_male_65_above_relative_change_03_2020_lower_ci,acs_male_65_above_relative_change_04_2020_lower_ci,acs_male_65_above_relative_change_05_2020_lower_ci,
                                                                                            acs_male_65_above_relative_change_06_2020_lower_ci, acs_male_65_above_relative_change_07_2020_lower_ci,acs_male_65_above_relative_change_08_2020_lower_ci,
                                                                                            acs_male_65_above_relative_change_09_2020_lower_ci,acs_male_65_above_relative_change_10_2020_lower_ci,acs_male_65_above_relative_change_11_2020_lower_ci,
                                                                                            acs_male_65_above_relative_change_12_2020_lower_ci,acs_male_65_above_relative_change_01_2021_lower_ci, acs_male_65_above_relative_change_02_2021_lower_ci,
                                                                                            acs_male_65_above_relative_change_03_2021_lower_ci, acs_male_65_above_relative_change_04_2021_lower_ci, acs_male_65_above_relative_change_05_2021_lower_ci,
                                                                                            acs_male_65_above_relative_change_06_2021_lower_ci, acs_male_65_above_relative_change_07_2021_lower_ci, acs_male_65_above_relative_change_08_2021_lower_ci,
                                                                                            acs_male_65_above_relative_change_09_2021_lower_ci, acs_male_65_above_relative_change_10_2021_lower_ci, acs_male_65_above_relative_change_11_2021_lower_ci, 
                                                                                            acs_male_65_above_relative_change_12_2021_lower_ci),
                                                            
                                                            relative_change_upper_95_ci = c(acs_male_65_above_relative_change_03_2020_upper_ci,acs_male_65_above_relative_change_04_2020_upper_ci,acs_male_65_above_relative_change_05_2020_upper_ci,
                                                                                            acs_male_65_above_relative_change_06_2020_upper_ci,acs_male_65_above_relative_change_07_2020_upper_ci,acs_male_65_above_relative_change_08_2020_upper_ci,
                                                                                            acs_male_65_above_relative_change_09_2020_upper_ci,acs_male_65_above_relative_change_10_2020_upper_ci,acs_male_65_above_relative_change_11_2020_upper_ci,
                                                                                            acs_male_65_above_relative_change_12_2020_upper_ci,acs_male_65_above_relative_change_01_2021_upper_ci,acs_male_65_above_relative_change_02_2021_upper_ci,
                                                                                            acs_male_65_above_relative_change_03_2021_upper_ci,acs_male_65_above_relative_change_04_2021_upper_ci,acs_male_65_above_relative_change_05_2021_upper_ci,
                                                                                            acs_male_65_above_relative_change_06_2021_upper_ci,acs_male_65_above_relative_change_07_2021_upper_ci,acs_male_65_above_relative_change_08_2021_upper_ci,
                                                                                            acs_male_65_above_relative_change_09_2021_upper_ci,acs_male_65_above_relative_change_10_2021_upper_ci,acs_male_65_above_relative_change_11_2021_upper_ci,
                                                                                            acs_male_65_above_relative_change_12_2021_upper_ci),
                                                            
                                                            acs_male_65_above_absolute_change_in_visits = c(acs_male_65_above_absolute_change_in_people_03_2020,acs_male_65_above_absolute_change_in_people_04_2020,acs_male_65_above_absolute_change_in_people_05_2020,
                                                                                                            acs_male_65_above_absolute_change_in_people_06_2020,acs_male_65_above_absolute_change_in_people_07_2020,acs_male_65_above_absolute_change_in_people_08_2020,
                                                                                                            acs_male_65_above_absolute_change_in_people_09_2020,acs_male_65_above_absolute_change_in_people_10_2020,acs_male_65_above_absolute_change_in_people_11_2020,
                                                                                                            acs_male_65_above_absolute_change_in_people_12_2020,acs_male_65_above_absolute_change_in_people_01_2021, acs_male_65_above_absolute_change_in_people_02_2021,
                                                                                                            acs_male_65_above_absolute_change_in_people_03_2021,acs_male_65_above_absolute_change_in_people_04_2021,acs_male_65_above_absolute_change_in_people_05_2021,
                                                                                                            acs_male_65_above_absolute_change_in_people_06_2021,acs_male_65_above_absolute_change_in_people_07_2021,acs_male_65_above_absolute_change_in_people_08_2021,
                                                                                                            acs_male_65_above_absolute_change_in_people_09_2021,acs_male_65_above_absolute_change_in_people_10_2021,acs_male_65_above_absolute_change_in_people_11_2021,
                                                                                                            acs_male_65_above_absolute_change_in_people_12_2021))




library(writexl)
write_xlsx(acs_male_65_above_results_2_for_every_month_df  , "C:/PROGRAMS/data/tables/acs_male_65_above_results_2_for_every_month_df.xlsx")  

###########################################################

# add a months column to the results of change difference file.
acs_male_65_above_results_2_for_every_month_df$time_by_month = seq(from = ymd("2020-03-01"), to = ymd("2021-12-31"), by="months")

# build graph of change percentage

acs_male_65_above_change_perc_graph  <- ggplot(data = acs_male_65_above_results_2_for_every_month_df, aes(x=time_by_month, y=relative_change))+
  
  #control the x-axis
  scale_x_date(date_breaks = "1 month", labels=date_format("%b-%y"))+
  
  # #control and limit the x-axis and y-axis 
  #coord_cartesian(xlim = c(as.Date.character("2020-03-01"),as.Date.character("2021-12-31")),expand = F)+
  
  
  geom_col(aes(fill =relative_change > 0), alpha = 0.7)+
  geom_text(aes( y = relative_change, label = paste0(round(relative_change,1),"%")), size = 2.5, vjust = -0.5)+
  theme_bw()+
  labs(title ="Estimated results of the ITS model for ACS in males ages 65 and above rate per 100,000, monthly relative change percentage\n")+
  labs(x ="\nMonth",y ="Relative monthly change %")+
  scale_fill_manual(name  ="", values =c( "brown","purple"),labels =c("Decrease", "Increase"))+
  #add angle to x-axis
  theme(axis.text.x = element_text(angle = 90, hjust = 0.5))+
  
  #put the text of the dates in the x-axis to more in the middle
  theme(axis.text.x = element_text(vjust = 0.5))+
  
  #change the position of the title 1 = right align, defult is left, 0.5 = center
  theme(plot.title = element_text(hjust = 0.5))+ 
  
  # add intervention lines at 15 Jab 2020 and 15 Jan 2021
  geom_vline(xintercept =as.Date.character("2020-2-15"), linetype= "dashed", size = 0.9)+
  geom_vline(xintercept =as.Date.character("2021-1-15"),linetype= "dashed",size=0.9 )

acs_male_65_above_change_perc_graph

