# Create by Alex Kagan

Sys.setlocale("LC_TIME", "English") # for the x-axis will be in English

library(ggplot2)
library(ggthemes)
library(hrbrthemes)
library(tidyverse)
library(zoo)

pop2020 = 9215100
pop2021 = 9371400 

fctrate100 = 100000


# the original file was downloaded from:https://ourworldindata.org/coronavirus/country/israel
d_cases <- read_excel("C:/PROGRAMS/WorkingDir/Erdata/Practice/Death_covid-19_a.xlsx")
View(d_cases)


# to remove the na and make it numeric 
d_cases[is.na(d_cases)] <- 0 

d_cases$date = ymd(d_cases$date)
d_cases$yearly = format(as.Date(d_cases$date), "%Y")
d_cases$monthly1 = as.yearmon(d_cases$date, "%b %y")
d_cases$monthly2 = format(d_cases$date, "%b %y")
d_cases$monthly3 = format(d_cases$date, "%b-%Y")
d_cases$weekly = format(as.Date(d_cases$date), "%V")


monthy_agg = d_cases %>% group_by(yearly,monthly1) %>%
  summarise(monthly_ag1 = sum(new_deaths))
monthy_agg$month_year = strftime(monthy_agg$monthly1 , "%b %y")
monthy_agg$date = seq(as.Date("2020/3/1"), as.Date("2021/12/31"), by = "month")


monthy_agg$rate = case_when(
  monthy_agg$yearly == 2020 ~ monthy_agg$monthly_ag1/pop2020*fctrate100,
  monthy_agg$yearly == 2021 ~ monthy_agg$monthly_ag1/pop2021*fctrate100
)



################
# the graph
###############


p=ggplot(data = monthy_agg, aes(x=date, y=rate))+
  scale_y_continuous(breaks=seq(0,20,1)) + 
  theme_clean()+
  
  geom_area(fill="steelblue",color = "black")+
  
  scale_x_date(date_breaks = "months" , date_labels = "%b-%y")+
  
  labs(y="COVID-19 deaths rate\n per 100,000 people/month\n", x="Time", title = "Monthly COVID-19 fatality rates per 100,000 people, study periods and major pandemic events in Israel\n")+
  coord_cartesian(xlim =c(as.Date("2019-11-1"), as.Date("2021-12-10")), ylim = c(0,22),expand =F)+
  
  # the interruption line
  geom_vline(xintercept = c(as.Date("2020-03-12"), as.Date("2021-02-7")) ,linetype="longdash", linewidth = 1, color = "red")+
  
  
  #first lockdown
  geom_rect(xmin = as.Date("2020-03-25"), xmax = as.Date("2020-5-4"),ymin = 0,ymax = 3.5, alpha = 0.01, fill = "grey")+
  
  # label for first lockdown
  geom_label(
    label="1st 
    National Lockdown", 
    x=as.Date("2020-04-15"),
    y=3.5,
    label.padding = unit(0.15, "lines"), # Rectangle size around label
    label.size = 0.1,
    color = "black",
    fill="white"
  ) +
  
  
  #second lockdown
  geom_rect(xmin = as.Date("2020-09-18"), xmax = as.Date("2020-10-18"),ymin = 0,ymax = 12, alpha = 0.01, fill = "grey")+
  
  # label for secomd lockdown
  geom_label(
    label="2nd 
    National Lockdown", 
    x=as.Date("2020-10-1"),
    y=12,
    label.padding = unit(0.15, "lines"), # Rectangle size around label
    label.size = 0.1,
    color = "black",
    fill="white"
  ) +
  
  
  #third lockdown
  geom_rect(xmin = as.Date("2020-12-27"), xmax = as.Date("2021-02-7"),ymin = 0,ymax = 18.5, alpha = 0.01, fill = "grey")+
  
  # label for secomd lockdown
  geom_label(
    label="3rd 
    National Lockdown", 
    x=as.Date("2021-01-17"),
    y=18.5,
    label.padding = unit(0.15, "lines"), # Rectangle size around label
    label.size = 0.1,
    color = "black",
    fill="white"
  )+  
  
  
  #for the first interruption curve
  geom_curve(aes(x =  as.Date("2020-05-1"), y = 14.5, xend = as.Date("2020-3-12"), yend = 17),
             arrow = arrow(length = unit(0.03, "npc"), type="closed"), 
             colour = "black", size = 1, curvature = +0.3, angle = -90) + 
  
  # label for the first interpreted point
  geom_label(
    label=" 1st Interruption - March, 2020
    The government initiated the restrictions
    and emergency state regulations", 
    x=as.Date("2020-5-15"),
    y=15,
    label.padding = unit(0.15, "lines"), # Rectangle size around label
    label.size = 0.1,
    color = "black",
    fill="Khaki")+
  
  # gegment for the vaccination campaign 
  geom_segment(x=as.Date("2020-12-27"),xend=as.Date("2020-12-27"),y=0,yend=17, col="green", linetype = "dashed", size =1.5)+
  
  # label for vaccination campaign 
  geom_label(
    label="The vaccination
    campaign began", 
    x=as.Date("2020-12-2"),
    y=17,
    label.padding = unit(0.15, "lines"), # Rectangle size around label
    label.size = 0.1,
    color = "black",
    fill="LightGreen"
  ) +
  
  
  
  #segment for first the vaccination 
  geom_segment(x=as.Date("2021-02-20"),xend=as.Date("2021-02-20"),y=0,yend=14, col="green", linetype = "dashed", size =1.5)+
  
  # label for 50% of the population
  geom_label(
    label="50% of the population 
    vaccinated with at least
     one dose of vaccine", 
    x=as.Date("2021-3-22"),
    y=14,
    label.padding = unit(0.15, "lines"), # Rectangle size around label
    label.size = 0.1,
    color = "black",
    fill="LightGreen"
  ) + 
  
  
  #segment for the second vaccination 
  geom_segment(x=as.Date("2021-03-9"),xend=as.Date("2021-03-14"),y=0,yend=9, col="green", linetype = "dashed", size =1.5)+
  
  # label for More than 50% vaccinated
  geom_label(
    label="50% of the population
    vaccinated according
    to the protocol", 
    x=as.Date("2021-4-14"),
    y=9,
    label.padding = unit(0.15, "lines"), # Rectangle size around label
    label.size = 0.1,
    color = "black",
    fill="LightGreen"
  ) +
  
  #for the second interruption curve
  geom_curve(aes(x =  as.Date("2021-03-20"), y = 17, xend = as.Date("2021-02-7"), yend = 20),
             arrow = arrow(length = unit(0.03, "npc"), type="closed"), 
             colour = "black", size = 1, curvature = +0.3, angle = -90) + 
  
  
  # label for the second interpreted point
  geom_label(
    label=" 2nd Interruption - February, 2021
     The government's restriction policy has been changed", 
    x=as.Date("2021-05-20"),
    y=17,
    label.padding = unit(0.15, "lines"), # Rectangle size around label
    label.size = 0.1,
    color = "black",
    fill="Khaki")+ 
  
  # points for the months  
  
  # points for the months  
  geom_point(size = 3, color = "blue", shape = 19) +
  
  # Study periods  
  annotate("text",x=as.Date("2019-12-25"), y=21, 
           label=
             "'Pre-pandemic period'\
January 2018 - February 2020\
"
           
           ,size=3.9, color = "black", fontface = "bold") + 
  
  annotate("text",x=as.Date("2020-07-15"), y=21, 
           label=
             "'Pandemic period 1'\
March 2020 - January 2021\
"
           
           ,size=3.9, color = "black", fontface = "bold") + 
  
  
  annotate("text",x=as.Date("2021-07-15"), y=21, label=
             "'Pandemic period 2'\
February 2021 - December 2021\
",size=3.9, color = "black", fontface = "bold")+
  
  #change the position of the title 1 = right align, defult is left, 0.5 = center
  theme(plot.title = element_text(hjust = 0.5))

p

#######################################################################################
# Graph 2: "Daily confirmed COVID-19 deaths (7-day rolling average) and major pandemic events



#Make the rolling 7 day average
d_cases$SMA = rollmean(d_cases$new_deaths, 7, na.pad=TRUE)

#Make the rolling 14 day average
d_cases$SMA14 = rollmean(d_cases$new_deaths, 14, na.pad=TRUE)


p2=ggplot(data = d_cases, aes(x=date, y=SMA))+
  scale_y_continuous(breaks=seq(0,120,10)) + 
  
  geom_area(fill="steelblue",color = "black")+
  scale_x_date(date_breaks = "months" , date_labels = "%b-%y")+
  theme_clean()+
  labs(y="COVID-19 deaths (7-day rolling average)\n", x="Time", title = "Daily confirmed COVID-19 deaths (7-day rolling average) and major pandemic events")+
  coord_cartesian(xlim =c(as.Date("2020-01-15"), as.Date("2021-12-10")), ylim = c(0,75),expand =F)+
  
  geom_vline(xintercept = c(as.Date("2020-01-27"), as.Date("2021-02-7")) ,linetype="longdash", size = 1, color = "red")+
  

  
  
  #first lockdown
  geom_rect(xmin = as.Date("2020-03-25"), xmax = as.Date("2020-5-4"),ymin = 0,ymax = 20, alpha = 0.01, fill = "grey")+
  
  # label for first lockdown
  geom_label(
    label="1st 
    National Lockdown", 
    x=as.Date("2020-04-10"),
    y=20,
    label.padding = unit(0.15, "lines"), # Rectangle size around label
    label.size = 0.1,
    color = "black",
    fill="white"
  ) +
  
  
  
  #second lockdown
  geom_rect(xmin = as.Date("2020-09-18"), xmax = as.Date("2020-10-18"),ymin = 0,ymax = 45, alpha = 0.01, fill = "grey")+
  
  # label for secomd lockdown
  geom_label(
    label="2nd 
    National Lockdown", 
    x=as.Date("2020-10-5"),
    y=45,
    label.padding = unit(0.15, "lines"), # Rectangle size around label
    label.size = 0.1,
    color = "black",
    fill="white"
  ) +
  
  
  
  
  #third lockdown
  geom_rect(xmin = as.Date("2020-12-27"), xmax = as.Date("2021-02-7"),ymin = 0,ymax = 67, alpha = 0.01, fill = "grey")+
  
  # label for secomd lockdown
  geom_label(
    label="3rd 
    National Lockdown", 
    x=as.Date("2021-01-20"),
    y=67,
    label.padding = unit(0.15, "lines"), # Rectangle size around label
    label.size = 0.1,
    color = "black",
    fill="white"
  )+
  
  
  geom_segment(x=as.Date("2020-12-27"),xend=as.Date("2020-12-27"),y=0,yend=60, col="green", linetype = "dashed", size =1.5)+
  
  # label for secomd lockdown
  geom_label(
    label="The vaccination campaign
    launched", 
    x=as.Date("2020-12-7"),
    y=60,
    label.padding = unit(0.15, "lines"), # Rectangle size around label
    label.size = 0.1,
    color = "black",
    fill="LightGreen"
  ) +
  
  geom_segment(x=as.Date("2021-03-01"),xend=as.Date("2021-03-01"),y=0,yend=50, col="green", linetype = "dashed", size =1.5)+
  
  # label for secomd lockdown
  geom_label(
    label="50% of the population 
    vaccinated with at least
     one dose of vaccine", 
    x=as.Date("2021-3-15"),
    y=50,
    label.padding = unit(0.15, "lines"), # Rectangle size around label
    label.size = 0.1,
    color = "black",
    fill="LightGreen"
  ) + 
  
  
  
  
  geom_segment(x=as.Date("2021-04-01"),xend=as.Date("2021-04-01"),y=0,yend=20, col="green", linetype = "dashed", size =1.5)+
  
  # label for secomd lockdown
  geom_label(
    label="More than 50% 
    of the population
    vaccinated according
    to the protocol", 
    x=as.Date("2021-4-15"),
    y=20,
    label.padding = unit(0.15, "lines"), # Rectangle size around label
    label.size = 0.1,
    color = "black",
    fill="LightGreen"
  ) 






p2+   # label for the first interpreted poin
  geom_label(
    label=" 1st interruption point - January 27, 2020
     Israeli Health Minister signed public health order, adding COVID-19 to
      the list of contagious diseases with international importance that requires an immediate report 
     in accordance with the International Health Regulations of WHO", 
    x=as.Date("2020-5-30"),
    y=57,
    label.padding = unit(0.15, "lines"), # Rectangle size around label
    label.size = 0.1,
    color = "black",
    fill="Khaki")+
  
  
  # label for the second interpreted point
  geom_label(
    label=" 2nd interruption point - February 7, 2021
     The goverment of isreal easing the lockdown restrictions", 
    x=as.Date("2021-05-20"),
    y=65,
    label.padding = unit(0.15, "lines"), # Rectangle size around label
    label.size = 0.1,
    color = "black",
    fill="Khaki")


p2

############################################################################