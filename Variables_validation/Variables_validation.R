
library(janitor)
library(nortest)
library(psych)
#################################
# Data validation (from monthly)
#################################

#get the working diretory
getwd() # "D:/Rprogram/Work"

#know what is the library directory
.libPaths()
#[1] "C:/Users/alexander.kagan1/Documents/R/win-library/4.1"
#[2] "C:/Rprogram/library"

#change the library directory to desired directory
.libPaths("C:/Rprogram/library")
.libPaths()
#[1] "C:/Rprogram/library"

#Set the time and dates in English
Sys.setlocale("LC_TIME", "English")


################################
# Emergency Room dataset upload 
# this data is the visit to general hospital mosly by the diagnosis of
# chest pain and acute myocardial infraction in the years 2018-2022. 
###################################

View(erdata)
head(erdata)
tail(erdata)
class(erdata)
nrow(erdata) 
ncol(erdata)  
str(erdata)

################################
names(erdata) # 

#frequency check and duplication and the class of the variables (step 1)
###############################
library(epiDisplay)

# Original visit number
class(erdata$original_visit_number) # output: "character"
sum(duplicated(erdata$original_visit_number)) # check if it's unique variables. 
sum(is.na(erdata$original_visit_number)) #check Na's output: zero Na's.

#patient gk (id of of the visit)
class(erdata$patient_gk) # output: "numeric"
sum(duplicated(erdata$patient_gk)) # check if it's unique variables.
sum(is.na(erdata$patient_gk))# Check Na's output: zero Na's.

#patient id
class(erdata$patient_id) # output: "character"
sum(duplicated(erdata$patient_id)) # to see if id is unique 
sum(is.na(erdata$patient_id))#check Na's - output: zero Na's.


# icd9 first diagnostic_code - the first diagnosis code of the visit. 
class(erdata$icd9_first_diagnostic_code)# output: "character"
count(erdata,"icd9_first_diagnostic_code")# count the frequency of variables 
sum(is.na(erdata$icd9_first_diagnostic_code))#check Na's - output: zero Na's.
barplot(table(erdata$icd9_first_diagnostic_code)) #display by using bar plot chart. 
tab1(erdata$icd9_first_diagnostic_code, sort.group = "decreasing", cum.percent = T) 


# secondary diagnostic code - the secondary diagnostic code. 
class(erdata$icd9_secondary_diagnostic_code)# output: "character"
count(erdata,"icd9_secondary_diagnostic_code")# count the frequency of variables.
sum(is.na(erdata$icd9_secondary_diagnostic_code))#check Na's - output: zero Na's.
barplot(table(erdata$icd9_secondary_diagnostic_code)) #display by using bar plot chart.
tab1(erdata$icd9_secondary_diagnostic_code, sort.group = "decreasing", cum.percent = T) #

dev.off()

library(ggpubr)
#patient age 
class(erdata$patient_age)# output: "numeric"
count(erdata,"patient_age") # check frequency of the age variable. 
sum(is.na(erdata$patient_age))#check Na's - output: zero Na's.
hist(erdata$patient_age, col = "lightblue") #histogram plot of patient age.
plot(density(erdata$patient_age)) # kernel Density plot of patient age. 
tab1(erdata$patient_age) # alternative way to display the patient age variable. 
median(erdata$patient_age) # for all ages the median age is__

ggdensity(erdata$patient_age,
          main = "Density plot of patient age",
          xlab = "Patient age",
          fill = "red")+
  #scale_x_discrete()
  scale_x_continuous(limits = c(24, 120))+
  stat_overlay_normal_density(color = "red", linetype = "dashed")


#density plot with dashed line of the normal distribution. 
ggdensity(erdata$patient_age,
          main = "Density plot of patient age",
          xlab = "Patient age",
          fill = "red",
          color="blue")+
  scale_x_discrete()+
  stat_overlay_normal_density(color = "red", linetype = "dashed", size = 1)


# normality tests for age - from ggpubr package 
ggqqplot(erdata$patient_age,
         palette = c("#0073C2FF"),
         ggtheme = theme_pubclean()) #not look like normal distribution.

#another qqplot of r base need to wait untill load the plot
qqnorm(erdata$patient_age) 

# for createing with a line.
qqline(erdata$patient_age)

# qqplot from car package 
qqPlot(erdata$patient_age)

# shapiro test from different library 
library(rstatix)
shapiro.test(erdata$patient_age) # not work - ask for 3-5000 sample size

#kolmogorov-smirnov test
ks.test(erdata$visit_start_date,erdata$patient_age) # work p-value less than 0.05 - not normal distribution. 

library(nortest)
#shapiro francia test - require a sample between 5-5000  
sf.test(erdata$patient_age)

# anderson-Darling test for normality - work
ad.test(erdata$patient_age) 

# Lilliefors (Kolmogorov-Smirnov) test for normality from the normtest package.
lillie.test(erdata$patient_age) # 

# Cramer-von Mises test for normality 
cvm.test(erdata$patient_age) # 


# skewness test - zero or close to zero indicates symmetry, but symetry is not everything. 
library(moments)
library(plotrix) # to find the std.error
skewness(erdata$patient_age) # # it's very close to zero so it's ok. 

# we check skewness if the confidence interval are fall in zero - if yes it's a good indication to symmetry
skewness(erdata$patient_age)+1.96*std.error(erdata$patient_age) #
skewness(erdata$patient_age)-1.96*std.error(erdata$patient_age) #


library(plotrix)
std.error(erdata$patient_age)


# kurtosis of zero indicates normal distribution
kurtosis(erdata$patient_age) # 

kurtosis(erdata$patient_age)+1.96*std.error(erdata$patient_age) 
kurtosis(erdata$patient_age)-1.96*std.error(erdata$patient_age) 

#summary table
library(rstatix)
get_summary_stats(erdata,patient_age)

# simple summary 
summary(erdata$patient_age)

#to get the percentage table (more accurate is propotion table)
library(janitor)
tabyl(erdata$patient_age)


#box plot
ggboxplot(erdata$patient_age)

# create chest pain variable by age to check the median age
median_age_chest_pain =  erdata %>%
  group_by(year_visit_start, month_year_visit_start,chest_pain_char_desc, patient_age) %>%
  summarise(chest_pain_by_age = n())
median_age_chest_pain = drop_na(median_age_chest_pain)

median(median_age_chest_pain$patient_age) #


# create MI variable by age to check the median age
median_age_MI =  erdata %>%
  group_by(year_visit_start, month_year_visit_start,myocardial_char_desc, patient_age) %>%
  summarise(MI_by_age = n())
median_age_MI  = drop_na(median_age_MI )

median(median_age_MI $patient_age) #
tab1(median_age_MI$patient_age)

# create STEMI variable by age to check the median age
median_age_STEMI =  erdata %>%
  group_by(year_visit_start, month_year_visit_start,STEMI_char_desc, patient_age) %>%
  summarise(STEMI_by_age = n())
median_age_STEMI  = drop_na(median_age_STEMI)

median(median_age_STEMI$patient_age) # 

# create NSTEMI variable by age to check the median age
median_age_NSTEMI =  erdata %>%
  group_by(year_visit_start, month_year_visit_start,NSTEMI_char_desc, patient_age) %>%
  summarise(NSTEMI_by_age = n())
median_age_NSTEMI  = drop_na(median_age_NSTEMI)

median(median_age_NSTEMI$patient_age) # 

library(psych)
general_chest_pain = dplyr::select(erdata, chest_pain_non_acs_char_desc ,patient_age)
general_chest_pain  = drop_na(general_chest_pain ) 
summary(general_chest_pain )
psych::describe(general_chest_pain ) 
tab1(general_chest_pain$patient_age)
summ(general_chest_pain$patient_age)

general_acs = dplyr::select(erdata, ACS_char_desc ,patient_age) # 
general_acs = drop_na(general_acs) 
summary(general_acs)
describe(general_acs)

Boxplot(general_acs$patient_age)
boxplot(general_acs$patient_age)
tab1(general_acs$patient_age)

# institute mbr code. code of the hospital.
class(erdata$institute_mbr_code)# output: "character"
count(erdata,"institute_mbr_code") #
sum(is.na(erdata$institute_mbr_code))#check Na's - output: zero Na's. 
tab1(erdata$institute_mbr_code) # see the frequency and also the percentage. 
tab1(erdata$institute_mbr_code, sort.group = "decreasing", cum.percent = T )


#patient city code
class(erdata$patient_city_code)# output: "character"
count(erdata,"patient_city_code") # check the cities 
sum(is.na(erdata$patient_city_code))#check Na's - output: zero Na's. 
tab1(erdata$patient_city_code, sort.group = "decreasing", cum.percent = T )
tab1(erdata$city_eng_desc , sort.group = "decreasing")
####################################################
# order all the data frame by time chronologically
###################################################

head(erdata$visit_start_date) # view from what period the dates start in data frame. so i saw in not by order. need to order it. 


# if the count function doesn't work detach the dplyr package. 
#patient visit reason code - for example disease, accident
class(erdata$patient_visit_reason_code)# output: "character"
sum(is.na(erdata$patient_visit_reason_code))#check Na's - output: zero Na's.
count(erdata,"patient_visit_reason_code") # 
tab1(erdata$patient_visit_reason_code, sort.group = "decreasing", cum.percent = T ) #

#patient_release_type_code - the destination of the patient release (hospitalization, death, release home)
class(erdata$patient_release_type_code)# output: "character"
sum(is.na(erdata$patient_release_type_code))#check Na's - output: zero Na's. 
count(erdata,"patient_release_type_code")#
tab1(erdata$patient_release_type_code, sort.group = "decreasing", cum.percent = T )  


# the code department of patient admission in the hospital. 
class(erdata$admission_dept_code)# output "numeric"
count(erdata,"admission_dept_code") # 
sum(is.na(erdata$admission_dept_code))#check Na's - output: zero Na's.
tab1(erdata$admission_dept_code, sort.group = "decreasing", cum.percent = T )

# source hospital
class(erdata$source_institute)# output: "character"
count(erdata,"source_institute") #
sum(is.na(erdata$source_institute))#check Na's
tab1(erdata$source_institute, sort.group = "decreasing", cum.percent = T )

#Destination_hospital
class(erdata$destination_institute)# output: "character"
count(erdata,"destination_institute") # looks ok, most of the values is na. maybe because wasn't transfers to other hospital.
sum(is.na(erdata$destination_institute))#check Na's
tab1(erdata$destination_institute, sort.group = "decreasing", cum.percent = T )

# Birth country code
class(erdata$birth_country_code)# output: "character"
count(erdata, "birth_country_code") # looks ok europe, asia, south and cental america, north america, Israel. 
sum(is.na(erdata$birth_country_code))#check Na's
tab1(erdata$birth_country_code, sort.group = "decreasing", cum.percent = T ) # most of the people is from Israel


# nation code
class(erdata$nation_code) #output "numeric"
count(erdata, "nation_code") # 
sum(is.na(erdata$nation_code))#check Na's - output: 0 Na's.
tab1(erdata$nation_code, sort.group = "decreasing", cum.percent = T )

# nation description of the code
class(erdata$nation_desc) #output:"character"
count(erdata, "nation_desc") # looks fine.
sum(is.na(erdata$nation_desc))#check Na's s.
tab1(erdata$nation_desc, sort.group = "decreasing", cum.percent = T ) # most of the obs is un-known (65%)

# gender code (1 - male, 2 - female)
class(erdata$gender_code) #output "numeric"
count(erdata, "gender_code") # looks fine. 
sum(is.na(erdata$gender_code))#check Na's - output: 0 Na's
plot(density(erdata$gender_code)) # kernel Density.
tab1(erdata$gender_code, sort.group = "decreasing", cum.percent = T )

##################################################################################################################
# gender description 
################################################################################################################
class(erdata$gender_desc) #output:"character"
count(erdata, "gender_desc") # looks fine.
sum(is.na(erdata$gender_desc))#check Na's - output: 0 Na's
tab1(erdata$gender_desc1, sort.group = "decreasing", cum.percent = T ) 

tab1(erdata$gender_desc1, sort.group = "decreasing", cum.percent = T )

aggregate.plot(erdata, by = gender_desc1) # don't know if it's work.

# data witout na because i notice it gives error of na. 
erdata2=erdata[!is.na(erdata$gender_desc1),]

# gender tesrs for normality distrubution - we reject the normal distribution of gender.


# normality tests
# Anderson test for normality 
ad.test(erdata$gender_desc1=="Male") # 
ad.test(erdata$gender_desc1=="Female")#


# Lilliefors (Kolmogorov-Smirnov) test for normality from the normtest package.
lillie.test(erdata$gender_desc1=="Male") 
lillie.test(erdata$gender_desc1=="Female") 

#################################################################################################
#divide the data to female and male because it give some NA and then errors.
#################################################################################################
#data of female 
erfemale=erdata2 %>%
  filter(gender_desc1=="Female")


#data of male 
ermale=erdata2 %>%
  filter(gender_desc1=="Male")


# qqplot for male - from ggpubr package - work 
ggqqplot(ermale, x="patient_age",
         color = "gender_desc1",
         ggtheme = theme_pubclean()) #


# qqplot for female - from ggpubr package - work 
ggqqplot(erfemale, x="patient_age",
         color = "gender_desc1",
         palette ="#0073C2FF",
         ggtheme = theme_pubclean()) #

# histogram for both female and male. 
# histogram for gender by age in x-axes. work
ggplot(erdata2, aes(x= patient_age))+
  geom_histogram(aes(color=gender_desc1,fill=gender_desc1),
                 position = "identity",bins = 30, alpha=0.5)+
  scale_color_manual(values = c("#00AFBB","#E7B800")) 



#can't permforme skewnees and kurtosis test - it's give error - need a numeric variable  

# variance test for the gender. they have the same variance
var.test(erdata$gender_desc1=="Male", erdata$gender_desc1=="Female") #
var(erdata$gender_desc1=="Male", erdata$gender_desc1=="Female", na.rm = T) #
var(erdata$gender_desc1=="Male", na.rm = T)  
var(erdata$gender_desc1=="Female", na.rm = T)  



# box plot 
boxplot(patient_age~gender_desc1, data = erdata)

#levene test for the variance between the group
leveneTest(erdata$patient_age ~ erdata$gender_desc1) # 

# density plot for male - be ready for long time of waiting and loading better not to activate. 
ggdensity(ermale, x="patient_age",
          add = "mean", rug = T,
          color = "gender_desc1", fill="gender_desc1",
          alpha = 0.5,
          palette = c("#0073c2FF","#FC4E07"))+ 
  scale_x_discrete()+
  stat_overlay_normal_density(color = "red", linetype = "dashed", size = 1)


#density plot with dashed line of the normal distribution. 
ggdensity(ermale$gender_desc1,
          main = "Density plot of patient age",
          xlab = "Male",
          fill = "red",
          color="blue")+
  scale_x_discrete()+
  stat_overlay_normal_density(color = "red", linetype = "dashed", size = 1)


# qqplot for a group - from ggpubr package - work 
ggqqplot(erdata2, x="patient_age",
         color = "gender_desc1",
         palette = c("#0073C2FF", "#FC4E07"),
         ggtheme = theme_pubclean()) #not look like normal distribution.

###############################################################################################################

sum(is.na(erdata$visit_start_date))#check Na's - output: 0 Na's
tab1(erdata$visit_start_date, cum.percent = T )
hist(erdata$visit_start_date, "months")
hist(erdata$visit_start_date, "weeks")

#give count number of obs in every month. 
table(format(erdata1$visit_start_date, "%B-%Y")) # not ordered.

#give us a plot of with count number of obs in every month. 
plot(table(format(erdata$visit_start_date, "%b-%Y"))) # not sorted plot - because format function not keep the order. in order to make in order we need to use zoo object of date. 



class(erdata$visit_end_date) #output: "Date"
count(erdata, "visit_end_date")
sum(is.na(erdata$visit_end_date))#check Na's 
tab1(erdata$visit_end_date, cum.percent = T )



class(erdata$visit_start_time) #output: "hms"      "difftime"
sum(is.na(erdata$visit_start_time))#check Na's - output: 0 Na's


class(erdata$visit_end_time) #output: "hms"      "difftime"

sum(is.na(erdata$visit_end_time))#check Na's - output: 0 Na's


# how the data is look like, with hist function charts. by piloting the visit_start_date variable
hist(erdata$visit_start_date,  "days")
hist(erdata$visit_start_date,  "weeks")
hist(erdata$visit_start_date,  "months")
hist(erdata$visit_start_date,  "quarters")
hist(erdata$visit_start_date,  "years")



class(erdata$visit_hours_difference) # ddifftime
tail(erdata$visit_end_datehour)
head(erdata$visit_end_datehour)


#working and manipulating dates
#########################################
library(lubridate)
library(chron)
library(plyr)
library(dplyr)
library(tidyr)

#check how many non-na values has in chest pain diagnosis:
sum(!is.na(erdata$chest_pain_diag)) #

# check the percentage from all - chest pain
sum(!is.na(erdata$chest_pain_diag)) /nrow(erdata) *100  #


#check how many non-na values has in myocardial diagnosis:
sum(!is.na(erdata$myocardial_diag)) #  

count(erdata, 'myocardial_char_desc')
tab1(erdata$myocardial_char_desc, sort.group = "decreasing", cum.percent = T)

# check the percentage of mi
sum(!is.na(erdata$myocardial_diag))/nrow(erdata)*100 

library(stringr)
###############################################
# check how many times we have the char "41051
###############################################

sum(str_count(erdata1$icd9_first_diagnostic_code, pattern = "41000")) #[1] 133
sum(str_count(erdata1$icd9_secondary_diagnostic_code, pattern = "41000"))

sum(str_count(erdata$merged_diag, pattern = "41000"))

sum(grepl("41051",erdata1$icd9_first_diagnostic_code)) #[1] 133
#####################################################################################


# STEMI 
sum(!is.na(erdata$STEMI)) # 5891
sum(!is.na(erdata$STEMI))/nrow(erdata) *100 # 

# NSTEMI 
sum(!is.na(erdata$NSTEMI)) #  16653
sum(!is.na(erdata$NSTEMI))/nrow(erdata) *100 # 



# for percentage display
tab1(erdata$age_grp,decimal = 2,  bar.values = "percent", main = "Visits by age groups",xlab = "Age groups"  , ylab = "Percent")

# for the frequency numbers display 
tab1(erdata$age_grp, main = "Visits by age groups",xlab = "Age groups"  , ylab = "Frequency")


tab1(erdata$patient_releas_desc, sort.group = "decreasing")

barplot(erdata$patient_releas_desc)


table(erdata$region_code) # it's not show the count for NA 
tab1(erdata$region_code, sort.group = "decreasing")
tab1(erdata$region_code_eng_desc, sort.group = "decreasing")
tab1(erdata$region_code_eng_desc,sort.group = "increasing")
sum(table(erdata$region_code))

tab1(erdata$myocardial_diag, sort.group = "decreasing", cum.percent = T)
#####################################################################



#################################################################################
# test normality for ciry of "Rural or small town"  - we reject the normal distribution

# normality tests
# Anderson test for normality -
ad.test(erdata$city_population_2017_typ=="City") # 
ad.test(erdata$city_population_2017_type=="Rural or small town") 


# qqplot for city - from ggpubr package 
ggqqplot(erdata, x="patient_age",
         color = "city_population_2017_type",
         ggtheme = theme_pubclean()) #


# histogram for both female and male. 
# histogram for gender by age in x-axes
ggplot(erdata2, aes(x= patient_age))+
  geom_histogram(aes(color=city_population_2017_type,fill=city_population_2017_type),
                 position = "identity",bins = 30, alpha=0.5)+
  scale_color_manual(values = c("#00AFBB","#E7B800")) 


# variance test - check if they have the same variance - have the same variance 
var.test(erdata$city_population_2017_type=="City", erdata$city_population_2017_type=="Rural or small town") #

var(erdata$city_population_2017_type=="City", erdata$city_population_2017_type=="Rural or small town", na.rm = T) #

var(erdata$city_population_2017_type=="City", na.rm = T)  #0.1621407
var(erdata$city_population_2017_type=="Rural or small town", na.rm = T)  #0.1621407

# can't performe the density plot char variables

boxplot(patient_age~city_population_2017_type, data = erdata)

#levene test for the variance between the group
leveneTest(erdata$patient_age ~ erdata$city_population_2017_type) #

#################################################################################

# normality test for SES
##########################

# normality tests
# Anderson test for normality - p-value less than 0.05 = not normal distribution 
ad.test(erdata$ses_category=="Low") # 
ad.test(erdata$ses_category=="High")  # 
ad.test(erdata$ses_category=="Intermediate")  
ad.test(erdata$ses_category=="Unknown")  #

# Can't performe histogram or density plot.

# variance test - check if they have the same variance - have the same variance 
var.test(erdata$ses_category=="High", erdata$ses_category=="low") # 

# SES they all have different variance.
var(erdata$ses_category=="Low") #
var(erdata$ses_category=="High") # 
var(erdata$ses_category=="Intermediate") # 
var(erdata$ses_category=="Unknown") #


# qqplot for ses_category - from ggpubr package - 
ggqqplot(erdata, x="patient_age",
         color = "ses_category",
         ggtheme = theme_pubclean()) #

boxplot(patient_age~ses_category, data = erdata)


#levene test for the variance between the group
leveneTest(erdata$patient_age ~ erdata$ses_category) 

#############################################################################################
# Normality test for patiet release type 
erdata$patient_releas_desc

# qqplot for patient_releas_desc - from ggpubr package 
ggqqplot(erdata, x="patient_age",
         color = "patient_releas_desc",
         ggtheme = theme_pubclean()) #


# normality tests
# Anderson test for normality - p-value less than 0.05 = not normal distribution - 
ad.test(erdata$patient_releas_desc=="Released Home") # 
ad.test(erdata$patient_releas_desc=="Hospitalization")  #
ad.test(erdata$patient_releas_desc=="Death")  
ad.test(erdata$patient_releas_desc=="Other") 

erdata$ACS_char_desc

boxplot(patient_age~patient_releas_desc, data = erdata)
boxplot(ACS_char_desc~patient_releas_desc, data = erdata) # NA non numeric argument


#levene test for the variance between the group
leveneTest(erdata$patient_age ~ erdata$patient_releas_desc) # alternative hyphotheis - the variance between at least one group is differ.

###########################################################################
