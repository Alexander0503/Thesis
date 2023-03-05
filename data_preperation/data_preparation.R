# preparation of the data

#get the working diretory
getwd() # "C:/PROGRAMS/WorkingDir"

#know what is the library directory
.libPaths()
#"C:/PROGRAMS/Rpackages"             
# "C:/Program Files/R/R-4.0.3/library"

#change the library directory to desired directory
.libPaths()
#[1] "C:/PROGRAMS/R-4.2.2/library"

.libPaths('C:/PROGRAMS/Rpackages') 
#Set the time and dates in English
Sys.setlocale("LC_TIME", "English")

#########################
#By Alexander Kagan
########################### 

# loading the dataset
library(readr)
## upload the file of ER to the R program and call it erdata
erdata <- read_delim(
  "D:/Data/2022.csv", 
  "|", escape_double = FALSE, trim_ws = TRUE)

###############################################################


# the dataset at the begininig contain 705,598 rows and 27 columns. 

###########################
#Checking the variables and delete unnecessary columns, # because all column is NAs.- Step 1
###########################
erdata=subset(erdata, select = -patient_visit_type_code) # delete non significant/un-necessary variable columns.
erdata=subset(erdata, select = -release_ind)# delete non significant/un-necessary variable columns.
erdata=subset(erdata, select = -first_diagnostic_date)# delete non significant/un-necessary variable columns.
erdata=subset(erdata, select = -secondary_diagnostic_date)# delete non significant/un-necessary variable columns.

# order the data frame by dates so it will start from 2018. 
erdata =  erdata[order(erdata$visit_start_date), ]
head(erdata$visit_start_date) # great!


######################
# save terem to terem
#####################
terem=erdata[grepl("36701",erdata$institute_mbr_code),] #  delete the rows with the code contain 36701 (terem). 

#######################
# delete terem
######################
erdata=erdata[!grepl("36701",erdata$institute_mbr_code),] #  delete the rows with the code contain 36701 (terem). now we have 558,892 rows.

######################################################
# create Dup variable that count the rows we have now
######################################################
require(readxl)
library(plyr)
library(dplyr)
library(epiDisplay)

# create column with number of row in order untill this sterp it will helps us identify duplicated, so then we can delete the duplicates. 
erdata$dub = 1:length(erdata$original_visit_number) 

###########################################################################################
# add English description to city code (also include the region description and population)
###########################################################################################

# load the data of the descrition in in english on the cities.
dim_ciry2 <- read_excel("D:/Data/translation_table/dim_city/dim ciry2.xlsx")

# make char class because in the erdata data frame is char, in oder to use the join function we need both columns will the same and same class. 
dim_ciry2$patient_city_code = as.character(dim_ciry2$patient_city_code) # to work the same 2 columns need to be at the same class form.

# use the join function to merge two data frames together. when we use join we can see it also add more duplicate observation to the data. it's not what we want. 
erdata = plyr::join(x=erdata, y=dim_ciry2, by = "patient_city_code")
# the join function add dublicates to the data frame. 

# we want to delete the duplicates and keep only unique rows. to keep the data frame complete. we will go by the dub column.
#erdata = erdata %>% dplyr::distinct(erdata$dub, .keep_all = T) #
erdata = erdata %>% dplyr::distinct(dub, .keep_all = T)


######################################################
# add the cluster(askholot) socio-demographic index 
######################################################

ashkol_changed <- read_excel("D:/Data/tables/ashkol_changed.xlsx")

# join to the index to the data
erdata = plyr::join(x=erdata, y=ashkol_changed, by = "patient_city_code")

# delete duplicated
erdata = erdata %>% dplyr::distinct(dub, .keep_all = T)

tab1(erdata$cluster_2017, sort.group = "decreasing", cum.percent = T )


library(dplyr)
erdata = erdata %>%  #we replace the NA in population_2017 to be zero. 
  dplyr::mutate(city_population_2017 = coalesce(city_population_2017, 0))

# cteate less than 20,000 habitans and more than 20,000 habitans. 
erdata$city_population_2017_type = as.factor(ifelse(erdata$city_population_2017 >=20000 ,"City","Rural or small town"))

##########################################################################################
# change the gender description to English
erdata$gender_desc1 = as.factor(ifelse(erdata$gender_code ==1, "Male", "Female"))

#omit the column of Hebrew description 
erdata=subset(erdata, select = -gender_desc)

########################
# visit start date
#######################

library(lubridate)
library(chron)
library(plyr)
library(dplyr)
library(tidyr)

# set the start year
erdata$year_visit_start= year(erdata$visit_start_date)

#set the right year date format: 
erdata$visit_start_date = ymd(erdata$visit_start_date)


#set the right date format: 
erdata$visit_end_date = ymd(erdata$visit_end_date)


# visit start time
erdata$visit_start_time= times(erdata$visit_start_time)

class(erdata$visit_start_time) #output: "hms"      "difftime"
sum(is.na(erdata$visit_start_time))#check Na's - output: 0 Na's

# visit end time
erdata$visit_end_time= times(erdata$visit_end_time)


# use chron package to create a vector that contain date and time together.

# add visit start dates and time vector to the data.
#erdata$visit_start_datehour = chron(dates. = as.character.Date(erdata$visit_start_date), 
#                                  times. = erdata$visit_start_time,
#                                   format = c(dates = "Y-m-d", times = "h:m:s"))

# add visit end dates and time vector to the data.
#erdata$visit_end_datehour = (chron(dates. = as.character.Date(erdata$visit_end_date), 
#                               times. = erdata$visit_end_time,
#                                format = c(dates = "Y-m-d", times = "h:m:s")))

# make a difference vector for start and the end of the visit. with absolute value. and add the vector to the data frame.
#erdata$visit_hours_difference =abs(difftime(erdata$visit_end_datehour,
#                                           erdata$visit_start_datehour,
#                                          units = "hours"))



########################################
#create a monthly visits
#########################################

#create a column of year and month. with zoo package.
require(zoo)
# create month and year variable with zoo (zoo keeps the order of the months).
erdata$month_year_visit_start = as.yearmon(erdata$visit_start_date, "%b %Y")

# Column of only by month name
erdata$month_name_visit_start= format(as.Date(erdata$visit_start_date), "%b")

require(tidyr)
########################################
#create a weekly visits
#########################################
# Week number ("V" - is  ISO standard week)
erdata$week_number_v= format(as.Date(erdata$visit_start_date), "%V")


########################################
#create a daily visits
#########################################
#create daily date number  of the month (1-31).
erdata$day_number_of_month_visit= format(as.Date(erdata$visit_start_date), "%d")

#create day number of year (01-366).
erdata$day_number_of_year_visit= format(as.Date(erdata$visit_start_date), "%j")

#Creating full day name (sunday, munday...)
erdata$weekday_name_visit= format(as.Date(erdata$visit_start_date), "%A")

##########################################
# building of categorical variables
#########################################
##################################################################
# build to categorical SES status of the patients by cities
library(dplyr)
##################################################################

erdata$ses_category = as.factor(case_when(
  erdata$cluster_2017 == 1|erdata$cluster_2017 == 2|erdata$cluster_2017 == 3 ~ "Low",
  erdata$cluster_2017 == 4|erdata$cluster_2017 == 5|erdata$cluster_2017 == 6|erdata$cluster_2017 == 7 ~ "Intermediate",
  erdata$cluster_2017 == 8|erdata$cluster_2017 == 9|erdata$cluster_2017 == 10 ~ "High",
  TRUE ~ "Unknown")) # true ~ : is like else command.

#display the distribution of the cluster by category
tab1(erdata$cluster_2017, sort.group = "decreasing", cum.percent = T )

#################################################################################################################
#Create unite(merged) vector from Diad1 and Diag2.
erdata$merged_diag =  paste(erdata$icd9_first_diagnostic_code,erdata$icd9_secondary_diagnostic_code,sep = ", ")
#################################################################################################################


##################################################################################################
# Building ACS diagnoses
##############################
library(stringr)

#number from first and second diagnosis if there is 410 or 411 will appear 1, if not 0. 
erdata$ACS_num_desc_pre_first = ifelse(str_detect(erdata$icd9_first_diagnostic_code,"^410|^411"),1,0)
erdata$ACS_num_desc_pre_second = ifelse(str_detect(erdata$icd9_secondary_diagnostic_code,"^410|^411"),1,0)

erdata$ACS_num_desc = ifelse(erdata$ACS_num_desc_pre_first==1|erdata$ACS_num_desc_pre_second==1,1,0)

#erdata$ACS_diag = ifelse(is.na(erdata$chest_pain_non_acs_num_desc), erdata$merged_diag, NA)

##############################################################################################################
# build ACS string desc variable
erdata$ACS_char_desc = ifelse(erdata$ACS_num_desc_pre_first==1|erdata$ACS_num_desc_pre_second==1,"ACS", NA)

sum(!is.na(erdata$ACS_char_desc)) #61,349

##############################################################################################################

#BUILD ACS AND CHEST PAIN COMBINE DIAG TYPE
############################################

erdata$chest_pain_acs_char_diag = ifelse(erdata$ACS_num_desc_pre_first==1|erdata$ACS_num_desc_pre_second==1,"ACS", "Chest pain")



####################################################################################################################################
# Chest pain diagnoses (Non ACS)
erdata$chest_pain_non_acs_num_desc = ifelse(erdata$ACS_num_desc_pre_first==0&erdata$ACS_num_desc_pre_second==0,1,0)

erdata$chest_pain_non_acs_char_desc = ifelse(erdata$ACS_num_desc_pre_first==0&erdata$ACS_num_desc_pre_second==0,"Chest pain",NA)


##################################################################################################################
# Chest pain diagnoses Regular that contain 7865

erdata$chest_pain_reg_num_first = ifelse(str_detect(erdata$icd9_first_diagnostic_code,"^7865"),1,0)
erdata$chest_pain_reg_num_second= ifelse(str_detect(erdata$icd9_secondary_diagnostic_code,"^7865"),1,0)

erdata$chest_pain_reg_char_first = ifelse(str_detect(erdata$icd9_first_diagnostic_code,"^7865"),"Chest pain",NA)
erdata$chest_pain_reg_char_second= ifelse(str_detect(erdata$icd9_secondary_diagnostic_code,"^7865"),"Chest pain",NA)

erdata$chest_pain_reg_num_desc = ifelse(erdata$chest_pain_reg_num_first==1|erdata$chest_pain_reg_num_second==1,1,0)

erdata$chest_pain_reg_char_desc = ifelse(erdata$chest_pain_reg_num_first==1|erdata$chest_pain_reg_num_second==1,"Chest pain",NA)


############################################
# Building Myocardial infarction diagnosis
###########################################

#number from first and second diagnosis if there is 410 will appear 1, if not 0. 
erdata$mi_num_desc_pre_first = ifelse(str_detect(erdata$icd9_first_diagnostic_code,"^410"),1,0)
erdata$mi_num_desc_pre_second = ifelse(str_detect(erdata$icd9_secondary_diagnostic_code,"^410"),1,0)

erdata$mi_char_desc_pre_first = ifelse(str_detect(erdata$icd9_first_diagnostic_code,"^410"),"MI",NA)
erdata$mi_char_desc_pre_second = ifelse(str_detect(erdata$icd9_secondary_diagnostic_code,"^410"),"MI",NA)


erdata$myocardial_num_desc = ifelse(erdata$mi_num_desc_pre_first ==1|erdata$mi_num_desc_pre_second ==1,1,0)

erdata$myocardial_char_desc = ifelse(erdata$mi_num_desc_pre_first ==1|erdata$mi_num_desc_pre_second ==1,"MI",NA)

######################################################################################################################
# NSTEMI #
erdata$pre_NSTEMI_first_num = ifelse((grepl("^41070|^41071|^41090|^41091", erdata$icd9_first_diagnostic_code)), 1, 0) # 
erdata$pre_NSTEMI_second_num = ifelse((grepl("^41070|^41071|^41090|^41091", erdata$icd9_secondary_diagnostic_code)), 1, 0) #

## Build the numeric NSTEMI description for counting (help count if have the case = 1, if not = 0)
erdata$NSTEMI_num_desc = ifelse(erdata$pre_NSTEMI_first_num ==1|erdata$pre_NSTEMI_second_num ==1,1,0)

# build the NSTEMI string desc variable
erdata$NSTEMI_char_desc = ifelse(erdata$pre_NSTEMI_first_num ==1|erdata$pre_NSTEMI_second_num ==1,"NSTEMI",NA) # So if it's not NA put NSTEMI else Put NA. 
sum(!is.na(erdata$NSTEMI_char_desc)) # 16,643

##############################################################################
# STEMI#
erdata$pre_STEMI_num_first = ifelse(str_detect(erdata$icd9_first_diagnostic_code,"^41000|^41001|^41010|^41011|^41020|^41021|^41030|^41031|^41040|^41041|^41050|^41051|^41060|^41061|^41080|^41081"), 1, 0)  
erdata$pre_STEMI_num_second = ifelse(str_detect(erdata$icd9_secondary_diagnostic_code,"^41000|^41001|^41010|^41011|^41020|^41021|^41030|^41031|^41040|^41041|^41050|^41051|^41060|^41061|^41080|^41081"), 1, 0)  

# build STEMI NUM desc variable
erdata$STEMI_num_desc = ifelse(erdata$pre_STEMI_num_first ==1|erdata$pre_STEMI_num_second ==1&erdata$pre_NSTEMI_first_num==0&erdata$pre_NSTEMI_second_num==0,1,0)

# build STEMI string desc variable
erdata$STEMI_char_desc = ifelse(erdata$pre_STEMI_num_first ==1|erdata$pre_STEMI_num_second ==1&erdata$pre_NSTEMI_first_num==0&erdata$pre_NSTEMI_second_num==0,"STEMI",NA)

tab1(erdata$STEMI_char_desc, sort.group = "decreasing", cum.percent = T)
sum(!is.na(erdata$STEMI_char_desc)) # 5,922 ok after i repair now it 5884 not overlap Of nstemi and stemi.
######################################################################################################

####################################################
# create un-defined MI (not STEMI and not NSTEMI)
####################################################

erdata$un_defined_mi_char_desc = ifelse(erdata$myocardial_num_desc==1&
                                          erdata$STEMI_num_desc==0&
                                          erdata$NSTEMI_num_desc==0, "un-defined MI", NA) 

erdata$un_defined_mi_num_desc = ifelse(erdata$myocardial_num_desc==1&
                                         erdata$STEMI_num_desc==0&
                                         erdata$NSTEMI_num_desc==0, 1,0) 

# check if have stemi and non-stemi at the same time.
erdata2$un_defined_mi_char_desc = ifelse(erdata$STEMI_num_desc==1&
                                           erdata$NSTEMI_num_desc==1, "MATCHHH", NA) 
sum(!is.na(erdata2$un_defined_mi_char_desc)) # we have 61 cases of stemi and non stemi.

#un_defined_mi_num_desc = ifelse(erdata$pre_un_defined_mi_first_num==1|erdata$pre_un_defined_mi_second_num==1,"UN_DEFINED_MI",NA)
sum(!is.na(erdata$un_defined_mi_char_desc)) #7,301

#########################################################################################################
#Building un-stable angina diagnosis
###########################################

#number from first and second diagnosis if there is 410 will appear 1, if not 0. 
erdata$UA_num_pre_first = ifelse(str_detect(erdata$icd9_first_diagnostic_code,"^411"),1,0)
erdata$UA_num_pre_second = ifelse(str_detect(erdata$icd9_secondary_diagnostic_code,"^411"),1,0)

erdata$UA_num_desc = ifelse(erdata$UA_num_pre_first ==1|erdata$UA_num_pre_second==1,1,0)

erdata$UA_char_desc = ifelse(erdata$UA_num_pre_first ==1|erdata$UA_num_pre_second ==1,"UA",NA)

sum(!is.na(erdata$UA_char_desc )) #31949 


# check if have 410 and 411 together 

erdata2$checking_410_411 = ifelse(
  erdata$myocardial_num_desc==1&
    erdata$UA_num_desc==1, "MATCH 410 and 411", NA) 

#######################################################################################
#########################################################
# building covid-19 positive variable
#########################################################
library(stringr)

#pre first number if have match it's 1 
erdata$covid_pre_num_first_desc= ifelse((grepl("07984|07985|07986|1392|
                                        |07990|V0255|V1205",erdata$icd9_first_diagnostic_code,ignore.case = T)),1,0)

#pre second number if have match it's 1
erdata$covid_pre_num_second_desc= ifelse((grepl("07984|07985|07986|1392|
                                        |07990|V0255|V1205",erdata$icd9_secondary_diagnostic_code,ignore.case = T)),1,0)


#number decription if have in first or in second it's - 1, if not it's - 0
erdata$covid_num_desc= ifelse(erdata$covid_pre_num_first_desc==1|erdata$covid_pre_num_second_desc==1 ,1,0)

##########################################################################################################################
# COVID-19 char representation  
erdata$covid_char_desc= ifelse(erdata$covid_pre_num_first_desc==1|erdata$covid_pre_num_second_desc==1,"COVID-19(+)","")
##########################################################################################################################

############################################
# Build ACS AND CHEST PAIN COVID-19 TYPE
############################################
erdata$chest_pain_acs_covid_char_type =  as.factor(paste(erdata$chest_pain_acs_char_diag,erdata$covid_char_desc))

levels(erdata$chest_pain_acs_covid_char_type)
levels(erdata$patient_releas_desc) #"ACS ", "ACS COVID-19(+)", "Chest pain ", "Chest pain COVID-19(+)"


#############################################################################################
# count and sum
###################################
sum(!is.na(erdata2$checking_410_411))
library(stringr)
# check how many time appear the chest pain as first diagnosis before diagnosed as MI
sum(str_count(erdata$merged_diag, pattern = "41002|41012|41022|41032|41042|41052|41062|41072|41082|41092")) # N = 448 

sum(str_count(erdata$merged_diag, pattern = "079"))
sum(str_count(erdata$merged_diag, pattern = "^V13"))


#####################################################################################
# create age groups create the following age groups: '25-64', '45-59','60-74','65+
####################################################################################
erdata$age_grp = cut(erdata$patient_age,
                     breaks = c(25,45,60,65, Inf),
                     labels = c ('25-64', '45-59','60-74','65+'),
                     right = F)


#####################################################################################
# create age groups sort 2. with 2 following age groups: 25-64,65+
####################################################################################
erdata$age_grp_2 = cut(erdata$patient_age,
                       breaks = c(25,65, Inf),
                       labels = c ('25-64','65+'),
                       right = F)


###################################################
# build the release description categorical variable
library(dplyr)
###################################################

erdata$patient_releas_desc = as.factor(case_when(
  erdata$patient_release_type_code == "02" ~ "Released Home",
  erdata$patient_release_type_code == "06" ~ "Hospitalization",
  erdata$patient_release_type_code == "03" ~ "Death",
  TRUE ~ "Other"))

erdata$patient_releas_desc = factor(erdata$patient_releas_desc, levels = c("Released Home", "Hospitalization",  "Death", "Other"))
levels(erdata$patient_releas_desc)


############################################################
# build district categorical variable description in English
library(dplyr)
###################################################
erdata$region_code_eng_desc = as.factor(case_when(
  erdata$region_code == 1|erdata$region_code == 10 ~ "Jerusalem District",
  erdata$region_code == 2||erdata$region_code == 20  ~ "Northern District",
  erdata$region_code == 3|erdata$region_code ==30 ~ "Haifa District",
  
  erdata$region_code == 4|erdata$region_code == 40 ~ "Central District",
  erdata$region_code == 5|erdata$region_code == 50 ~ "Tel-Aviv District",
  erdata$region_code == 6|erdata$region_code ==62|erdata$region_code ==61 ~ "Southern District",
  erdata$region_code == 7|erdata$region_code ==70 ~ "Judea and Samaria District",
  TRUE ~ "Unknown")) # true ~ : is like else command.

###################################################################################################
#                     CREATING THE VARIABLES WITH RATES 
####################################################################################################

#create factor rate per 100,000
fcrateper100ts = 100000

all_pop_2018 =  5039100 
all_pop_2019 =  5139400 
all_pop_2020 =  5235400 
all_pop_2021 =  5329862 

######################################
# all visits data frame include rates
#####################################
#extract total all visits   monthly. 
all_visits_monthly =  erdata %>% 
  group_by(year_visit_start, month_year_visit_start) %>%
  summarise(all_visits = n())

erdata$city_population_2017_type
#create 2018 all visits
all_visits_monthly_2018 = filter(all_visits_monthly, year_visit_start =="2018")

# create crude rates for 2018, for all population 
all_visits_monthly_2018$all_visits_rate = all_visits_monthly_2018$all_visits/all_pop_2018*fcrateper100ts

# create all visits of 2019
all_visits_monthly_2019 = filter(all_visits_monthly, year_visit_start =="2019")

# create crude rates for 2019, for all population
all_visits_monthly_2019$all_visits_rate = all_visits_monthly_2019$all_visits/all_pop_2019*fcrateper100ts

# create all visits of 2020
all_visits_monthly_2020 = filter(all_visits_monthly, year_visit_start =="2020")

# create crude rates for 2020, for all population
all_visits_monthly_2020$all_visits_rate = all_visits_monthly_2020$all_visits/all_pop_2020*fcrateper100ts

# create all visits of 2021
all_visits_monthly_2021 = filter(all_visits_monthly, year_visit_start =="2021")

# create crude rates for 2021, for all population
all_visits_monthly_2021$all_visits_rate = all_visits_monthly_2018$all_visits/all_pop_2021*fcrateper100ts

#create data frame of crude chest pain visits monthly 
all_visits_rate_monthly_df = rbind(all_visits_monthly_2018,all_visits_monthly_2019, all_visits_monthly_2020, all_visits_monthly_2021) 

###################################################
#extract hospitalization from all visits monthly. 
###################################################
library(tidyr)
hosp_from_all_visits_monthly =  erdata %>% 
  group_by(year_visit_start, month_year_visit_start, patient_releas_desc) %>%
  summarise(release_type_all_vists = n())

#drop na
hosp_from_all_visits_monthly = drop_na(hosp_from_all_visits_monthly)

#arrange the table of gender daily
hosp_from_all_visits_monthly = hosp_from_all_visits_monthly %>%
  pivot_wider(names_from = patient_releas_desc,
              values_from = release_type_all_vists)

# convet the hospitalization to excel file
library(writexl)
write_xlsx(hosp_from_all_visits_monthly, "D:/Data/tables/adjusted_rate/hosp_from_all_visits_monthly.xlsx")  
##########################################################################################

###########################
# create ACS visits
##########################

#extract ACS_visits visits monthly. 
ACS_visits_monthly =  erdata %>% 
  group_by(year_visit_start, month_year_visit_start, ACS_char_desc) %>%
  summarise(ACS_monthly = n())

#drop na
ACS_visits_monthly = drop_na(ACS_visits_monthly)

# delete un-nesesery column
ACS_visits_monthly = subset(ACS_visits_monthly, select = -ACS_char_desc)

#create visits by yearly 2018/2019/2020/2021 
ACS_visits_monthly_2018 = filter(ACS_visits_monthly, year_visit_start =="2018")
ACS_visits_monthly_2019 = filter(ACS_visits_monthly,year_visit_start =="2019")
ACS_visits_monthly_2020 = filter(ACS_visits_monthly, year_visit_start =="2020")
ACS_visits_monthly_2021 = filter(ACS_visits_monthly, year_visit_start =="2021")

# create crude rates for the years
ACS_visits_monthly_2018$ACS_rate = ACS_visits_monthly_2018$ACS_monthly/all_pop_2018*fcrateper100ts
ACS_visits_monthly_2019$ACS_rate = ACS_visits_monthly_2019$ACS_monthly/all_pop_2019*fcrateper100ts
ACS_visits_monthly_2020$ACS_rate = ACS_visits_monthly_2020$ACS_monthly/all_pop_2020*fcrateper100ts
ACS_visits_monthly_2021$ACS_rate = ACS_visits_monthly_2021$ACS_monthly/all_pop_2021*fcrateper100ts


#create data frame of crude chest pain visits monthly 
ACS_visits_monthly_rate_df = rbind(ACS_visits_monthly_2018,ACS_visits_monthly_2019, ACS_visits_monthly_2020, ACS_visits_monthly_2021) 

#########################################################################################################
# convet the hospitalization to excel file
library(writexl)
write_xlsx(ACS_visits_monthly_rate_df, "D:/Data/tables/adjusted_rate/ACS_visits_monthly_rate_df.xlsx")  

#################################################################
#ACS and rural or City 
#extract ACS_visits visits monthly. 
ACS_visits_by_city =  erdata %>% 
  group_by(year_visit_start, month_year_visit_start, ACS_char_desc,city_population_2017_type ) %>%
  summarise(ACS_by_city = n())

#drop na
ACS_visits_by_city = drop_na(ACS_visits_by_city)

# delete un-nesesery column
ACS_visits_by_city = subset(ACS_visits_by_city, select = -ACS_char_desc)

#arrange the table of
library(tidyr)
library(dplyr)

ACS_visits_by_city  =ACS_visits_by_city  %>%
  pivot_wider(names_from = city_population_2017_type,
              values_from = ACS_by_city)

ACS_visits_by_city$Rural=ACS_visits_by_city$`Rural or small town`
ACS_visits_by_city = subset(ACS_visits_by_city, select = -`Rural or small town`)

###################################################################
# acs rate city
###################################################################
ACS_visits_by_city$acs_city_rate = case_when(
  ACS_visits_by_city$year_visit_start  == 2018 ~ ACS_visits_by_city$City/all_pop_2018*fcrateper100ts,
  ACS_visits_by_city$year_visit_start  == 2019 ~ ACS_visits_by_city$City/all_pop_2019*fcrateper100ts,
  ACS_visits_by_city$year_visit_start  == 2020 ~ ACS_visits_by_city$City/all_pop_2020*fcrateper100ts,
  ACS_visits_by_city$year_visit_start  == 2021 ~ ACS_visits_by_city$City/all_pop_2021*fcrateper100ts)

###################################################################
# acs rate rural
###################################################################
ACS_visits_by_city$acs_rural_rate = case_when(
  ACS_visits_by_city$year_visit_start  == 2018 ~ ACS_visits_by_city$Rural/all_pop_2018*fcrateper100ts,
  ACS_visits_by_city$year_visit_start  == 2019 ~ ACS_visits_by_city$Rural/all_pop_2019*fcrateper100ts,
  ACS_visits_by_city$year_visit_start  == 2020 ~ ACS_visits_by_city$Rural/all_pop_2020*fcrateper100ts,
  ACS_visits_by_city$year_visit_start  == 2021 ~ ACS_visits_by_city$Rural/all_pop_2021*fcrateper100ts)

library(lubridate)
ACS_visits_by_city$time_by_month= seq(from = ymd("2018-01-01"), to = ymd("2021-12-31"), by="month")

#########################################################################################################
# convert to excel file
library(writexl)
write_xlsx(ACS_visits_by_city, "D:/Data/tables/adjusted_rate/ACS_visits_by_city.xlsx")  

#######################################################


######################################
#chest pain data frame include rates
#####################################
library(tidyverse)

#extract chest pain_visits visits monthly. 
chest_pain_visits_monthly = erdata %>% 
  group_by(year_visit_start, month_year_visit_start, chest_pain_non_acs_char_desc) %>%
  summarise(chest_pain_monthly = n())
erdata$chest_pain_non_acs_char_desc

#drop na
chest_pain_visits_monthly = drop_na(chest_pain_visits_monthly)

# delete un-nesesery column
chest_pain_visits_monthly = subset(chest_pain_visits_monthly , select = -chest_pain_non_acs_char_desc)

#create visits by yearly 2018/2019/2020/2021 
chest_pain_visits_monthly_2018 = filter(chest_pain_visits_monthly, year_visit_start =="2018")
chest_pain_visits_monthly_2019 = filter(chest_pain_visits_monthly,year_visit_start =="2019")
chest_pain_visits_monthly_2020 = filter(chest_pain_visits_monthly, year_visit_start =="2020")
chest_pain_visits_monthly_2021 = filter(chest_pain_visits_monthly, year_visit_start =="2021")

# create crude rates for the years
chest_pain_visits_monthly_2018$chest_pain_rate = chest_pain_visits_monthly_2018$chest_pain_monthly/all_pop_2018*fcrateper100ts
chest_pain_visits_monthly_2019$chest_pain_rate = chest_pain_visits_monthly_2019$chest_pain_monthly/all_pop_2019*fcrateper100ts
chest_pain_visits_monthly_2020$chest_pain_rate = chest_pain_visits_monthly_2020$chest_pain_monthly/all_pop_2020*fcrateper100ts
chest_pain_visits_monthly_2021$chest_pain_rate = chest_pain_visits_monthly_2021$chest_pain_monthly/all_pop_2021*fcrateper100ts

#create data frame of crude chest pain visits monthly 
chest_pain_visits_rate = rbind(chest_pain_visits_monthly_2018,chest_pain_visits_monthly_2019, chest_pain_visits_monthly_2020, chest_pain_visits_monthly_2021) 


######################################################
#extract hospitalization from chest pain visits monthly
#####################################################

hosp_from_chest_pain_visits_monthly =  erdata %>% 
  group_by(year_visit_start, month_year_visit_start , chest_pain_non_acs_num_desc, patient_releas_desc) %>%
  summarise(release_type_chest_pain = n())

#drop na
hosp_from_chest_pain_visits_monthly = drop_na(hosp_from_chest_pain_visits_monthly)

#arrange the table of gender daily
library(tidyr)
hosp_from_chest_pain_visits_monthly = hosp_from_chest_pain_visits_monthly %>%
  pivot_wider(names_from = patient_releas_desc,
              values_from = release_type_chest_pain)

# convet the hospitalization to excel file
library(writexl)
write_xlsx(hosp_from_chest_pain_visits_monthly, "D:/Data/tables/adjusted_rate/hosp_from_chest_pain_visits_monthly.xlsx")  


########################################################
#create myocardial infraction data frame include rates
#######################################################

#extract mi total visits monthly. 
mi_visits_monthly =  erdata %>% 
  group_by(year_visit_start, month_year_visit_start, myocardial_char_desc) %>%
  summarise(mi = n())

#drop na
mi_visits_monthly = drop_na(mi_visits_monthly)

# delete un-nesesery column
mi_visits_monthly=subset(mi_visits_monthly, select = -myocardial_char_desc)

#create 2018 mi
mi_visits_2018 = filter(mi_visits_monthly, year_visit_start =="2018")

# create crude rates for 2018, for population above 24
mi_visits_2018$mi_visits_rate = mi_visits_2018$mi/all_pop_2018*fcrateper100ts

#create 2019 mi
mi_visits_2019 = filter(mi_visits_monthly, year_visit_start =="2019")

# create crude rates for 2019, for population above 24
mi_visits_2019$mi_visits_rate = mi_visits_2019$mi/all_pop_2019*fcrateper100ts

#create 2020 mi
mi_visits_2020 = filter(mi_visits_monthly, year_visit_start =="2020")

# create crude rates for 2020, for population above 24
mi_visits_2020$mi_visits_rate = mi_visits_2020$mi/all_pop_2020*fcrateper100ts

#create 2021 mi
mi_visits_2021 = filter(mi_visits_monthly, year_visit_start =="2021")

# create crude rates for 2020, for population above 24
mi_visits_2021$mi_visits_rate = mi_visits_2021$mi/all_pop_2021*fcrateper100ts

#create data frame of crude chest pain visits monthly 
mi_vists_rate_monthly_df = rbind(mi_visits_2018,mi_visits_2019,mi_visits_2020, mi_visits_2021) 

######################################################
#extract hospitalization from mi visits monthly
#####################################################

hosp_from_mi_visits_monthly =  erdata %>% 
  group_by(year_visit_start, month_year_visit_start , myocardial_char_desc, patient_releas_desc) %>%
  summarise(release_type_mi = n())

#drop na
hosp_from_mi_visits_monthly = drop_na(hosp_from_mi_visits_monthly)

#arrange the table of gender daily
library(tidyr)
hosp_from_mi_visits_monthly = hosp_from_mi_visits_monthly %>%
  pivot_wider(names_from = patient_releas_desc,
              values_from = release_type_mi)

# convet the hospitalization to excel file
library(writexl)
write_xlsx(hosp_from_mi_visits_monthly, "D:/Data/tables/adjusted_rate/hosp_from_mi_visits_monthly.xlsx")  


###########################
# create STEMI visits
##########################

#extract STEMI total visits monthly. 
stemi_visits_monthly =  erdata %>% 
  group_by(year_visit_start, month_year_visit_start, STEMI_char_desc) %>%
  summarise(stemi = n())

#drop na
stemi_visits_monthly = drop_na(stemi_visits_monthly )

# delete un-nesesery column
stemi_visits_monthly=subset(stemi_visits_monthly, select = -STEMI_char_desc)


#create visits by yearly 2018/2019/2020/2021 
stemi_visits_2018 = filter(stemi_visits_monthly, year_visit_start =="2018")
stemi_visits_2019 = filter(stemi_visits_monthly,year_visit_start =="2019")
stemi_visits_2020 = filter(stemi_visits_monthly, year_visit_start =="2020")
stemi_visits_2021 = filter(stemi_visits_monthly, year_visit_start =="2021")

# create crude rates for the years
stemi_visits_2018$stemi_rate = stemi_visits_2018$stemi/all_pop_2018*fcrateper100ts
stemi_visits_2019$stemi_rate = stemi_visits_2019$stemi/all_pop_2019*fcrateper100ts
stemi_visits_2020$stemi_rate = stemi_visits_2020$stemi/all_pop_2020*fcrateper100ts
stemi_visits_2021$stemi_rate = stemi_visits_2021$stemi/all_pop_2021*fcrateper100ts

#create data frame of crude chest pain visits monthly 
stemi_vists_rate_monthly_df = rbind(stemi_visits_2018,stemi_visits_2019, stemi_visits_2020, stemi_visits_2021) 

######################################################
#extract hospitalization from stemi visits monthly
#####################################################

hosp_from_stemi_monthly =  erdata %>% 
  group_by(year_visit_start, month_year_visit_start , STEMI_char_desc, patient_releas_desc) %>%
  summarise(release_type_stemi = n())

#drop na
hosp_from_stemi_monthly = drop_na(hosp_from_stemi_monthly)

#arrange the table of gender daily
library(tidyr)
hosp_from_stemi_monthly = hosp_from_stemi_monthly %>%
  pivot_wider(names_from = patient_releas_desc,
              values_from = release_type_stemi)

# convet the hospitalization to excel file
library(writexl)
write_xlsx(hosp_from_stemi_monthly, "D:/Data/tables/adjusted_rate/hosp_from_stemi_monthly.xlsx")  

###########################
# create N-STEMI visits
##########################

#extract NSTEMI total visits monthly. 
nstemi_visits_monthly =  erdata %>% 
  group_by(year_visit_start, month_year_visit_start, NSTEMI_char_desc) %>%
  summarise(nstemi = n())

#drop na
nstemi_visits_monthly = drop_na(nstemi_visits_monthly )

# delete un-nesesery column
nstemi_visits_monthly=subset(nstemi_visits_monthly, select = -NSTEMI_char_desc)

#create visits by yearly 2018/2019/2020/2021 
nstemi_visits_2018 = filter(nstemi_visits_monthly, year_visit_start =="2018")
nstemi_visits_2019 = filter(nstemi_visits_monthly,year_visit_start =="2019")
nstemi_visits_2020 = filter(nstemi_visits_monthly, year_visit_start =="2020")
nstemi_visits_2021 = filter(nstemi_visits_monthly, year_visit_start =="2021")

# create crude rates for the years
nstemi_visits_2018$nstemi_rate = nstemi_visits_2018$nstemi/all_pop_2018*fcrateper100ts
nstemi_visits_2019$nstemi_rate = nstemi_visits_2019$nstemi/all_pop_2019*fcrateper100ts
nstemi_visits_2020$nstemi_rate = nstemi_visits_2020$nstemi/all_pop_2020*fcrateper100ts
nstemi_visits_2021$nstemi_rate = nstemi_visits_2021$nstemi/all_pop_2021*fcrateper100ts

#create data frame of crude chest pain visits monthly 
nstemi_vists_rate_monthly_df = rbind(nstemi_visits_2018,nstemi_visits_2019, nstemi_visits_2020, nstemi_visits_2021) 

######################################################
#extract hospitalization from nstemi visits monthly
#####################################################

hosp_from_nstemi_monthly =  erdata %>% 
  group_by(year_visit_start, month_year_visit_start , NSTEMI_char_desc, patient_releas_desc) %>%
  summarise(release_type_nstemi = n())

#drop na
hosp_from_nstemi_monthly  = drop_na(hosp_from_nstemi_monthly)

#arrange the table of gender daily
library(tidyr)
hosp_from_nstemi_monthly = hosp_from_nstemi_monthly  %>%
  pivot_wider(names_from = patient_releas_desc,
              values_from = release_type_nstemi)


# convet the hospitalization to excel file
library(writexl)
write_xlsx(hosp_from_nstemi_monthly, "D:/Data/tables/adjusted_rate/hosp_from_nstemi_monthly.xlsx")  

######################################################################################

###########################
# create un-defined mi visits
##########################

#extract un-defined visits monthly. 
un_def_mi_visits_monthly =  erdata %>% 
  group_by(year_visit_start, month_year_visit_start, un_defined_mi_char_desc) %>%
  summarise(un_def_mi = n())

#drop na
un_def_mi_visits_monthly = drop_na(un_def_mi_visits_monthly)

# delete un-nesesery column
un_def_mi_visits_monthly = subset(un_def_mi_visits_monthly, select = -un_defined_mi_char_desc)


#create visits by yearly 2018/2019/2020/2021 
un_def_mi_visits_2018 = filter(un_def_mi_visits_monthly, year_visit_start =="2018")
un_def_mi_visits_2019 = filter(un_def_mi_visits_monthly,year_visit_start =="2019")
un_def_mi_visits_2020 = filter(un_def_mi_visits_monthly, year_visit_start =="2020")
un_def_mi_visits_2021 = filter(un_def_mi_visits_monthly, year_visit_start =="2021")


# create crude rates for the years
un_def_mi_visits_2018$un_def_mi_rate = un_def_mi_visits_2018$un_def_mi/all_pop_2018*fcrateper100ts
un_def_mi_visits_2019$un_def_mi_rate = un_def_mi_visits_2019$un_def_mi/all_pop_2019*fcrateper100ts
un_def_mi_visits_2020$un_def_mi_rate = un_def_mi_visits_2020$un_def_mi/all_pop_2020*fcrateper100ts
un_def_mi_visits_2021$un_def_mi_rate = un_def_mi_visits_2021$un_def_mi/all_pop_2021*fcrateper100ts


#create data frame of crude chest pain visits monthly 
un_def_mi_vists_rate_monthly_df = rbind(un_def_mi_visits_2018,un_def_mi_visits_2019, un_def_mi_visits_2020, un_def_mi_visits_2021) 

#########################################################################################################

#################################################################
#gender specific rate  - stratification method
################################################################
# population size in gender by year

pop_2018_male =  2438500
pop_2019_male =  2488800 
pop_2020_male =  2536200 
pop_2021_male =  2583560 

pop_2018_female =  2600600 
pop_2019_female = 2650600 
pop_2020_female =  2699200 
pop_2021_female =  2746302 

######################
#all visits by gender 
######################
all_visits_by_gender =  erdata %>%
  group_by(year_visit_start, month_year_visit_start,gender_desc1) %>%
  summarise(all_visits_gender = n())


#arrange the table of gender daily
library(tidyr)
library(dplyr)

all_visits_by_gender = all_visits_by_gender %>%
  pivot_wider(names_from = gender_desc1,
              values_from = all_visits_gender)

###################################################################
# rate for female by years and size of the population in this year
###################################################################

all_visits_by_gender$female_rate = case_when(
  all_visits_by_gender$year_visit_start  == 2018 ~ all_visits_by_gender$Female/pop_2018_female*fcrateper100ts,
  all_visits_by_gender$year_visit_start  == 2019 ~ all_visits_by_gender$Female/pop_2019_female*fcrateper100ts,
  all_visits_by_gender$year_visit_start  == 2020 ~ all_visits_by_gender$Female/pop_2020_female*fcrateper100ts,
  all_visits_by_gender$year_visit_start  == 2021 ~ all_visits_by_gender$Female/pop_2021_female*fcrateper100ts)



###################################################################
# rate for male by years and size of the population in this year
###################################################################

all_visits_by_gender$male_rate = case_when(
  all_visits_by_gender$year_visit_start  == 2018 ~ all_visits_by_gender$Male/pop_2018_male*fcrateper100ts,
  all_visits_by_gender$year_visit_start  == 2019 ~ all_visits_by_gender$Male/pop_2019_male*fcrateper100ts,
  all_visits_by_gender$year_visit_start  == 2020 ~ all_visits_by_gender$Male/pop_2020_male*fcrateper100ts,
  all_visits_by_gender$year_visit_start  == 2021 ~ all_visits_by_gender$Male/pop_2021_male*fcrateper100ts)

library(epiDisplay)
summ(all_visits_by_gender)
summ(all_visits_by_gender$female_rate)
aggregate.numeric(all_visits_by_gender$Female, by = list(all_visits_by_gender$year_visit_start==2018), FUN = c("sum","sd", "se")) # for 2018 = 57671 


####################
#convert to excel
##################
write_xlsx(all_visits_by_gender, "D:/Data/tables/adjusted_rate/all_visits_by_gender.xlsx")  


###############################################


######################
# ACS by gender 
######################

ACS_by_gender =  erdata %>%
  group_by(year_visit_start, month_year_visit_start,ACS_char_desc, gender_desc1) %>%
  summarise(acs_by_gender = n())

#remove na
ACS_by_gender = drop_na(ACS_by_gender) 

#arrange the table of gender daily
library(tidyr)

ACS_by_gender= ACS_by_gender %>%
  pivot_wider(names_from = gender_desc1,
              values_from = acs_by_gender)

###################################################################
# rate for female by years and size of the population in this year
###################################################################

ACS_by_gender$acs_gender_female_rate = case_when(
  ACS_by_gender$year_visit_start  == 2018 ~ ACS_by_gender$Female/pop_2018_female*fcrateper100ts,
  ACS_by_gender$year_visit_start  == 2019 ~ ACS_by_gender$Female/pop_2019_female*fcrateper100ts,
  ACS_by_gender$year_visit_start  == 2020 ~ ACS_by_gender$Female/pop_2020_female*fcrateper100ts,
  ACS_by_gender$year_visit_start  == 2021 ~ ACS_by_gender$Female/pop_2021_female*fcrateper100ts)


###################################################################
# rate for male by years and size of the population in this year
###################################################################

ACS_by_gender$acs_gender_male_rate = case_when(
  ACS_by_gender$year_visit_start  == 2018 ~ ACS_by_gender$Male/pop_2018_male*fcrateper100ts,
  ACS_by_gender$year_visit_start  == 2019 ~ ACS_by_gender$Male/pop_2019_male*fcrateper100ts,
  ACS_by_gender$year_visit_start  == 2020 ~ ACS_by_gender$Male/pop_2020_male*fcrateper100ts,
  ACS_by_gender$year_visit_start  == 2021 ~ ACS_by_gender$Male/pop_2021_male*fcrateper100ts)

####################
#convert to excel
##################
write_xlsx(ACS_by_gender, "D:/Data/tables/adjusted_rate/ACS_by_gender.xlsx")  


######################
#chest pain by gender 
######################

chest_pain_visits_by_gender =  erdata %>%
  group_by(year_visit_start, month_year_visit_start, chest_pain_non_acs_num_desc, gender_desc1) %>%
  summarise(chest_pain_visits_by_gender = n())

#remove na
chest_pain_visits_by_gender = drop_na(chest_pain_visits_by_gender ) 

#arrange the table of gender daily
library(tidyr)

chest_pain_visits_by_gender= chest_pain_visits_by_gender %>%
  pivot_wider(names_from = gender_desc1,
              values_from = chest_pain_visits_by_gender)



###################################################################
# rate for female by years and size of the population in this year
###################################################################

chest_pain_visits_by_gender$female_rate = case_when(
  chest_pain_visits_by_gender$year_visit_start  == 2018 ~ chest_pain_visits_by_gender$Female/pop_2018_female*fcrateper100ts,
  chest_pain_visits_by_gender$year_visit_start  == 2019 ~ chest_pain_visits_by_gender$Female/pop_2019_female*fcrateper100ts,
  chest_pain_visits_by_gender$year_visit_start  == 2020 ~ chest_pain_visits_by_gender$Female/pop_2020_female*fcrateper100ts,
  chest_pain_visits_by_gender$year_visit_start  == 2021 ~ chest_pain_visits_by_gender$Female/pop_2021_female*fcrateper100ts)


###################################################################
# rate for male by years and size of the population in this year
###################################################################

chest_pain_visits_by_gender$male_rate = case_when(
  chest_pain_visits_by_gender$year_visit_start  == 2018 ~ chest_pain_visits_by_gender$Male/pop_2018_male*fcrateper100ts,
  chest_pain_visits_by_gender$year_visit_start  == 2019 ~ chest_pain_visits_by_gender$Male/pop_2019_male*fcrateper100ts,
  chest_pain_visits_by_gender$year_visit_start  == 2020 ~ chest_pain_visits_by_gender$Male/pop_2020_male*fcrateper100ts,
  chest_pain_visits_by_gender$year_visit_start  == 2021 ~ chest_pain_visits_by_gender$Male/pop_2021_male*fcrateper100ts)

####################
#convert to excel
##################
write_xlsx( chest_pain_visits_by_gender, "D:/Data/tables/adjusted_rate/chest_pain_visits_by_gender.xlsx")  

###########################################################################################

###################################################################
#               MI by gender 
##################################################################

mi_visits_by_gender =  erdata %>%
  group_by(year_visit_start, month_year_visit_start, myocardial_char_desc, gender_desc1) %>%
  summarise(mi_visits_by_gender = n())

#remove na
mi_visits_by_gender = drop_na(mi_visits_by_gender) 

#arrange the table of gender daily
library(tidyr)

mi_visits_by_gender = mi_visits_by_gender  %>%
  pivot_wider(names_from = gender_desc1,
              values_from = mi_visits_by_gender)


###################################################################
# rate for female by years and size of the population in this year
###################################################################

mi_visits_by_gender$female_rate = case_when(
  mi_visits_by_gender$year_visit_start  == 2018 ~ mi_visits_by_gender$Female/pop_2018_female*fcrateper100ts,
  mi_visits_by_gender$year_visit_start  == 2019 ~ mi_visits_by_gender$Female/pop_2019_female*fcrateper100ts,
  mi_visits_by_gender$year_visit_start  == 2020 ~ mi_visits_by_gender$Female/pop_2020_female*fcrateper100ts,
  mi_visits_by_gender$year_visit_start  == 2021 ~ mi_visits_by_gender$Female/pop_2021_female*fcrateper100ts)


###################################################################
# rate for male by years and size of the population in this year
###################################################################

mi_visits_by_gender$male_rate = case_when(
  mi_visits_by_gender$year_visit_start  == 2018 ~ mi_visits_by_gender$Male/pop_2018_male*fcrateper100ts,
  mi_visits_by_gender$year_visit_start  == 2019 ~ mi_visits_by_gender$Male/pop_2019_male*fcrateper100ts,
  mi_visits_by_gender$year_visit_start  == 2020 ~ mi_visits_by_gender$Male/pop_2020_male*fcrateper100ts,
  mi_visits_by_gender$year_visit_start  == 2021 ~ mi_visits_by_gender$Male/pop_2021_male*fcrateper100ts)


####################
#convert to excel
##################
write_xlsx(mi_visits_by_gender, "D:/Data/tables/adjusted_rate/mi_visits_by_gender.xlsx")  



###################################################################
#               STEMI by gender 
##################################################################

stemi_visits_by_gender =  erdata %>%
  group_by(year_visit_start, month_year_visit_start, STEMI_char_desc, gender_desc1) %>%
  summarise(stemi_visits_by_gender = n())

#remove na
stemi_visits_by_gender = drop_na(stemi_visits_by_gender) 

#arrange the table of gender daily
library(tidyr)

stemi_visits_by_gender = stemi_visits_by_gender  %>%
  pivot_wider(names_from = gender_desc1,
              values_from = stemi_visits_by_gender)


###################################################################
# rate for female by years and size of the population in this year
###################################################################

stemi_visits_by_gender$female_rate = case_when(
  stemi_visits_by_gender$year_visit_start  == 2018 ~ stemi_visits_by_gender$Female/pop_2018_female*fcrateper100ts,
  stemi_visits_by_gender$year_visit_start  == 2019 ~ stemi_visits_by_gender$Female/pop_2019_female*fcrateper100ts,
  stemi_visits_by_gender$year_visit_start  == 2020 ~ stemi_visits_by_gender$Female/pop_2020_female*fcrateper100ts,
  stemi_visits_by_gender$year_visit_start  == 2021 ~ stemi_visits_by_gender$Female/pop_2021_female*fcrateper100ts)


###################################################################
# rate for male by years and size of the population in this year
###################################################################

stemi_visits_by_gender$male_rate = case_when(
  stemi_visits_by_gender$year_visit_start  == 2018 ~ stemi_visits_by_gender$Male/pop_2018_male*fcrateper100ts,
  stemi_visits_by_gender$year_visit_start  == 2019 ~ stemi_visits_by_gender$Male/pop_2019_male*fcrateper100ts,
  stemi_visits_by_gender$year_visit_start  == 2020 ~ stemi_visits_by_gender$Male/pop_2020_male*fcrateper100ts,
  stemi_visits_by_gender$year_visit_start  == 2021 ~ stemi_visits_by_gender$Male/pop_2021_male*fcrateper100ts)


####################
#convert to excel
##################
write_xlsx(  stemi_visits_by_gender, "D:/Data/tables/adjusted_rate/  stemi_visits_by_gender.xlsx")  



###################################################################
#               NSTEMI by gender 
##################################################################

nstemi_visits_by_gender =  erdata %>%
  group_by(year_visit_start, month_year_visit_start, NSTEMI_char_desc, gender_desc1) %>%
  summarise(nstemi_visits_by_gender = n())

#remove na
nstemi_visits_by_gender = drop_na(nstemi_visits_by_gender) 

#arrange the table of gender daily
library(tidyr)

nstemi_visits_by_gender = nstemi_visits_by_gender  %>%
  pivot_wider(names_from = gender_desc1,
              values_from = nstemi_visits_by_gender)


###################################################################
# rate for female by years and size of the population in this year
###################################################################

nstemi_visits_by_gender$female_rate = case_when(
  nstemi_visits_by_gender$year_visit_start  == 2018 ~ nstemi_visits_by_gender$Female/pop_2018_female*fcrateper100ts,
  nstemi_visits_by_gender$year_visit_start  == 2019 ~ nstemi_visits_by_gender$Female/pop_2019_female*fcrateper100ts,
  nstemi_visits_by_gender$year_visit_start  == 2020 ~ nstemi_visits_by_gender$Female/pop_2020_female*fcrateper100ts,
  nstemi_visits_by_gender$year_visit_start  == 2021 ~ nstemi_visits_by_gender$Female/pop_2021_female*fcrateper100ts)


###################################################################
# rate for male by years and size of the population in this year
###################################################################

nstemi_visits_by_gender$male_rate = case_when(
  nstemi_visits_by_gender$year_visit_start  == 2018 ~ nstemi_visits_by_gender$Male/pop_2018_male*fcrateper100ts,
  nstemi_visits_by_gender$year_visit_start  == 2019 ~ nstemi_visits_by_gender$Male/pop_2019_male*fcrateper100ts,
  nstemi_visits_by_gender$year_visit_start  == 2020 ~ nstemi_visits_by_gender$Male/pop_2020_male*fcrateper100ts,
  nstemi_visits_by_gender$year_visit_start  == 2021 ~ nstemi_visits_by_gender$Male/pop_2021_male*fcrateper100ts)


####################
#convert to excel
####################
write_xlsx(  nstemi_visits_by_gender, "D:/Data/tables/adjusted_rate/  nstemi_visits_by_gender.xlsx")  

#######################################################################


#################################################################
# specific rate for age group 
################################################################
# population size in age groups by year

pop_2018_age_grp_2_25_64 =  4001900
pop_2019_age_grp_2_25_64 =  4064400
pop_2020_age_grp_2_25_64 =  4124700
pop_2021_age_grp_2_25_64 =  4184539

pop_2018_age_grp_2_above_65 = 1037200
pop_2019_age_grp_2_above_65 = 1065000
pop_2020_age_grp_2_above_65 = 1110700 
pop_2021_age_grp_2_above_65 = 1145322

###########################
#all visits by age groups 
##########################
all_visits_by_age_grp_2 =  erdata %>%
  group_by(year_visit_start, month_year_visit_start,age_grp_2) %>%
  summarise(all_visits_by_age_grp_2 = n())


#arrange the table of gender daily
library(tidyr)

all_visits_by_age_grp_2 = all_visits_by_age_grp_2 %>%
  pivot_wider(names_from = age_grp_2,
              values_from = all_visits_by_age_grp_2)


###################################################################
# all visits specific rate for age_grp_2
###################################################################

####for the first group of ages
all_visits_by_age_grp_2$rate_age_grp_2_25_64 = case_when(
  all_visits_by_age_grp_2$year_visit_start  == 2018 ~ all_visits_by_age_grp_2$`25-64`/pop_2018_age_grp_2_25_64*fcrateper100ts,
  all_visits_by_age_grp_2$year_visit_start  == 2019 ~ all_visits_by_age_grp_2$`25-64`/pop_2019_age_grp_2_25_64*fcrateper100ts,
  all_visits_by_age_grp_2$year_visit_start  == 2020 ~ all_visits_by_age_grp_2$`25-64`/pop_2020_age_grp_2_25_64*fcrateper100ts,
  all_visits_by_age_grp_2$year_visit_start  == 2021 ~ all_visits_by_age_grp_2$`25-64`/pop_2021_age_grp_2_25_64*fcrateper100ts)


# second group of age 65+
all_visits_by_age_grp_2$rate_age_grp_2_65_above = case_when(
  all_visits_by_age_grp_2$year_visit_start  == 2018 ~ all_visits_by_age_grp_2$`65+`/pop_2018_age_grp_2_above_65*fcrateper100ts,
  all_visits_by_age_grp_2$year_visit_start  == 2019 ~ all_visits_by_age_grp_2$`65+`/pop_2019_age_grp_2_above_65*fcrateper100ts,
  all_visits_by_age_grp_2$year_visit_start  == 2020 ~ all_visits_by_age_grp_2$`65+`/pop_2020_age_grp_2_above_65*fcrateper100ts,
  all_visits_by_age_grp_2$year_visit_start  == 2021 ~ all_visits_by_age_grp_2$`65+`/pop_2021_age_grp_2_above_65*fcrateper100ts)

####################
#convert to excel
####################
write_xlsx(all_visits_by_age_grp_2, "D:/Data/tables/adjusted_rate/all_visits_by_age_grp_2.xlsx")  

#########################################################################
###################################################################
# acs specific rate for age_grp_2
###################################################################

acs_by_age_grp_2 =  erdata %>%
  group_by(year_visit_start, month_year_visit_start,ACS_char_desc, age_grp_2) %>%
  summarise(acs_by_age_grp_2 = n())


acs_by_age_grp_2 = drop_na(acs_by_age_grp_2)

#arrange the table of gender daily
library(tidyr)

acs_by_age_grp_2 = acs_by_age_grp_2 %>%
  pivot_wider(names_from = age_grp_2,
              values_from = acs_by_age_grp_2)

####for the first group of ages 25-64
acs_by_age_grp_2$acs_rate_age_grp_2_25_64 = case_when(
  acs_by_age_grp_2$year_visit_start  == 2018 ~ acs_by_age_grp_2$`25-64`/pop_2018_age_grp_2_25_64*fcrateper100ts,
  acs_by_age_grp_2$year_visit_start  == 2019 ~ acs_by_age_grp_2$`25-64`/pop_2019_age_grp_2_25_64*fcrateper100ts,
  acs_by_age_grp_2$year_visit_start  == 2020 ~ acs_by_age_grp_2$`25-64`/pop_2020_age_grp_2_25_64*fcrateper100ts,
  acs_by_age_grp_2$year_visit_start  == 2021 ~ acs_by_age_grp_2$`25-64`/pop_2021_age_grp_2_25_64*fcrateper100ts)

# for the second group of age 65 and above
acs_by_age_grp_2$acs_rate_age_grp_2_65_above = case_when(
  acs_by_age_grp_2$year_visit_start  == 2018 ~ acs_by_age_grp_2$`65+`/pop_2018_age_grp_2_above_65*fcrateper100ts,
  acs_by_age_grp_2$year_visit_start  == 2019 ~ acs_by_age_grp_2$`65+`/pop_2019_age_grp_2_above_65*fcrateper100ts,
  acs_by_age_grp_2$year_visit_start  == 2020 ~ acs_by_age_grp_2$`65+`/pop_2020_age_grp_2_above_65*fcrateper100ts,
  acs_by_age_grp_2$year_visit_start  == 2021 ~ acs_by_age_grp_2$`65+`/pop_2021_age_grp_2_above_65*fcrateper100ts)

####################
#convert to excel
####################
write_xlsx(acs_by_age_grp_2, "D:/Data/tables/adjusted_rate/acs_by_age_grp_2.xlsx")  

###########################
#chest pain visits by age groups 
##########################
chest_pain_by_age_grp_2 =  erdata %>%
  group_by(year_visit_start, month_year_visit_start,chest_pain_non_acs_num_desc, age_grp_2) %>%
  summarise(chest_pain_by_age_grp_2 = n())

chest_pain_by_age_grp_2 = drop_na(chest_pain_by_age_grp_2)

#arrange the table of gender daily
library(tidyr)

chest_pain_by_age_grp_2 = chest_pain_by_age_grp_2 %>%
  pivot_wider(names_from = age_grp_2,
              values_from = chest_pain_by_age_grp_2)


###################################################################
# chest pain specific rate for age_grp_2
###################################################################

####for the first group of ages
chest_pain_by_age_grp_2$rate_age_grp_2_25_64 = case_when(
  chest_pain_by_age_grp_2$year_visit_start  == 2018 ~ chest_pain_by_age_grp_2$`25-64`/pop_2018_age_grp_2_25_64*fcrateper100ts,
  chest_pain_by_age_grp_2$year_visit_start  == 2019 ~ chest_pain_by_age_grp_2$`25-64`/pop_2019_age_grp_2_25_64*fcrateper100ts,
  chest_pain_by_age_grp_2$year_visit_start  == 2020 ~ chest_pain_by_age_grp_2$`25-64`/pop_2020_age_grp_2_25_64*fcrateper100ts,
  chest_pain_by_age_grp_2$year_visit_start  == 2021 ~ chest_pain_by_age_grp_2$`25-64`/pop_2021_age_grp_2_25_64*fcrateper100ts)

# second group of ages
chest_pain_by_age_grp_2$rate_age_grp_2_65_above = case_when(
  chest_pain_by_age_grp_2$year_visit_start  == 2018 ~ chest_pain_by_age_grp_2$`65+`/pop_2018_age_grp_2_above_65*fcrateper100ts,
  chest_pain_by_age_grp_2$year_visit_start  == 2019 ~ chest_pain_by_age_grp_2$`65+`/pop_2019_age_grp_2_above_65*fcrateper100ts,
  chest_pain_by_age_grp_2$year_visit_start  == 2020 ~ chest_pain_by_age_grp_2$`65+`/pop_2020_age_grp_2_above_65*fcrateper100ts,
  chest_pain_by_age_grp_2$year_visit_start  == 2021 ~ chest_pain_by_age_grp_2$`65+`/pop_2021_age_grp_2_above_65*fcrateper100ts)

####################
#convert to excel
####################
write_xlsx(chest_pain_by_age_grp_2, "D:/Data/tables/adjusted_rate/chest_pain_by_age_grp_2.xlsx")  

#########################################################################


###########################
# mi visits by age groups 
##########################
mi_by_age_grp_2 =  erdata %>%
  group_by(year_visit_start, month_year_visit_start,myocardial_char_desc, age_grp_2) %>%
  summarise(mi_by_age_grp_2 = n())

mi_by_age_grp_2 = drop_na(mi_by_age_grp_2)

#arrange the table of gender daily
library(tidyr)

mi_by_age_grp_2 = mi_by_age_grp_2 %>%
  pivot_wider(names_from = age_grp_2,
              values_from = mi_by_age_grp_2)


###################################################################
# mi specific rate for age_grp_2
###################################################################

####for the first group of ages
mi_by_age_grp_2$rate_age_grp_2_25_64 = case_when(
  mi_by_age_grp_2$year_visit_start  == 2018 ~ mi_by_age_grp_2$`25-64`/pop_2018_age_grp_2_25_64*fcrateper100ts,
  mi_by_age_grp_2$year_visit_start  == 2019 ~ mi_by_age_grp_2$`25-64`/pop_2019_age_grp_2_25_64*fcrateper100ts,
  mi_by_age_grp_2$year_visit_start  == 2020 ~ mi_by_age_grp_2$`25-64`/pop_2020_age_grp_2_25_64*fcrateper100ts,
  mi_by_age_grp_2$year_visit_start  == 2021 ~ mi_by_age_grp_2$`25-64`/pop_2021_age_grp_2_25_64*fcrateper100ts)

# Fourth group of ages
mi_by_age_grp_2$rate_age_grp_2_65_above = case_when(
  mi_by_age_grp_2$year_visit_start  == 2018 ~ mi_by_age_grp_2$`65+`/pop_2018_age_grp_2_above_65*fcrateper100ts,
  mi_by_age_grp_2$year_visit_start  == 2019 ~ mi_by_age_grp_2$`65+`/pop_2019_age_grp_2_above_65*fcrateper100ts,
  mi_by_age_grp_2$year_visit_start  == 2020 ~ mi_by_age_grp_2$`65+`/pop_2020_age_grp_2_above_65*fcrateper100ts,
  mi_by_age_grp_2$year_visit_start  == 2021 ~ mi_by_age_grp_2$`65+`/pop_2021_age_grp_2_above_65*fcrateper100ts)

####################
#convert to excel
####################
write_xlsx(mi_by_age_grp_2, "D:/Data/tables/adjusted_rate/mi_by_age_grp_2.xlsx")  

###############################################################################


###########################
# stemi visits by age groups 
##########################
stemi_by_age_grp_2 =  erdata %>%
  group_by(year_visit_start, month_year_visit_start,STEMI_char_desc, age_grp_2) %>%
  summarise(stemi_by_age_grp_2 = n())

stemi_by_age_grp_2 = drop_na(stemi_by_age_grp_2)

#arrange the table of gender daily
library(tidyr)

stemi_by_age_grp_2 = stemi_by_age_grp_2 %>%
  pivot_wider(names_from = age_grp_2,
              values_from = stemi_by_age_grp_2)

###################################################################
# stemi specific rate for age_grp_2
###################################################################

####for the first group of ages
stemi_by_age_grp_2$rate_age_grp_2_25_64 = case_when(
  stemi_by_age_grp_2$year_visit_start  == 2018 ~ stemi_by_age_grp_2$`25-64`/pop_2018_age_grp_2_25_64*fcrateper100ts,
  stemi_by_age_grp_2$year_visit_start  == 2019 ~ stemi_by_age_grp_2$`25-64`/pop_2019_age_grp_2_25_64*fcrateper100ts,
  stemi_by_age_grp_2$year_visit_start  == 2020 ~ stemi_by_age_grp_2$`25-64`/pop_2020_age_grp_2_25_64*fcrateper100ts,
  stemi_by_age_grp_2$year_visit_start  == 2021 ~ stemi_by_age_grp_2$`25-64`/pop_2021_age_grp_2_25_64*fcrateper100ts)

# second group of age
stemi_by_age_grp_2$rate_age_grp_2_65_above = case_when(
  stemi_by_age_grp_2$year_visit_start  == 2018 ~ stemi_by_age_grp_2$`65+`/pop_2018_age_grp_2_above_65*fcrateper100ts,
  stemi_by_age_grp_2$year_visit_start  == 2019 ~ stemi_by_age_grp_2$`65+`/pop_2019_age_grp_2_above_65*fcrateper100ts,
  stemi_by_age_grp_2$year_visit_start  == 2020 ~ stemi_by_age_grp_2$`65+`/pop_2020_age_grp_2_above_65*fcrateper100ts,
  stemi_by_age_grp_2$year_visit_start  == 2021 ~ stemi_by_age_grp_2$`65+`/pop_2021_age_grp_2_above_65*fcrateper100ts)

####################
#convert to excel
####################
write_xlsx(stemi_by_age_grp_2, "D:/Data/tables/adjusted_rate/stemi_by_age_grp_2.xlsx")  

#################################################################################


###########################
# nstemi visits by age groups 
##########################
nstemi_by_age_grp_2 =  erdata %>%
  group_by(year_visit_start, month_year_visit_start,NSTEMI_char_desc, age_grp_2) %>%
  summarise(nstemi_by_age_grp_2 = n())

nstemi_by_age_grp_2 = drop_na(nstemi_by_age_grp_2)

#arrange the table of gender daily
library(tidyr)

nstemi_by_age_grp_2 = nstemi_by_age_grp_2 %>%
  pivot_wider(names_from = age_grp_2,
              values_from = nstemi_by_age_grp_2)

###################################################################
# nstemi specific rate for age_grp_2
###################################################################

####for the first group of ages
nstemi_by_age_grp_2$rate_age_grp_2_25_64 = case_when(
  nstemi_by_age_grp_2$year_visit_start  == 2018 ~ nstemi_by_age_grp_2$`25-64`/pop_2018_age_grp_2_25_64*fcrateper100ts,
  nstemi_by_age_grp_2$year_visit_start  == 2019 ~ nstemi_by_age_grp_2$`25-64`/pop_2019_age_grp_2_25_64*fcrateper100ts,
  nstemi_by_age_grp_2$year_visit_start  == 2020 ~ nstemi_by_age_grp_2$`25-64`/pop_2020_age_grp_2_25_64*fcrateper100ts,
  nstemi_by_age_grp_2$year_visit_start  == 2021 ~ nstemi_by_age_grp_2$`25-64`/pop_2021_age_grp_2_25_64*fcrateper100ts)

# Second group of age
nstemi_by_age_grp_2$rate_age_grp_2_65_above = case_when(
  nstemi_by_age_grp_2$year_visit_start  == 2018 ~ nstemi_by_age_grp_2$`65+`/pop_2018_age_grp_2_above_65*fcrateper100ts,
  nstemi_by_age_grp_2$year_visit_start  == 2019 ~ nstemi_by_age_grp_2$`65+`/pop_2019_age_grp_2_above_65*fcrateper100ts,
  nstemi_by_age_grp_2$year_visit_start  == 2020 ~ nstemi_by_age_grp_2$`65+`/pop_2020_age_grp_2_above_65*fcrateper100ts,
  nstemi_by_age_grp_2$year_visit_start  == 2021 ~ nstemi_by_age_grp_2$`65+`/pop_2021_age_grp_2_above_65*fcrateper100ts)

####################
#convert to excel
####################
write_xlsx(nstemi_by_age_grp_2, "D:/Data/tables/adjusted_rate/nstemi_by_age_grp_2.xlsx")  
#############################################################################################


###########################
# un_defined_mi visits by age groups 
##########################
un_defined_mi_by_age_grp_2 =  erdata %>%
  group_by(year_visit_start, month_year_visit_start,un_defined_mi_char_desc, age_grp_2) %>%
  summarise(un_defined_mi_by_age_grp_2 = n())

un_defined_mi_by_age_grp_2 = drop_na(un_defined_mi_by_age_grp_2)

#arrange the table of gender daily
library(tidyr)

un_defined_mi_by_age_grp_2 = un_defined_mi_by_age_grp_2 %>%
  pivot_wider(names_from = age_grp_2,
              values_from = un_defined_mi_by_age_grp_2)

###################################################################
# un_defined_mi specific rate for age_grp_2
###################################################################

####for the first group of ages
un_defined_mi_by_age_grp_2$rate_age_grp_2_25_64 = case_when(
  un_defined_mi_by_age_grp_2$year_visit_start  == 2018 ~ un_defined_mi_by_age_grp_2$`25-64`/pop_2018_age_grp_2_25_64*fcrateper100ts,
  un_defined_mi_by_age_grp_2$year_visit_start  == 2019 ~ un_defined_mi_by_age_grp_2$`25-64`/pop_2019_age_grp_2_25_64*fcrateper100ts,
  un_defined_mi_by_age_grp_2$year_visit_start  == 2020 ~ un_defined_mi_by_age_grp_2$`25-64`/pop_2020_age_grp_2_25_64*fcrateper100ts,
  un_defined_mi_by_age_grp_2$year_visit_start  == 2021 ~ un_defined_mi_by_age_grp_2$`25-64`/pop_2021_age_grp_2_25_64*fcrateper100ts)

# Second group of age
un_defined_mi_by_age_grp_2$rate_age_grp_2_65_above = case_when(
  un_defined_mi_by_age_grp_2$year_visit_start  == 2018 ~ un_defined_mi_by_age_grp_2$`65+`/pop_2018_age_grp_2_above_65*fcrateper100ts,
  un_defined_mi_by_age_grp_2$year_visit_start  == 2019 ~ un_defined_mi_by_age_grp_2$`65+`/pop_2019_age_grp_2_above_65*fcrateper100ts,
  un_defined_mi_by_age_grp_2$year_visit_start  == 2020 ~ un_defined_mi_by_age_grp_2$`65+`/pop_2020_age_grp_2_above_65*fcrateper100ts,
  un_defined_mi_by_age_grp_2$year_visit_start  == 2021 ~ un_defined_mi_by_age_grp_2$`65+`/pop_2021_age_grp_2_above_65*fcrateper100ts)

####################
#convert to excel
####################
write_xlsx(un_defined_mi_by_age_grp_2, "D:/Data/tables/adjusted_rate/un_defined_mi_by_age_grp_2.xlsx")  
#############################################################################################


#################################################################
# specific rate for age_sex groups 
#################################################################
# population size in age_sex groups by year

pop_2018_female_25_64 = 2024400
pop_2019_female_25_64 = 2054400 
pop_2020_female_25_64 = 2083600
pop_2021_female_25_64 = 2111804

pop_2018_male_25_64 = 1977500
pop_2019_male_25_64 = 2010000
pop_2020_male_25_64 = 2041100
pop_2021_male_25_64 = 2072735

pop_2018_female_above_65 = 576200
pop_2019_female_above_65 = 596200
pop_2020_female_above_65 = 615600
pop_2021_female_above_65 = 634498 

pop_2018_male_above_65 = 461000
pop_2019_male_above_65 = 478800
pop_2020_male_above_65 = 495100
pop_2021_male_above_65 = 510825

###########################
#all visits by age_sex groups 
##########################
all_visits_age_sex_grp_2 =  erdata %>%
  group_by(year_visit_start, month_year_visit_start, gender_desc1,age_grp_2) %>%
  summarise(all_visits_by_age_sex_grp_2 = n())

library(tidyr)
all_visits_age_sex_grp_2 = all_visits_age_sex_grp_2   %>%
  pivot_wider(names_from = age_grp_2,
              values_from =all_visits_by_age_sex_grp_2 )

#create male and female df
all_visits_male_age_grp_2 = filter(all_visits_age_sex_grp_2 , gender_desc1 =="Male")
all_visits_female_age_grp_2 = filter(all_visits_age_sex_grp_2 , gender_desc1 =="Female")

#reaname the age groups
all_visits_male_age_grp_2 = all_visits_male_age_grp_2 %>%
  rename(
    male_25_64 = `25-64`,
    male_65_above = `65+`  
  )

all_visits_female_age_grp_2 = all_visits_female_age_grp_2 %>%
  rename(
    female_25_64 = `25-64`,
    female_65_above = `65+`  
  )

###################################################################
# all visits specific sex_age_group rate 
###################################################################

####for the first group of ages - female
all_visits_female_age_grp_2$rate_female_age_grp_2_25_64 = case_when(
  all_visits_female_age_grp_2$year_visit_start  == 2018 ~ all_visits_female_age_grp_2$female_25_64/pop_2018_female_25_64*fcrateper100ts,
  all_visits_female_age_grp_2$year_visit_start  == 2019 ~ all_visits_female_age_grp_2$female_25_64/pop_2019_female_25_64*fcrateper100ts,
  all_visits_female_age_grp_2$year_visit_start  == 2020 ~ all_visits_female_age_grp_2$female_25_64/pop_2020_female_25_64*fcrateper100ts,
  all_visits_female_age_grp_2$year_visit_start  == 2021 ~ all_visits_female_age_grp_2$female_25_64/pop_2021_female_25_64*fcrateper100ts)

# second group of ages - female 
all_visits_female_age_grp_2$rate_female_age_grp_2_65_above = case_when(
  all_visits_female_age_grp_2$year_visit_start  == 2018 ~ all_visits_female_age_grp_2$female_65_above/pop_2018_female_above_65*fcrateper100ts,
  all_visits_female_age_grp_2$year_visit_start  == 2019 ~ all_visits_female_age_grp_2$female_65_above/pop_2018_female_above_65*fcrateper100ts,
  all_visits_female_age_grp_2$year_visit_start  == 2020 ~ all_visits_female_age_grp_2$female_65_above/pop_2018_female_above_65*fcrateper100ts,
  all_visits_female_age_grp_2$year_visit_start  == 2021 ~ all_visits_female_age_grp_2$female_65_above/pop_2018_female_above_65*fcrateper100ts)

####################
#convert to excel
####################
write_xlsx(all_visits_female_age_grp_2, "D:/Data/tables/adjusted_rate/all_visits_female_age_grp_2.xlsx")  


###################################################################
# all visits specific sex_age_group rate 
###################################################################

####for the first group of ages - male
all_visits_male_age_grp_2$rate_male_age_grp_2_25_64 = case_when(
  all_visits_male_age_grp_2$year_visit_start  == 2018 ~ all_visits_male_age_grp_2$male_25_64/pop_2018_male_25_64*fcrateper100ts,
  all_visits_male_age_grp_2$year_visit_start  == 2019 ~ all_visits_male_age_grp_2$male_25_64/pop_2019_male_25_64*fcrateper100ts,
  all_visits_male_age_grp_2$year_visit_start  == 2020 ~ all_visits_male_age_grp_2$male_25_64/pop_2020_male_25_64*fcrateper100ts,
  all_visits_male_age_grp_2$year_visit_start  == 2021 ~ all_visits_male_age_grp_2$male_25_64/pop_2021_male_25_64*fcrateper100ts)

# Second group of ages - male 
all_visits_male_age_grp_2$rate_male_age_grp_2_65_above = case_when(
  all_visits_male_age_grp_2$year_visit_start  == 2018 ~ all_visits_male_age_grp_2$male_65_above/pop_2018_male_above_65*fcrateper100ts,
  all_visits_male_age_grp_2$year_visit_start  == 2019 ~ all_visits_male_age_grp_2$male_65_above/pop_2018_male_above_65*fcrateper100ts,
  all_visits_male_age_grp_2$year_visit_start  == 2020 ~ all_visits_male_age_grp_2$male_65_above/pop_2018_male_above_65*fcrateper100ts,
  all_visits_male_age_grp_2$year_visit_start  == 2021 ~ all_visits_male_age_grp_2$male_65_above/pop_2018_male_above_65*fcrateper100ts)

####################
#convert to excel
####################
write_xlsx(all_visits_male_age_grp_2, "D:/Data/tables/adjusted_rate/all_visits_male_age_grp_2.xlsx")  

all_visits_sex_age_grp_2_df = cbind.data.frame(all_visits_male_age_grp_2,all_visits_female_age_grp_2)

write_xlsx(all_visits_sex_age_grp_2_df , "D:/Data/tables/adjusted_rate/all_visits_sex_age_grp_2_df .xlsx")  

########################################################################

###################################################################
#  acs specific sex_age_group rate 
###################################################################

acs_age_sex_grp_2 =  erdata %>%
  group_by(year_visit_start, month_year_visit_start, ACS_char_desc ,gender_desc1,age_grp_2) %>%
  summarise(acs_by_age_sex_grp_2 = n())

acs_age_sex_grp_2 = drop_na(acs_age_sex_grp_2)

library(tidyr)
acs_age_sex_grp_2 = acs_age_sex_grp_2   %>%
  pivot_wider(names_from = age_grp_2,
              values_from =acs_by_age_sex_grp_2 )

#create male and female df
acs_male_age_grp_2 = filter(acs_age_sex_grp_2 , gender_desc1 =="Male")
acs_female_age_grp_2 = filter(acs_age_sex_grp_2 , gender_desc1 =="Female")

#reaname the age groups
acs_male_age_grp_2 = acs_male_age_grp_2 %>%
  rename(
    male_25_64 = `25-64`,
    
    male_65_above = `65+`  
  )

acs_female_age_grp_2 = acs_female_age_grp_2 %>%
  rename(
    female_25_64 = `25-64`,
    
    female_65_above = `65+`  
  )

###################################################################
# acs specific sex_age_group rate 
###################################################################

####for the first group of ages - female
acs_female_age_grp_2$acs_rate_female_age_grp_2_25_64 = case_when(
  acs_female_age_grp_2$year_visit_start  == 2018 ~ acs_female_age_grp_2$female_25_64/pop_2018_female_25_64*fcrateper100ts,
  acs_female_age_grp_2$year_visit_start  == 2019 ~ acs_female_age_grp_2$female_25_64/pop_2019_female_25_64*fcrateper100ts,
  acs_female_age_grp_2$year_visit_start  == 2020 ~ acs_female_age_grp_2$female_25_64/pop_2020_female_25_64*fcrateper100ts,
  acs_female_age_grp_2$year_visit_start  == 2021 ~ acs_female_age_grp_2$female_25_64/pop_2021_female_25_64*fcrateper100ts)

# Second group of ages - female 
acs_female_age_grp_2$acs_rate_female_age_grp_2_65_above = case_when(
  acs_female_age_grp_2$year_visit_start  == 2018 ~ acs_female_age_grp_2$female_65_above/pop_2018_female_above_65*fcrateper100ts,
  acs_female_age_grp_2$year_visit_start  == 2019 ~ acs_female_age_grp_2$female_65_above/pop_2018_female_above_65*fcrateper100ts,
  acs_female_age_grp_2$year_visit_start  == 2020 ~ acs_female_age_grp_2$female_65_above/pop_2018_female_above_65*fcrateper100ts,
  acs_female_age_grp_2$year_visit_start  == 2021 ~ acs_female_age_grp_2$female_65_above/pop_2018_female_above_65*fcrateper100ts)

####################
#convert to excel
####################
write_xlsx(acs_female_age_grp_2, "D:/Data/tables/adjusted_rate/acs_female_age_grp_2.xlsx")  

###################################################################
# acs specific sex_age_group rate 
###################################################################

####for the first group of ages - male
acs_male_age_grp_2$acs_rate_male_age_grp_2_25_64 = case_when(
  acs_male_age_grp_2$year_visit_start  == 2018 ~ acs_male_age_grp_2$male_25_64/pop_2018_male_25_64*fcrateper100ts,
  acs_male_age_grp_2$year_visit_start  == 2019 ~ acs_male_age_grp_2$male_25_64/pop_2019_male_25_64*fcrateper100ts,
  acs_male_age_grp_2$year_visit_start  == 2020 ~ acs_male_age_grp_2$male_25_64/pop_2020_male_25_64*fcrateper100ts,
  acs_male_age_grp_2$year_visit_start  == 2021 ~ acs_male_age_grp_2$male_25_64/pop_2021_male_25_64*fcrateper100ts)

# second group of ages - male 
acs_male_age_grp_2$acs_rate_male_age_grp_2_65_above = case_when(
  acs_male_age_grp_2$year_visit_start  == 2018 ~ acs_male_age_grp_2$male_65_above/pop_2018_male_above_65*fcrateper100ts,
  acs_male_age_grp_2$year_visit_start  == 2019 ~ acs_male_age_grp_2$male_65_above/pop_2018_male_above_65*fcrateper100ts,
  acs_male_age_grp_2$year_visit_start  == 2020 ~ acs_male_age_grp_2$male_65_above/pop_2018_male_above_65*fcrateper100ts,
  acs_male_age_grp_2$year_visit_start  == 2021 ~ acs_male_age_grp_2$male_65_above/pop_2018_male_above_65*fcrateper100ts)

####################
#convert to excel
####################
write_xlsx(acs_male_age_grp_2, "D:/Data/tables/adjusted_rate/acs_male_age_grp_2.xlsx")  

acs_sex_age_grp_2_df = cbind.data.frame(acs_male_age_grp_2,acs_female_age_grp_2)

write_xlsx(acs_sex_age_grp_2_df , "D:/Data/tables/adjusted_rate/acs_sex_age_grp_2_df .xlsx")  

#####################################################################################

###################################################################
# chest pain specific sex_age_group rate 
###################################################################

chest_pain_age_sex_grp_2 =  erdata %>%
  group_by(year_visit_start, month_year_visit_start, chest_pain_non_acs_num_desc ,gender_desc1,age_grp_2) %>%
  summarise(chest_pain_by_age_sex_grp_2 = n())

chest_pain_age_sex_grp_2 = drop_na(chest_pain_age_sex_grp_2)

library(tidyr)
chest_pain_age_sex_grp_2 = chest_pain_age_sex_grp_2   %>%
  pivot_wider(names_from = age_grp_2,
              values_from =chest_pain_by_age_sex_grp_2 )

#create male and female df
chest_pain_male_age_grp_2 = filter(chest_pain_age_sex_grp_2 , gender_desc1 =="Male")
chest_pain_female_age_grp_2 = filter(chest_pain_age_sex_grp_2 , gender_desc1 =="Female")

#reaname the age groups
chest_pain_male_age_grp_2 = chest_pain_male_age_grp_2 %>%
  rename(
    male_25_64 = `25-64`,
    male_65_above = `65+`  
  )

chest_pain_female_age_grp_2 = chest_pain_female_age_grp_2 %>%
  rename(
    female_25_64 = `25-64`,
    female_65_above = `65+`  
  )


###################################################################
# chest pain specific sex_age_group rate 
###################################################################

####for the first group of ages - female
chest_pain_female_age_grp_2$rate_female_age_grp_2_25_64 = case_when(
  chest_pain_female_age_grp_2$year_visit_start  == 2018 ~ chest_pain_female_age_grp_2$female_25_64/pop_2018_female_25_64*fcrateper100ts,
  chest_pain_female_age_grp_2$year_visit_start  == 2019 ~ chest_pain_female_age_grp_2$female_25_64/pop_2019_female_25_64*fcrateper100ts,
  chest_pain_female_age_grp_2$year_visit_start  == 2020 ~ chest_pain_female_age_grp_2$female_25_64/pop_2020_female_25_64*fcrateper100ts,
  chest_pain_female_age_grp_2$year_visit_start  == 2021 ~ chest_pain_female_age_grp_2$female_25_64/pop_2021_female_25_64*fcrateper100ts)

# Second group of ages - female 
chest_pain_female_age_grp_2$rate_female_age_grp_2_65_above = case_when(
  chest_pain_female_age_grp_2$year_visit_start  == 2018 ~ chest_pain_female_age_grp_2$female_65_above/pop_2018_female_above_65*fcrateper100ts,
  chest_pain_female_age_grp_2$year_visit_start  == 2019 ~ chest_pain_female_age_grp_2$female_65_above/pop_2018_female_above_65*fcrateper100ts,
  chest_pain_female_age_grp_2$year_visit_start  == 2020 ~ chest_pain_female_age_grp_2$female_65_above/pop_2018_female_above_65*fcrateper100ts,
  chest_pain_female_age_grp_2$year_visit_start  == 2021 ~ chest_pain_female_age_grp_2$female_65_above/pop_2018_female_above_65*fcrateper100ts)

####################
#convert to excel
####################
write_xlsx(chest_pain_female_age_grp_2, "D:/Data/tables/adjusted_rate/chest_pain_female_age_grp_2.xlsx")  

###################################################################
# chest pain specific sex_age_group rate 
###################################################################

####for the first group of ages - male
chest_pain_male_age_grp_2$rate_male_age_grp_2_25_64 = case_when(
  chest_pain_male_age_grp_2$year_visit_start  == 2018 ~ chest_pain_male_age_grp_2$male_25_64/pop_2018_male_25_64*fcrateper100ts,
  chest_pain_male_age_grp_2$year_visit_start  == 2019 ~ chest_pain_male_age_grp_2$male_25_64/pop_2019_male_25_64*fcrateper100ts,
  chest_pain_male_age_grp_2$year_visit_start  == 2020 ~ chest_pain_male_age_grp_2$male_25_64/pop_2020_male_25_64*fcrateper100ts,
  chest_pain_male_age_grp_2$year_visit_start  == 2021 ~ chest_pain_male_age_grp_2$male_25_64/pop_2021_male_25_64*fcrateper100ts)

# Second group of ages - male 
chest_pain_male_age_grp_2$rate_male_age_grp_2_65_above = case_when(
  chest_pain_male_age_grp_2$year_visit_start  == 2018 ~ chest_pain_male_age_grp_2$male_65_above/pop_2018_male_above_65*fcrateper100ts,
  chest_pain_male_age_grp_2$year_visit_start  == 2019 ~ chest_pain_male_age_grp_2$male_65_above/pop_2018_male_above_65*fcrateper100ts,
  chest_pain_male_age_grp_2$year_visit_start  == 2020 ~ chest_pain_male_age_grp_2$male_65_above/pop_2018_male_above_65*fcrateper100ts,
  chest_pain_male_age_grp_2$year_visit_start  == 2021 ~ chest_pain_male_age_grp_2$male_65_above/pop_2018_male_above_65*fcrateper100ts)

####################
#convert to excel
####################
write_xlsx(chest_pain_male_age_grp_2, "D:/Data/tables/adjusted_rate/chest_pain_male_age_grp_2.xlsx")  

chest_pain_sex_age_grp_2_df = cbind.data.frame(chest_pain_male_age_grp_2,chest_pain_female_age_grp_2)

write_xlsx(chest_pain_sex_age_grp_2_df , "D:/Data/tables/adjusted_rate/chest_pain_sex_age_grp_2_df .xlsx")  

##############################################################################################################

###################################################################
#  MI specific sex_age_group rate 
###################################################################

mi_age_sex_grp_2 =  erdata %>%
  group_by(year_visit_start, month_year_visit_start, myocardial_char_desc ,gender_desc1,age_grp_2) %>%
  summarise(mi_by_age_sex_grp_2 = n())

mi_age_sex_grp_2 = drop_na(mi_age_sex_grp_2)

library(tidyr)
mi_age_sex_grp_2 = mi_age_sex_grp_2   %>%
  pivot_wider(names_from = age_grp_2,
              values_from =mi_by_age_sex_grp_2 )

#create male and female df
mi_male_age_grp_2 = filter(mi_age_sex_grp_2 , gender_desc1 =="Male")
mi_female_age_grp_2 = filter(mi_age_sex_grp_2 , gender_desc1 =="Female")

#reaname the age groups
mi_male_age_grp_2 = mi_male_age_grp_2 %>%
  rename(
    male_25_64 = `25-64`,
    
    male_65_above = `65+`  
  )

mi_female_age_grp_2 = mi_female_age_grp_2 %>%
  rename(
    female_25_64 = `25-64`,
    
    female_65_above = `65+`  
  )

###################################################################
# mi specific sex_age_group rate 
###################################################################

####for the first group of ages - female
mi_female_age_grp_2$rate_female_age_grp_2_25_64 = case_when(
  mi_female_age_grp_2$year_visit_start  == 2018 ~ mi_female_age_grp_2$female_25_64/pop_2018_female_25_64*fcrateper100ts,
  mi_female_age_grp_2$year_visit_start  == 2019 ~ mi_female_age_grp_2$female_25_64/pop_2019_female_25_64*fcrateper100ts,
  mi_female_age_grp_2$year_visit_start  == 2020 ~ mi_female_age_grp_2$female_25_64/pop_2020_female_25_64*fcrateper100ts,
  mi_female_age_grp_2$year_visit_start  == 2021 ~ mi_female_age_grp_2$female_25_64/pop_2021_female_25_64*fcrateper100ts)

# Fourth group of ages - female 
mi_female_age_grp_2$rate_female_age_grp_2_65_above = case_when(
  mi_female_age_grp_2$year_visit_start  == 2018 ~ mi_female_age_grp_2$female_65_above/pop_2018_female_above_65*fcrateper100ts,
  mi_female_age_grp_2$year_visit_start  == 2019 ~ mi_female_age_grp_2$female_65_above/pop_2018_female_above_65*fcrateper100ts,
  mi_female_age_grp_2$year_visit_start  == 2020 ~ mi_female_age_grp_2$female_65_above/pop_2018_female_above_65*fcrateper100ts,
  mi_female_age_grp_2$year_visit_start  == 2021 ~ mi_female_age_grp_2$female_65_above/pop_2018_female_above_65*fcrateper100ts)

####################
#convert to excel
####################
write_xlsx(mi_female_age_grp_2, "D:/Data/tables/adjusted_rate/mi_female_age_grp_2.xlsx")  

###################################################################
# mi visits specific sex_age_group rate 
###################################################################

####for the first group of ages - male
mi_male_age_grp_2$rate_male_age_grp_2_25_64 = case_when(
  mi_male_age_grp_2$year_visit_start  == 2018 ~ mi_male_age_grp_2$male_25_64/pop_2018_male_25_64*fcrateper100ts,
  mi_male_age_grp_2$year_visit_start  == 2019 ~ mi_male_age_grp_2$male_25_64/pop_2019_male_25_64*fcrateper100ts,
  mi_male_age_grp_2$year_visit_start  == 2020 ~ mi_male_age_grp_2$male_25_64/pop_2020_male_25_64*fcrateper100ts,
  mi_male_age_grp_2$year_visit_start  == 2021 ~ mi_male_age_grp_2$male_25_64/pop_2021_male_25_64*fcrateper100ts)


# second group of ages - male 
mi_male_age_grp_2$rate_male_age_grp_2_65_above = case_when(
  mi_male_age_grp_2$year_visit_start  == 2018 ~ mi_male_age_grp_2$male_65_above/pop_2018_male_above_65*fcrateper100ts,
  mi_male_age_grp_2$year_visit_start  == 2019 ~ mi_male_age_grp_2$male_65_above/pop_2018_male_above_65*fcrateper100ts,
  mi_male_age_grp_2$year_visit_start  == 2020 ~ mi_male_age_grp_2$male_65_above/pop_2018_male_above_65*fcrateper100ts,
  mi_male_age_grp_2$year_visit_start  == 2021 ~ mi_male_age_grp_2$male_65_above/pop_2018_male_above_65*fcrateper100ts)

####################
#convert to excel
####################
write_xlsx(mi_male_age_grp_2, "D:/Data/tables/adjusted_rate/mi_male_age_grp_2.xlsx")  

mi_sex_age_grp_2_df = cbind.data.frame(mi_male_age_grp_2,mi_female_age_grp_2)

write_xlsx(mi_sex_age_grp_2_df , "D:/Data/tables/adjusted_rate/mi_sex_age_grp_2_df .xlsx")  

##########################################################################################


###################################################################
#  stemi specific sex_age_group rate 
###################################################################

stemi_age_sex_grp_2 =  erdata %>%
  group_by(year_visit_start, month_year_visit_start, STEMI_char_desc ,gender_desc1,age_grp_2) %>%
  summarise(stemi_by_age_sex_grp_2 = n())

stemi_age_sex_grp_2 = drop_na(stemi_age_sex_grp_2)

library(tidyr)
stemi_age_sex_grp_2 = stemi_age_sex_grp_2   %>%
  pivot_wider(names_from = age_grp_2,
              values_from =stemi_by_age_sex_grp_2 )

#create male and female df
stemi_male_age_grp_2 = filter(stemi_age_sex_grp_2 , gender_desc1 =="Male")
stemi_female_age_grp_2 = filter(stemi_age_sex_grp_2 , gender_desc1 =="Female")

#reaname the age groups
stemi_male_age_grp_2 = stemi_male_age_grp_2 %>%
  rename(
    male_25_64 = `25-64`,
    
    male_65_above = `65+`  
  )

stemi_female_age_grp_2 = stemi_female_age_grp_2 %>%
  rename(
    female_25_64 = `25-64`,
    
    female_65_above = `65+`  
  )

###################################################################
# stemi specific sex_age_group rate 
###################################################################

####for the first group of ages - female
stemi_female_age_grp_2$rate_female_age_grp_2_25_64 = case_when(
  stemi_female_age_grp_2$year_visit_start  == 2018 ~ stemi_female_age_grp_2$female_25_64/pop_2018_female_25_64*fcrateper100ts,
  stemi_female_age_grp_2$year_visit_start  == 2019 ~ stemi_female_age_grp_2$female_25_64/pop_2019_female_25_64*fcrateper100ts,
  stemi_female_age_grp_2$year_visit_start  == 2020 ~ stemi_female_age_grp_2$female_25_64/pop_2020_female_25_64*fcrateper100ts,
  stemi_female_age_grp_2$year_visit_start  == 2021 ~ stemi_female_age_grp_2$female_25_64/pop_2021_female_25_64*fcrateper100ts)

# Second group of ages - female 
stemi_female_age_grp_2$rate_female_age_grp_2_65_above = case_when(
  stemi_female_age_grp_2$year_visit_start  == 2018 ~ stemi_female_age_grp_2$female_65_above/pop_2018_female_above_65*fcrateper100ts,
  stemi_female_age_grp_2$year_visit_start  == 2019 ~ stemi_female_age_grp_2$female_65_above/pop_2018_female_above_65*fcrateper100ts,
  stemi_female_age_grp_2$year_visit_start  == 2020 ~ stemi_female_age_grp_2$female_65_above/pop_2018_female_above_65*fcrateper100ts,
  stemi_female_age_grp_2$year_visit_start  == 2021 ~ stemi_female_age_grp_2$female_65_above/pop_2018_female_above_65*fcrateper100ts)

####################
#convert to excel
####################
write_xlsx(stemi_female_age_grp_2, "D:/Data/tables/adjusted_rate/stemi_female_age_grp_2.xlsx")  

###################################################################
# stemi specific sex_age_group rate 
###################################################################

####for the first group of ages - male
stemi_male_age_grp_2$rate_male_age_grp_2_25_64 = case_when(
  stemi_male_age_grp_2$year_visit_start  == 2018 ~ stemi_male_age_grp_2$male_25_64/pop_2018_male_25_64*fcrateper100ts,
  stemi_male_age_grp_2$year_visit_start  == 2019 ~ stemi_male_age_grp_2$male_25_64/pop_2019_male_25_64*fcrateper100ts,
  stemi_male_age_grp_2$year_visit_start  == 2020 ~ stemi_male_age_grp_2$male_25_64/pop_2020_male_25_64*fcrateper100ts,
  stemi_male_age_grp_2$year_visit_start  == 2021 ~ stemi_male_age_grp_2$male_25_64/pop_2021_male_25_64*fcrateper100ts)

# Second group of ages - male 
stemi_male_age_grp_2$rate_male_age_grp_2_65_above = case_when(
  stemi_male_age_grp_2$year_visit_start  == 2018 ~ stemi_male_age_grp_2$male_65_above/pop_2018_male_above_65*fcrateper100ts,
  stemi_male_age_grp_2$year_visit_start  == 2019 ~ stemi_male_age_grp_2$male_65_above/pop_2018_male_above_65*fcrateper100ts,
  stemi_male_age_grp_2$year_visit_start  == 2020 ~ stemi_male_age_grp_2$male_65_above/pop_2018_male_above_65*fcrateper100ts,
  stemi_male_age_grp_2$year_visit_start  == 2021 ~ stemi_male_age_grp_2$male_65_above/pop_2018_male_above_65*fcrateper100ts)

####################
#convert to excel
####################
write_xlsx(stemi_male_age_grp_2, "D:/Data/tables/adjusted_rate/stemi_male_age_grp_2.xlsx")  

stemi_sex_age_grp_2_df = cbind.data.frame(stemi_male_age_grp_2,stemi_female_age_grp_2)

write_xlsx(stemi_sex_age_grp_2_df , "D:/Data/tables/adjusted_rate/stemi_sex_age_grp_2_df .xlsx")  


##################################################################################################################

###################################################################
#  nstemi specific sex_age_group rate 
###################################################################

nstemi_age_sex_grp_2 =  erdata %>%
  group_by(year_visit_start, month_year_visit_start, NSTEMI_char_desc ,gender_desc1,age_grp_2) %>%
  summarise(nstemi_by_age_sex_grp_2 = n())

nstemi_age_sex_grp_2 = drop_na(nstemi_age_sex_grp_2)

library(tidyr)
nstemi_age_sex_grp_2 = nstemi_age_sex_grp_2   %>%
  pivot_wider(names_from = age_grp_2,
              values_from =nstemi_by_age_sex_grp_2 )

#create male and female df
nstemi_male_age_grp_2 = filter(nstemi_age_sex_grp_2 , gender_desc1 =="Male")
nstemi_female_age_grp_2 = filter(nstemi_age_sex_grp_2 , gender_desc1 =="Female")

#reaname the age groups
nstemi_male_age_grp_2 = nstemi_male_age_grp_2 %>%
  rename(
    male_25_64 = `25-64`,
    
    male_65_above = `65+`  
  )

nstemi_female_age_grp_2 = nstemi_female_age_grp_2 %>%
  rename(
    female_25_64 = `25-64`,
    
    female_65_above = `65+`  
  )

###################################################################
# nstemi specific sex_age_group rate 
###################################################################

####for the first group of ages - female
nstemi_female_age_grp_2$rate_female_age_grp_2_25_64 = case_when(
  nstemi_female_age_grp_2$year_visit_start  == 2018 ~ nstemi_female_age_grp_2$female_25_64/pop_2018_female_25_64*fcrateper100ts,
  nstemi_female_age_grp_2$year_visit_start  == 2019 ~ nstemi_female_age_grp_2$female_25_64/pop_2019_female_25_64*fcrateper100ts,
  nstemi_female_age_grp_2$year_visit_start  == 2020 ~ nstemi_female_age_grp_2$female_25_64/pop_2020_female_25_64*fcrateper100ts,
  nstemi_female_age_grp_2$year_visit_start  == 2021 ~ nstemi_female_age_grp_2$female_25_64/pop_2021_female_25_64*fcrateper100ts)

# Second group of ages - female 
nstemi_female_age_grp_2$rate_female_age_grp_2_65_above = case_when(
  nstemi_female_age_grp_2$year_visit_start  == 2018 ~ nstemi_female_age_grp_2$female_65_above/pop_2018_female_above_65*fcrateper100ts,
  nstemi_female_age_grp_2$year_visit_start  == 2019 ~ nstemi_female_age_grp_2$female_65_above/pop_2018_female_above_65*fcrateper100ts,
  nstemi_female_age_grp_2$year_visit_start  == 2020 ~ nstemi_female_age_grp_2$female_65_above/pop_2018_female_above_65*fcrateper100ts,
  nstemi_female_age_grp_2$year_visit_start  == 2021 ~ nstemi_female_age_grp_2$female_65_above/pop_2018_female_above_65*fcrateper100ts)

####################
#convert to excel
####################
write_xlsx(nstemi_female_age_grp_2, "D:/Data/tables/adjusted_rate/nstemi_female_age_grp_2.xlsx")  

###################################################################
# nstemi specific sex_age_group rate 
###################################################################

####for the first group of ages - male
nstemi_male_age_grp_2$rate_male_age_grp_2_25_64 = case_when(
  nstemi_male_age_grp_2$year_visit_start  == 2018 ~ nstemi_male_age_grp_2$male_25_64/pop_2018_male_25_64*fcrateper100ts,
  nstemi_male_age_grp_2$year_visit_start  == 2019 ~ nstemi_male_age_grp_2$male_25_64/pop_2019_male_25_64*fcrateper100ts,
  nstemi_male_age_grp_2$year_visit_start  == 2020 ~ nstemi_male_age_grp_2$male_25_64/pop_2020_male_25_64*fcrateper100ts,
  nstemi_male_age_grp_2$year_visit_start  == 2021 ~ nstemi_male_age_grp_2$male_25_64/pop_2021_male_25_64*fcrateper100ts)

# second group of ages - male 
nstemi_male_age_grp_2$rate_male_age_grp_2_65_above = case_when(
  nstemi_male_age_grp_2$year_visit_start  == 2018 ~ nstemi_male_age_grp_2$male_65_above/pop_2018_male_above_65*fcrateper100ts,
  nstemi_male_age_grp_2$year_visit_start  == 2019 ~ nstemi_male_age_grp_2$male_65_above/pop_2018_male_above_65*fcrateper100ts,
  nstemi_male_age_grp_2$year_visit_start  == 2020 ~ nstemi_male_age_grp_2$male_65_above/pop_2018_male_above_65*fcrateper100ts,
  nstemi_male_age_grp_2$year_visit_start  == 2021 ~ nstemi_male_age_grp_2$male_65_above/pop_2018_male_above_65*fcrateper100ts)

####################
#convert to excel
####################
write_xlsx(nstemi_male_age_grp_2, "D:/Data/tables/adjusted_rate/nstemi_male_age_grp_2.xlsx")  

nstemi_sex_age_grp_2_df = cbind.data.frame(nstemi_male_age_grp_2,nstemi_female_age_grp_2)

write_xlsx(nstemi_sex_age_grp_2_df , "D:/Data/tables/adjusted_rate/nstemi_sex_age_grp_2_df .xlsx")  

#####################################################################################

###################################################################
#  un_defined_mi specific sex_age_group rate 
###################################################################

un_defined_mi_age_sex_grp_2 =  erdata %>%
  group_by(year_visit_start, month_year_visit_start, un_defined_mi_char_desc ,gender_desc1,age_grp_2) %>%
  summarise(un_defined_mi_by_age_sex_grp_2 = n())

un_defined_mi_age_sex_grp_2 = drop_na(un_defined_mi_age_sex_grp_2)

library(tidyr)
un_defined_mi_age_sex_grp_2 = un_defined_mi_age_sex_grp_2   %>%
  pivot_wider(names_from = age_grp_2,
              values_from =un_defined_mi_by_age_sex_grp_2 )

#create male and female df
un_defined_mi_male_age_grp_2 = filter(un_defined_mi_age_sex_grp_2 , gender_desc1 =="Male")
un_defined_mi_female_age_grp_2 = filter(un_defined_mi_age_sex_grp_2 , gender_desc1 =="Female")

#reaname the age groups
un_defined_mi_male_age_grp_2 = un_defined_mi_male_age_grp_2 %>%
  rename(
    male_25_64 = `25-64`,
    
    male_65_above = `65+`  
  )

un_defined_mi_female_age_grp_2 = un_defined_mi_female_age_grp_2 %>%
  rename(
    female_25_64 = `25-64`,
    
    female_65_above = `65+`  
  )

###################################################################
# un_defined_mi specific sex_age_group rate 
###################################################################

####for the first group of ages - female
un_defined_mi_female_age_grp_2$rate_female_age_grp_2_25_64 = case_when(
  un_defined_mi_female_age_grp_2$year_visit_start  == 2018 ~ un_defined_mi_female_age_grp_2$female_25_64/pop_2018_female_25_64*fcrateper100ts,
  un_defined_mi_female_age_grp_2$year_visit_start  == 2019 ~ un_defined_mi_female_age_grp_2$female_25_64/pop_2019_female_25_64*fcrateper100ts,
  un_defined_mi_female_age_grp_2$year_visit_start  == 2020 ~ un_defined_mi_female_age_grp_2$female_25_64/pop_2020_female_25_64*fcrateper100ts,
  un_defined_mi_female_age_grp_2$year_visit_start  == 2021 ~ un_defined_mi_female_age_grp_2$female_25_64/pop_2021_female_25_64*fcrateper100ts)

# Second group of ages - female 
un_defined_mi_female_age_grp_2$rate_female_age_grp_2_65_above = case_when(
  un_defined_mi_female_age_grp_2$year_visit_start  == 2018 ~ un_defined_mi_female_age_grp_2$female_65_above/pop_2018_female_above_65*fcrateper100ts,
  un_defined_mi_female_age_grp_2$year_visit_start  == 2019 ~ un_defined_mi_female_age_grp_2$female_65_above/pop_2018_female_above_65*fcrateper100ts,
  un_defined_mi_female_age_grp_2$year_visit_start  == 2020 ~ un_defined_mi_female_age_grp_2$female_65_above/pop_2018_female_above_65*fcrateper100ts,
  un_defined_mi_female_age_grp_2$year_visit_start  == 2021 ~ un_defined_mi_female_age_grp_2$female_65_above/pop_2018_female_above_65*fcrateper100ts)

####################
#convert to excel
####################
write_xlsx(un_defined_mi_female_age_grp_2, "D:/Data/tables/adjusted_rate/un_defined_mi_female_age_grp_2.xlsx")  

###################################################################
# un_defined_mi specific sex_age_group rate 
###################################################################

####for the first group of ages - male
un_defined_mi_male_age_grp_2$rate_male_age_grp_2_25_64 = case_when(
  un_defined_mi_male_age_grp_2$year_visit_start  == 2018 ~ un_defined_mi_male_age_grp_2$male_25_64/pop_2018_male_25_64*fcrateper100ts,
  un_defined_mi_male_age_grp_2$year_visit_start  == 2019 ~ un_defined_mi_male_age_grp_2$male_25_64/pop_2019_male_25_64*fcrateper100ts,
  un_defined_mi_male_age_grp_2$year_visit_start  == 2020 ~ un_defined_mi_male_age_grp_2$male_25_64/pop_2020_male_25_64*fcrateper100ts,
  un_defined_mi_male_age_grp_2$year_visit_start  == 2021 ~ un_defined_mi_male_age_grp_2$male_25_64/pop_2021_male_25_64*fcrateper100ts)

# Second group of ages - male 
un_defined_mi_male_age_grp_2$rate_male_age_grp_2_65_above = case_when(
  un_defined_mi_male_age_grp_2$year_visit_start  == 2018 ~ un_defined_mi_male_age_grp_2$male_65_above/pop_2018_male_above_65*fcrateper100ts,
  un_defined_mi_male_age_grp_2$year_visit_start  == 2019 ~ un_defined_mi_male_age_grp_2$male_65_above/pop_2018_male_above_65*fcrateper100ts,
  un_defined_mi_male_age_grp_2$year_visit_start  == 2020 ~ un_defined_mi_male_age_grp_2$male_65_above/pop_2018_male_above_65*fcrateper100ts,
  un_defined_mi_male_age_grp_2$year_visit_start  == 2021 ~ un_defined_mi_male_age_grp_2$male_65_above/pop_2018_male_above_65*fcrateper100ts)

####################
#convert to excel
####################
write_xlsx(un_defined_mi_male_age_grp_2, "D:/Data/tables/adjusted_rate/un_defined_mi_male_age_grp_2.xlsx")  

un_defined_mi_sex_age_grp_2_df = cbind.data.frame(un_defined_mi_male_age_grp_2,un_defined_mi_female_age_grp_2)

write_xlsx(un_defined_mi_sex_age_grp_2_df , "D:/Data/tables/adjusted_rate/un_defined_mi_sex_age_grp_2_df .xlsx")  

#####################################################################################

############################################
## Chest pain by socio-economic index 1-10
###########################################

#with region
#chest_pain_visits_ses_1_10_region =  erdata %>%
# group_by(year_visit_start, month_year_visit_start, chest_pain_non_acs_num_desc, region_code_eng_desc, cluster_2017) %>%
#summarise(chest_pain_by_ses_1_10_region = n())


#remove na 
# chest_pain_visits_ses_1_10_region  = drop_na(chest_pain_visits_ses_1_10_region ) 

# without region
chest_pain_visits_ses_1_10 =  erdata %>%
  group_by(year_visit_start, month_year_visit_start, chest_pain_non_acs_num_desc, cluster_2017) %>%
  summarise(chest_pain_by_ses_1_10 = n())


#remove na
chest_pain_visits_ses_1_10 = drop_na(chest_pain_visits_ses_1_10) #480 obs


# chest pain by ses 1_3
chest_pain_visits_ses_1_3=  erdata %>%
  group_by(year_visit_start, month_year_visit_start, chest_pain_non_acs_num_desc, ses_category) %>%
  summarise(chest_pain_by_ses_1_3 = n())

chest_pain_visits_ses_1_3 = drop_na(chest_pain_visits_ses_1_3)

#####################################################################
# Create a variable by a separation by period. 

erdata$period_type = as.factor(case_when(
  erdata$visit_start_date < "2020-03-01" ~ "Pre-pandemic period",
  erdata$visit_start_date >= "2020-03-01"& erdata$visit_start_date  <"2021-02-01" ~ "Pandemic period 1",
  erdata$visit_start_date >="2021-02-01"& erdata$visit_start_date  <="2021-12-31" ~ "Pandemic period 2",
  F ~ "")) # true ~ : is like else command.
erdata$period_type = relevel(erdata$period_type, "Pre-pandemic period")
levels(erdata$period_type)

#################################

#####################################################################
#delete un-use columns at this stage.
erdata=subset(erdata, select = -city_desc)
erdata=subset(erdata, select = -region_desc)
erdata=subset(erdata, select = -city_total_population)
erdata=subset(erdata, select = -city_total_populationc)
erdata=subset(erdata, select = -age_grp)
erdata=subset(erdata, select = -birth_country_code)
erdata=subset(erdata, select = -nation_desc)
erdata=subset(erdata, select = -nation_code)


##############################################################
# create data frame of all the variables with the rate.
##############################################################

df_monthly_rate = cbind.data.frame(ACS_visits_monthly_rate_df, 
                                   chest_pain_visits_rate, all_visits_rate_monthly_df,
                                   stemi_vists_rate_monthly_df, nstemi_vists_rate_monthly_df,
                                   ACS_by_gender,acs_by_age_grp_2,
                                   acs_sex_age_grp_2_df)

###########################################################################################
# From asus

# load df_monthly_rate
df_monthly_rate <- read_excel("Erdata/df_monthly_rate_from_march/df_monthly_rate.xlsx")

library(lubridate)
df_monthly_rate$time_by_month= seq(from = ymd("2018-01-01"), to = ymd("2021-12-31"), by="months")
############################################################################################
library(tsModel)#for the harmonic terms

month_number = rep(1:12, times = 4) 

#create the harmonic terms
four_harmonic = as.data.frame(harmonic(month_number,2,12))
one_from_4 = four_harmonic$V1
V1 = four_harmonic$V1

two_from_4 = four_harmonic$V2
V2 = four_harmonic$V2

three_from_4 =four_harmonic$V3
V3 =four_harmonic$V3

four_from_4 =four_harmonic$V4
V4=four_harmonic$V4

######################################################################################
# Create a seasonal dummy variables 
library(forecast)
library(quantmod)
library(zoo)
library(xts)
ACS_rate_females_65_above_ts_all_period = ts(df_monthly_rate$acs_rate_female_age_grp_2_65_above, start = 2018, frequency = 12)
adf.test(ACS_rate_females_65_above_ts_all_period)
Year_dummy=seasonaldummy(ACS_rate_females_65_above_ts_all_period)

################################################################################################

df_monthly_rate= cbind.data.frame(df_monthly_rate, four_harmonic, Year_dummy)


xreg1 = as.matrix(df_monthly_rate[,c("time","level_first", "trend_first", "level_second", "trend_second", "V1","V2","V3", "V4")])
xreg1df = as.data.frame(df_monthly_rate[,c("time","level_first", "trend_first", "level_second", "trend_second", "V1","V2","V3", "V4")])


library(dplyr)
library(lubridate)
#create a time by month sequence
df_monthly_rate$time_by_month= seq(from = ymd("2018-01-01"), to = ymd("2021-12-31"), by="month")


#######################################
df_monthly_rate # do not delete
######################################

##########################################################################################################
# preparation of the months for ITS - first interruption in March 2020 and the second in February 2021
##########################################################################################################
months_by_name <- read_excel("D:/Data/tables/months_by_name.xlsx")

# creates the months names in difference types
df_monthly_rate$month_name_1 = months_by_name$month_name_1
df_monthly_rate$month_name_2 = months_by_name$month_name_2
df_monthly_rate$month_name_3 = months_by_name$month_name_3

#create a month number variable for the furier terms (harmonic)
df_monthly_rate$month_number = rep(1:12, times = 4) 

#create the time variable
df_monthly_rate$time = as.numeric(1:48)
time = as.numeric(1:48)

#create the first level variable, the first interruption is from February 2020 (so March 2020 is the 26 month)
#fill with zeros from 1 to 26 month then fill with 1 from the 26 month (march 2020) to the end of the data
df_monthly_rate$level_first = c(rep(0,26), rep(1,22))
# was c(rep(0,25), rep(1,23)) so it's 0,0,0....(until 26) and then (from 27 point) 1,1,1,1
level_first = c(rep(0,26), rep(1,22))

#create the first trend variable, fill with zero months 1:25 and from month 26 a sequence of 1:23
df_monthly_rate$trend_first = c(rep(0,26), rep(1:22))
trend_first = c(rep(0,26), rep(1:22))

#create the second level variable, fill with zeros untill month 37 (Jan 2021)
# and then from month 38 (February 2021) with the value 1 (11 times)
df_monthly_rate$level_second = c(rep(0,37), rep(1,11))
# stayed the same from previous 
level_second = c(rep(0,37), rep(1,11))

#create the second trend variable, untill month 37 fill zeros and from month 38 with a sequence of 1:11
df_monthly_rate$trend_second = c(rep(0,37), rep(1:11))
# stayed the same from previous 
trend_second = c(rep(0,37), rep(1:11))


####################################
# convert the data frame to excel 
###################################
library(writexl)
write_xlsx(df_monthly_rate, "D:/Data/tables/adjusted_rate/df_monthly_crude_rate.xlsx")  

