# Creating Table 1

ibrary(gtsummary)
library(gt)
library(flextable)

# acs only with overall

table1_acs_overall = erdata %>% 
  select (period_type,ACS_char_desc , gender_desc1,patient_age,age_grp_2,
          city_population_2017_type,patient_releas_desc,ses_category
  ) %>% 
  tbl_summary(by = "period_type" , missing = "no",
              
              statistic = list(patient_age ~ c("{median} ({p25}, {p75})")#, 
                               #c(cluster_2017)~"{mean} ({sd})")#
              ), 
              digits = list(all_continuous()~1),
              
              type =list(gender_desc1~ "categorical",patient_age~"continuous",
                         age_grp_2~"categorical", patient_releas_desc~"categorical",
                         ses_category~"categorical"),
              label = list(age_grp_2="Age groups",
                           patient_age="Age, years",
                           gender_desc1 = "Gender",
                           ACS_char_desc  = "ACS (ICD-9 410,411)",
                           ses_category="SES",
                           patient_releas_desc = "Post ED status",
                           city_population_2017_type ="Urbanity"
              )
  )%>%
  
  modify_caption("**Table 1. Characteristic of the ACS and Non-ACS chest pain ED's visits, Israel 2018-2021**") %>%
  modify_header(label~"**Variable**") %>%
  bold_labels() %>%
  add_overall() %>%
  modify_table_styling(columns = label, rows = label=="Chest pain", 
                       footnote = "Chest pain visits, which did not contain any ACS codes")%>%
  #add_difference() - the "by" need to have exactly two levels. 
  #add_p(test=list(all_categorical() ~ "kruskal.test", all_continuous()~"aov")) %>%
  
  modify_table_styling(columns = label, rows = label=="Urbanity", 
                       footnote = "The patient's place of residence. 'City', population of 20,000 people and more. 
                     Rural, village, or small town with a population less than 20,000 people.
                     The population size is according to the 2017 Central Bureau of Statistics registry")%>%
  
  modify_table_styling(columns = label, rows = label=="SES", 
                       footnote = "Socioeconomic status, based on the patient's place of residence
                       according to the characteristics defined by Central Bureau of Statistics")%>%
  
  modify_footnote(stat_1~ "Pre-pandemic period: 1 Jan 2018 - 29 Feb 2020",
                  stat_2~"Pandemic period 1: 1 Mar 2020 - 31 Jan 2021",
                  stat_3~"Pandemic period 2: 1 Feb 2021 - 31 Dec 2021")

table1_acs_overall   
####################################################
# acs only without overall

table1_acs = erdata %>% 
  select (period_type,ACS_char_desc , gender_desc1,patient_age,age_grp_2,
          city_population_2017_type,patient_releas_desc,ses_category
  ) %>% 
  tbl_summary(by = "period_type" , missing = "no",
              
              statistic = list(patient_age ~ c("{median} ({p25}, {p75})")#, 
                               #c(cluster_2017)~"{mean} ({sd})")#
              ), 
              digits = list(all_continuous()~1),
              
              type =list(gender_desc1~ "categorical",patient_age~"continuous",
                         age_grp_2~"categorical", patient_releas_desc~"categorical",
                         ses_category~"categorical"),
              label = list(age_grp_2="Age groups",
                           patient_age="Age, years",
                           gender_desc1 = "Gender",
                           ACS_char_desc  = "ACS (ICD-9 410,411)",
                           ses_category="SES",
                           patient_releas_desc = "Post ED status",
                           city_population_2017_type ="Urbanity"
              )
  )%>%
  
  modify_caption("**Table 1. Characteristic of the ACS and Non-ACS chest pain ED's visits, Israel 2018-2021**") %>%
  modify_header(label~"**Variable**") %>%
  bold_labels() %>%
  modify_table_styling(columns = label, rows = label=="Chest pain", 
                       footnote = "Chest pain visits, which did not contain any ACS codes")%>%
  #add_difference() - the "by" need to have exactly two levels. 
  #add_p(test=list(all_categorical() ~ "kruskal.test", all_continuous()~"aov")) %>%
  
  modify_table_styling(columns = label, rows = label=="Urbanity", 
                       footnote = "The patient's place of residence. 'City', population of 20,000 people and more. 
                     Rural, village, or small town with a population less than 20,000 people.
                     The population size is according to the 2017 Central Bureau of Statistics registry")%>%
  
  modify_table_styling(columns = label, rows = label=="SES", 
                       footnote = "Socioeconomic status, based on the patient's place of residence
                       according to the characteristics defined by Central Bureau of Statistics")%>%
  
  modify_footnote(stat_1~ "Pre-pandemic period: 1 Jan 2018 - 29 Feb 2020",
                  stat_2~"Pandemic period 1: 1 Mar 2020 - 31 Jan 2021",
                  stat_3~"Pandemic period 2: 1 Feb 2021 - 31 Dec 2021")

table1_acs

################################################
#chest pain with overall 

table1_chest_pain_overall = erdata %>% 
  select (period_type,chest_pain_non_acs_char_desc , gender_desc1,patient_age,age_grp_2,
          city_population_2017_type,patient_releas_desc,ses_category
  ) %>% 
  tbl_summary(by = "period_type" , missing = "no",
              
              statistic = list(patient_age ~ c("{median} ({p25}, {p75})")#, 
                               #c(cluster_2017)~"{mean} ({sd})")#
              ), 
              digits = list(all_continuous()~1),
              
              type =list(gender_desc1~ "categorical",patient_age~"continuous",
                         age_grp_2~"categorical", patient_releas_desc~"categorical",
                         ses_category~"categorical"),
              label = list(age_grp_2="Age groups",
                           patient_age="Age, years",
                           gender_desc1 = "Gender",
                           chest_pain_non_acs_char_desc = "Chest pain (ICD-9 786.5)",
                           ses_category="SES",
                           patient_releas_desc = "Post ED status",
                           city_population_2017_type ="Urbanity"
              )
  )%>%
  
  modify_caption("**Table 1. Characteristic of the ACS and Non-ACS chest pain ED's visits, Israel 2018-2021**") %>%
  modify_header(label~"**Variable**") %>%
  bold_labels() %>%
  add_overall() %>%
  modify_table_styling(columns = label, rows = label=="Chest pain", 
                       footnote = "Chest pain visits, which did not contain any ACS codes")%>%
  #add_difference() - the "by" need to have exactly two levels. 
  #add_p(test=list(all_categorical() ~ "kruskal.test", all_continuous()~"aov")) %>%
  
  modify_table_styling(columns = label, rows = label=="Urbanity", 
                       footnote = "The patient's place of residence. 'City', population of 20,000 people and more. 
                     Rural, village, or small town with a population less than 20,000 people.
                     The population size is according to the 2017 Central Bureau of Statistics registry")%>%
  
  modify_table_styling(columns = label, rows = label=="SES", 
                       footnote = "Socioeconomic status, based on the patient's place of residence
                       according to the characteristics defined by Central Bureau of Statistics")%>%
  
  modify_footnote(stat_1~ "Pre-pandemic period: 1 Jan 2018 - 29 Feb 2020",
                  stat_2~"Pandemic period 1: 1 Mar 2020 - 31 Jan 2021",
                  stat_3~"Pandemic period 2: 1 Feb 2021 - 31 Dec 2021")

table1_chest_pain_overall
#############################################################
#chest pain with overall 

table1_chest_pain= erdata %>% 
  select (period_type,chest_pain_non_acs_char_desc , gender_desc1,patient_age,age_grp_2,
          city_population_2017_type,patient_releas_desc,ses_category
  ) %>% 
  tbl_summary(by = "period_type" , missing = "no",
              
              statistic = list(patient_age ~ c("{median} ({p25}, {p75})")#, 
                               #c(cluster_2017)~"{mean} ({sd})")#
              ), 
              digits = list(all_continuous()~1),
              
              type =list(gender_desc1~ "categorical",patient_age~"continuous",
                         age_grp_2~"categorical", patient_releas_desc~"categorical",
                         ses_category~"categorical"),
              label = list(age_grp_2="Age groups",
                           patient_age="Age, years",
                           gender_desc1 = "Gender",
                           chest_pain_non_acs_char_desc = "Chest pain (ICD-9 786.5)",
                           ses_category="SES",
                           patient_releas_desc = "Post ED status",
                           city_population_2017_type ="Urbanity"
              )
  )%>%
  
  modify_caption("**Table 1. Characteristic of the ACS and Non-ACS chest pain ED's visits, Israel 2018-2021**") %>%
  modify_header(label~"**Variable**") %>%
  bold_labels() %>%
  
  modify_table_styling(columns = label, rows = label=="Chest pain", 
                       footnote = "Chest pain visits, which did not contain any ACS codes")%>%
  #add_difference() - the "by" need to have exactly two levels. 
  #add_p(test=list(all_categorical() ~ "kruskal.test", all_continuous()~"aov")) %>%
  
  modify_table_styling(columns = label, rows = label=="Urbanity", 
                       footnote = "The patient's place of residence. 'City', population of 20,000 people and more. 
                     Rural, village, or small town with a population less than 20,000 people.
                     The population size is according to the 2017 Central Bureau of Statistics registry")%>%
  
  modify_table_styling(columns = label, rows = label=="SES", 
                       footnote = "Socioeconomic status, based on the patient's place of residence
                       according to the characteristics defined by Central Bureau of Statistics")%>%
  
  modify_footnote(stat_1~ "Pre-pandemic period: 1 Jan 2018 - 29 Feb 2020",
                  stat_2~"Pandemic period 1: 1 Mar 2020 - 31 Jan 2021",
                  stat_3~"Pandemic period 2: 1 Feb 2021 - 31 Dec 2021")

table1_chest_pain

####################################################
# merge tables
##################

# tables of overall
table_merge_overall = tbl_merge(
  tbls = list(table1_chest_pain_overall,table1_acs_overall),
  tab_spanner = c("**Chest pain non ACS", "ACS"))

# tables witout everall
table_merge = tbl_merge(
  tbls = list(table1_chest_pain,table1_acs),
  tab_spanner = c("**Chest pain non ACS", "ACS"))

################################################################
# by = acs and checst pain

# acs only without overall

table1_acs = erdata %>% 
  select (period_type,ACS_char_desc , gender_desc1,patient_age,age_grp_2,
          city_population_2017_type,patient_releas_desc,ses_category
  ) %>% 
  tbl_summary(by = "period_type" , missing = "no",
              
              statistic = list(patient_age ~ c("{median} ({p25}, {p75})")#, 
                               #c(cluster_2017)~"{mean} ({sd})")#
              ), 
              digits = list(all_continuous()~1),
              
              type =list(gender_desc1~ "categorical",patient_age~"continuous",
                         age_grp_2~"categorical", patient_releas_desc~"categorical",
                         ses_category~"categorical"),
              label = list(age_grp_2="Age groups",
                           patient_age="Age, years",
                           gender_desc1 = "Gender",
                           ACS_char_desc  = "ACS (ICD-9 410,411)",
                           ses_category="SES",
                           patient_releas_desc = "Post ED status",
                           city_population_2017_type ="Urbanity"
              )
  )%>%
  
  modify_caption("**Table 1. Characteristic of the ACS and Non-ACS chest pain ED's visits, Israel 2018-2021**") %>%
  modify_header(label~"**Variable**") %>%
  bold_labels() %>%
  modify_table_styling(columns = label, rows = label=="Chest pain", 
                       footnote = "Chest pain visits, which did not contain any ACS codes")%>%
  #add_difference() - the "by" need to have exactly two levels. 
  #add_p(test=list(all_categorical() ~ "kruskal.test", all_continuous()~"aov")) %>%
  
  modify_table_styling(columns = label, rows = label=="Urbanity", 
                       footnote = "The patient's place of residence. 'City', population of 20,000 people and more. 
                     Rural, village, or small town with a population less than 20,000 people.
                     The population size is according to the 2017 Central Bureau of Statistics registry")%>%
  
  modify_table_styling(columns = label, rows = label=="SES", 
                       footnote = "Socioeconomic status, based on the patient's place of residence
                       according to the characteristics defined by Central Bureau of Statistics")%>%
  
  modify_footnote(stat_1~ "Pre-pandemic period: 1 Jan 2018 - 29 Feb 2020",
                  stat_2~"Pandemic period 1: 1 Mar 2020 - 31 Jan 2021",
                  stat_3~"Pandemic period 2: 1 Feb 2021 - 31 Dec 2021")

table1_acs

##########################################################
erdata$ACS_num_desc
erdata$chest_pain_non_acs_char_desc
erdata_acs = filter(erdata, ACS_char_desc == "ACS")
erdata_chest_pain = filter(erdata, chest_pain_non_acs_char_desc == "Chest pain")

# acs only without overall

table1_acs_1 = erdata_acs %>% 
  select (period_type,ACS_char_desc , gender_desc1,patient_age,age_grp_2,
          city_population_2017_type,patient_releas_desc,ses_category
  ) %>% 
  tbl_summary(by = "period_type" , missing = "no",
              
              statistic = list(patient_age ~ c("{median} ({p25}, {p75})")#, 
                               #c(cluster_2017)~"{mean} ({sd})")#
              ), 
              digits = list(all_continuous()~1),
              
              type =list(gender_desc1~ "categorical",patient_age~"continuous",
                         age_grp_2~"categorical", patient_releas_desc~"categorical",
                         ses_category~"categorical"),
              label = list(age_grp_2="Age groups",
                           patient_age="Age, years",
                           gender_desc1 = "Gender",
                           ACS_char_desc  = "ACS (ICD-9 410,411)",
                           ses_category="SES",
                           patient_releas_desc = "Post ED status",
                           city_population_2017_type ="Urbanity"
              )
  )%>%
  
  modify_caption("**Table 1. Characteristic of ACS ED visits, Israel 2018-2021**") %>%
  modify_header(label~"**Variable**") %>%
  bold_labels() %>%
  modify_table_styling(columns = label, rows = label=="Chest pain", 
                       footnote = "Chest pain visits, which did not contain any ACS codes")%>%
  #add_difference() - the "by" need to have exactly two levels. 
  #add_p(test=list(all_categorical() ~ "kruskal.test", all_continuous()~"aov")) %>%
  
  modify_table_styling(columns = label, rows = label=="Urbanity", 
                       footnote = "The patient's place of residence. 'City', population of 20,000 people and more. 
                     Rural, village, or small town with a population less than 20,000 people.
                     The population size is according to the 2017 Central Bureau of Statistics registry")%>%
  
  modify_table_styling(columns = label, rows = label=="SES", 
                       footnote = "Socioeconomic status, based on the patient's place of residence
                       according to the characteristics defined by Central Bureau of Statistics")%>%
  
  modify_footnote(stat_1~ "Pre-pandemic period: 1 Jan 2018 - 29 Feb 2020",
                  stat_2~"Pandemic period 1: 1 Mar 2020 - 31 Jan 2021",
                  stat_3~"Pandemic period 2: 1 Feb 2021 - 31 Dec 2021")

table1_acs_1

###################################################################

#chest pain without overall

table1_chest_pain_1= erdata_chest_pain %>% 
  select (period_type,chest_pain_non_acs_char_desc , gender_desc1,patient_age,age_grp_2,
          city_population_2017_type,patient_releas_desc,ses_category
  ) %>% 
  tbl_summary(by = "period_type" , missing = "no",
              
              statistic = list(patient_age ~ c("{median} ({p25}, {p75})")#, 
                               #c(cluster_2017)~"{mean} ({sd})")#
              ), 
              digits = list(all_continuous()~1),
              
              type =list(gender_desc1~ "categorical",patient_age~"continuous",
                         age_grp_2~"categorical", patient_releas_desc~"categorical",
                         ses_category~"categorical"),
              label = list(age_grp_2="Age groups",
                           patient_age="Age, years",
                           gender_desc1 = "Gender",
                           chest_pain_non_acs_char_desc = "Chest pain (ICD-9 786.5)",
                           ses_category="SES",
                           patient_releas_desc = "Post ED status",
                           city_population_2017_type ="Urbanity"
              )
  )%>%
  
  modify_caption("**Table 1. Characteristic of the ACS and Non-ACS chest pain ED's visits, Israel 2018-2021**") %>%
  modify_header(label~"**Variable**") %>%
  bold_labels() %>%
  
  modify_table_styling(columns = label, rows = label=="Chest pain", 
                       footnote = "Chest pain visits, which did not contain any ACS codes")%>%
  
  add_p(test=list(all_categorical() ~ "kruskal.test", all_continuous()~"aov")) %>%
  
  modify_table_styling(columns = label, rows = label=="Urbanity", 
                       footnote = "The patient's place of residence. 'City', population of 20,000 people and more. 
                     Rural, village, or small town with a population less than 20,000 people.
                     The population size is according to the 2017 Central Bureau of Statistics registry")%>%
  
  modify_table_styling(columns = label, rows = label=="SES", 
                       footnote = "Socioeconomic status, based on the patient's place of residence
                       according to the characteristics defined by Central Bureau of Statistics")%>%
  
  modify_footnote(stat_1~ "Pre-pandemic period: 1 Jan 2018 - 29 Feb 2020",
                  stat_2~"Pandemic period 1: 1 Mar 2020 - 31 Jan 2021",
                  stat_3~"Pandemic period 2: 1 Feb 2021 - 31 Dec 2021")

table1_chest_pain_1

###################################################

# tables witout everall
table_merge2 = tbl_merge(
  tbls = list(table1_chest_pain_1,table1_acs_1),
  tab_spanner = c("**Chest pain non ACS**", "**ACS**"))