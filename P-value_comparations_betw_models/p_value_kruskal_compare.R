##############################################
# P-value comparation Kruskal wallis test
#############################################


# Chest pain comparations 
##############################

# Kruskal Wallis ran sum test
chest_pain_pre_period=(fitted(model_chest_pain_final_p11q1_d)[1:26])
chest_pain_pandemic_period_1= (fitted(model_chest_pain_final_p11q1_d)[27:37])
chest_pain_pandemic_period_2 =(fitted(model_chest_pain_final_p11q1_d)[38:48])

# Crate the data frame
chest_pain_compare_periods = data.frame(period_type=c(rep("pre_pandemic", 26), rep("pandemic_period_1", 11), rep("pandemic_period_2", 11)), 
                                        rate=c(chest_pain_pre_period, chest_pain_pandemic_period_1, chest_pain_pandemic_period_2))

kruskal.test(rate ~ period_type, data=chest_pain_compare_periods)

# ACS general comparations 
##############################

# Kruskal Wallis ran sum test
acs_pre_period=(fitted(model_acf_final_p0q5_sep)[1:26])
acs_pandemic_period_1= (fitted(model_acf_final_p0q5_sep)[27:37])
acs_pandemic_period_2 =(fitted(model_acf_final_p0q5_sep)[38:48])

# Crate the data frame
acs_compare_periods = data.frame(period_type=c(rep("pre_pandemic", 26), rep("pandemic_period_1", 11), rep("pandemic_period_2", 11)), 
                                 rate=c(acs_pre_period, acs_pandemic_period_1, acs_pandemic_period_2))

kruskal.test(rate ~ period_type, data=acs_compare_periods) #p-value =1.873e-07

#####################################################################

# ACS young women comparations 
##############################

# Kruskal Wallis ran sum test
acs_female_25_64_pre_period=(fitted(model_acs_female_25_64_final_p9q1_may)[1:26])
acs_female_25_64_pandemic_period_1= (fitted(model_acs_female_25_64_final_p9q1_may)[27:37])
acs_female_25_64_pandemic_period_2 =(fitted(model_acs_female_25_64_final_p9q1_may)[38:48])

# Crate the data frame
acs_female_25_64_compare_periods = data.frame(period_type=c(rep("pre_pandemic", 26), rep("pandemic_period_1", 11), rep("pandemic_period_2", 11)), 
                                              rate=c(acs_female_25_64_pre_period, acs_female_25_64_pandemic_period_1, acs_female_25_64_pandemic_period_2))

kruskal.test(rate ~ period_type, data=acs_female_25_64_compare_periods) #p-value = 8.608e-05

#####################################################################
# ACS young men comparations 
##############################

# Kruskal Wallis ran sum test
acs_male_25_64_pre_period=(fitted(model_acs_male_25_64_final_p1q0)[1:26])
acs_male_25_64_pandemic_period_1= (fitted(model_acs_male_25_64_final_p1q0)[27:37])
acs_male_25_64_pandemic_period_2 =(fitted(model_acs_male_25_64_final_p1q0)[38:48])

# Crate the data frame
acs_male_25_64_compare_periods = data.frame(period_type=c(rep("pre_pandemic", 26), rep("pandemic_period_1", 11), rep("pandemic_period_2", 11)), 
                                            rate=c(acs_male_25_64_pre_period, acs_male_25_64_pandemic_period_1, acs_male_25_64_pandemic_period_2))

kruskal.test(rate ~ period_type, data=acs_male_25_64_compare_periods) #p-value = 1.827e-08

#####################################################################

#####################################################################
# ACS old women comparations 
##############################
# Kruskal Wallis ran sum test
acs_female_65_older_pre_period=(fitted(model_acs_female_65_above_final_corsym_sep)[1:26])
acs_female_65_older_pandemic_period_1= (fitted(model_acs_female_65_above_final_corsym_sep)[27:37])
acs_female_65_older_pandemic_period_2 =(fitted(model_acs_female_65_above_final_corsym_sep)[38:48])

# Crate the data frame
acs_female_65_above_compare_periods = data.frame(period_type=c(rep("pre_pandemic", 26), rep("pandemic_period_1", 11), rep("pandemic_period_2", 11)), 
                                                 rate=c(acs_female_65_older_pre_period, acs_female_65_older_pandemic_period_1, acs_female_65_older_pandemic_period_2))

kruskal.test(rate ~ period_type, data=acs_female_65_above_compare_periods) #p-value = 5.972e-06
#####################################################################

# ACS old men comparations 
##############################
# Kruskal Wallis ran sum test
acs_male_65_older_pre_period=(fitted(model_acs_male_65_above_final_p2q0)[1:26])
acs_male_65_older_pandemic_period_1= (fitted(model_acs_male_65_above_final_p2q0)[27:37])
acs_male_65_older_pandemic_period_2 =(fitted(model_acs_male_65_above_final_p2q0)[38:48])

# Crate the data frame
acs_male_65_above_compare_periods = data.frame(period_type=c(rep("pre_pandemic", 26), rep("pandemic_period_1", 11), rep("pandemic_period_2", 11)), 
                                               rate=c(acs_male_65_older_pre_period, acs_male_65_older_pandemic_period_1, acs_male_65_older_pandemic_period_2))

kruskal.test(rate ~ period_type, data=acs_male_65_above_compare_periods) #p-value = 0.000787
#####################################################################