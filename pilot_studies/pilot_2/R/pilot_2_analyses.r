library(tidyverse)
library(Hmisc)

final_data<- read.csv("data/final_data/final_pilot_2.csv")
#analysing data

#normally distributed variables
shapiro.test(final_data$challange_d)
shapiro.test(final_data$crit_d)


#not normally distributed variables
shapiro.test(final_data$iqms_avg)
shapiro.test(final_data$chms_avg)
shapiro.test(final_data$crsc_avg)
shapiro.test(final_data$crms_avg)



#correlations with Spearman
Hmisc::rcorr(data.matrix(final_data[, c("challange_d", "crit_d",
                                        "iqms_avg", "crms_avg", "chms_avg", "fms_avg",
                                        "fsc_avg", "crsc_avg", "challengescenario_1")]),
             type = "spearman")



