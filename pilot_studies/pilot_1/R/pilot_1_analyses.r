library(tidyverse)
library(ggpubr)
library(Hmisc)
#plotting implicit citicism mindset and explicit criticism mindset


ggscatter(final_data, x = "crit_d", y = "crms_avg", 
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Implicit Criticism Mindset", ylab = "Explicit Criticism Mindset",
          add.params = list(color = "darkblue",
                            fill = "gray27"))



#analysing data

#normally distributed variables
shapiro.test(final_data$challange_d)
shapiro.test(final_data$crit_d)
shapiro.test(final_data$crms_avg)

#not normally distributed variables
shapiro.test(final_data$iqms_avg)
shapiro.test(final_data$chms_avg)
shapiro.test(final_data$crsc_avg)


#correlations with Pearson
Hmisc::rcorr(data.matrix(final_data[, c("crit_d","crms_avg")]),
             type = "pearson")

#correlations with Spearman
Hmisc::rcorr(data.matrix(final_data[, c("challange_d", "crit_d",
                                        "iqms_avg", "crms_avg", "chms_avg", "fms_avg",
                                        "fsc_avg", "crsc_avg", "challengescenario_1")]),
             type = "spearman")



