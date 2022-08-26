library(tidyverse)
library(report)
library(skimr)
library(sjPlot)
library(Hmisc)
library(performance)
library(qqplotr)
library(reghelper)
library(psych)

#setting working directory
setwd("main_study")

#Reading final data

processed_final<- read.csv("data/final_data/final_main.csv") %>% 
  select(-c(1))

#checking descriptive statistics
processed_final %>% 
  dplyr::select( 
                d_sens_chall, d_sens_crit, criticism_d, challange_d,
                chms_avg, crms_avg, chsc, crsc_avg, iqms_avg,
                perc_score, effort, resilience, challenge_seeking, persistence) %>% 
  skim()

#creating distribution plots for the PERC score

processed_final %>% 
  ggplot() +
  aes(x = perc_score) +
  geom_histogram(bins = 20) +
  labs(title = "Distribution of the perc score")

#creating distribution plots for the PERC subscores
processed_final %>% 
  select(effort, persistence, resilience, challenge_seeking) %>% 
  pivot_longer(everything()) %>% 
  mutate(name = fct_inorder(name)) %>% 
  ggplot() +
  aes(x = value) +
  geom_histogram(bins = 25) +
  scale_x_continuous(labels = scales::percent_format()) +
  facet_wrap(~name)


#H1: More positive implicit criticism attitude score will be a positive predictor of the behavioral PERC score.

mod1<-lm(scale(perc_score)~scale(d_sens_crit),
         data=processed_final)
summary(mod1)

# check_model(mod1, 
#             check = c("vif", "qq", "normality", "ncv", "homogeneity", "reqq"))


#H2: More positive implicit challenge attitude score will be a positive predictor of the behavioral PERC score.
mod1<-lm(scale(perc_score)~scale(d_sens_chall),
         data=processed_final)
summary(mod1)

report(mod1)


#H3: The Intuition-Deliberation Scale will moderate the relation between implicit criticism/ challenge attitude and mastery behaviors (PERC score).

#alpha of REI intuition

intuition<- processed_final %>% select(rei_1:rei_6)
alpha(intuition)

#CRITICISM
#d' sensitivity score
mod1<-lm(scale(perc_score)~ scale(d_sens_crit) * scale(intuition_rei),
         data=processed_final)
summary(mod1)


#CHALLENGE
#d' sensitivity score
mod1<-lm(scale(perc_score)~ scale(d_sens_chall) * scale(intuition_rei) ,
         data=processed_final)
summary(mod1)


#H4: Implicit criticism attitude and challenge scores will predict mastery behaviors separately (persistence, effort, resilience and challenge-seeking). 
##PERSISTENCE
#CRITICISM
#d' sensitivity score
mod1<-lm(scale(perc_score)~ scale(d_sens_crit),
         data=processed_final)
summary(mod1)

#CHALLENGE
#d' sensitivity score
mod1<-lm(scale(perc_score)~ scale(d_sens_chall),
         data=processed_final)
summary(mod1)


##EFFORT
#CRITICISM
#d' sensitivity score
mod1<-lm(scale(effort)~ scale(d_sens_crit),
         data=processed_final)
summary(mod1)


#CHALLENGE
#d' sensitivity score
mod1<-lm(scale(effort)~ scale(d_sens_chall),
         data=processed_final)
summary(mod1)
report(mod1)


##RESILIENCE
#CRITICISM
#d' sensitivity score
mod1<-lm(scale(resilience)~ scale(d_sens_crit),
         data=processed_final)
summary(mod1)


#CHALLENGE
#d' sensitivity score
mod1<-lm(resilience~ d_sens_chall,
         data=processed_final)
summary(mod1)
report(mod1)


ggplot(mod1, aes(x = d_sens_chall, y = resilience)) + 
  stat_smooth(method = "lm", col = "darkolivegreen4")+
  ggtitle("Resilience predicted by the implicit challenge sensitivity score") +
  xlab("implicit challenge (d')") + ylab("resilience sub-score")


##CHALLENGE-SEEKING
#CRITICISM
#d' sensitivity score
mod1<-glm(challenge_seeking~ d_sens_crit,
          family="binomial",
         data=processed_final)
summary(mod1)


#CHALLENGE
#d' sensitivity score
mod1<-glm(challenge_seeking~ d_sens_chall,
          family="binomial",
          data=processed_final)
summary(mod1)

report(mod1)


#H5

mod1<-glm(chsc~ d_sens_chall,
          family="binomial",
          data=processed_final)
summary(mod1)


#H6 Correlations between implicit and explicit measures

#checking normality of the data
#normally distributed scores
shapiro.test(processed_final$d_sens_crit)
shapiro.test(processed_final$criticism_d)
shapiro.test(processed_final$challange_d)

#not normally distributed ones
shapiro.test(processed_final$d_sens_chall)
shapiro.test(processed_final$perc_score)
shapiro.test(processed_final$persistence)
shapiro.test(processed_final$effort)
shapiro.test(processed_final$resilience)
shapiro.test(processed_final$challenge_seeking)
shapiro.test(processed_final$chms_avg)
shapiro.test(processed_final$crms_avg)
shapiro.test(processed_final$chsc)
shapiro.test(processed_final$crsc_avg)
shapiro.test(processed_final$iqms_avg)
shapiro.test(processed_final$intuition_rei)
shapiro.test(processed_final$chall_ev_participant)
shapiro.test(processed_final$chall_ev_us)
shapiro.test(processed_final$crit_ev_participant)
shapiro.test(processed_final$crit_ev_us)



#checking Spearman correlations

Hmisc::rcorr(data.matrix(processed_final[, c("d_sens_chall", "d_sens_crit",
                                             "perc_score", "effort", "resilience", "challenge_seeking", "persistence",
                                             #"chms_avg", "crms_avg", "chsc", "crsc_avg", "iqms_avg", "srgpa_1", "pre_ms_ev",
                                             #"chall_ev_participant", "chall_ev_us", "crit_ev_participant", "crit_ev_us"
                                             "mcsd", "grit_avg"
)]),
type = "spearman")


#Exploratory analyses

mod1<-lm(scale(crms_avg)~ d_sens_crit,
         data=processed_final)
summary(mod1)

