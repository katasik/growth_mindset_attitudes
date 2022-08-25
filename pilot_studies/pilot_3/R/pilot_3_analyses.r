#read final data
processed_final<- read.csv("data/final_data/final_pilot_3.csv") %>% 
  select(-c(1))


###ANALYSES
library(tidyverse)
library(report)
library(skimr)
library(sjPlot)


#checking descriptive statistics
processed_final %>% 
  dplyr::select(d_sens_chall, d_sens_crit, criticism_d, challange_d,
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



#checking difficulty criteria of the PERC task

correct_rates_perc<- perc_final %>% 
  mutate(first_set_correct = rowMeans(x = select(.data = ., t1p1_correct, t1p2_correct, t1p3_correct, 
                                                t1p4_correct)),
         second_set_correct = rowMeans(x = select(.data = ., t1pp1_correct, t1pp2_correct, t1pp3_correct)),
         third_set_correct = rowMeans(x = select(.data = ., t1tp2_correct, t1tp3_correct, t1tp4_correct,
                                                 t1tp5_correct)),
         fourth_set_correct = rowMeans(x = select(.data = ., t1tp6_correct, t1tp7_correct, t1tp8_correct)))
  
#checking correct rates of the perc task
mean(correct_rates_perc$first_set_correct)
mean(correct_rates_perc$second_set_correct)
mean(correct_rates_perc$third_set_correct)
mean(correct_rates_perc$fourth_set_correct)




#testing first hypothesis
mod1<- lm(scale(perc_score) ~ scale(d_sens_chall), 
          data=processed_final)
summary(mod1)

mod1<- lm(scale(perc_score) ~ scale(d_sens_crit), 
          data=processed_final)
summary(mod1)

#testing second hypothesis
#PERSISTENCE
mod1<- lm(scale(persistence) ~ scale(d_sens_chall), 
          data=processed_final)
summary(mod1)

mod1<- lm(scale(persistence) ~ scale(d_sens_crit), 
          data=processed_final)
summary(mod1)

#EFFORT
mod1<- lm(scale(effort) ~ scale(d_sens_chall), 
          data=processed_final)
summary(mod1)

mod1<- lm(scale(effort) ~ scale(d_sens_crit), 
          data=processed_final)
summary(mod1)

#RESILIENCE
mod1<- lm(scale(resilience) ~ scale(d_sens_chall), 
          data=processed_final)
summary(mod1)

mod1<- lm(scale(resilience) ~ scale(d_sens_crit), 
          data=processed_final)
summary(mod1)

#CHALLENGE-SEEKING
mod1<- glm(challenge_seeking ~ scale(d_sens_chall), 
          family="binomial",
          data=processed_final)
summary(mod1)

mod1<- glm(challenge_seeking ~ scale(d_sens_crit), 
           family="binomial",
          data=processed_final)
summary(mod1)

#testing third hypotheses
mod1<- lm(perc_score ~ scale(d_sens_chall) * scale(intuition_pid), 
          data=processed_final)
summary(mod1)

mod1<- lm(perc_score ~ scale(d_sens_crit) * scale(intuition_pid), 
          data=processed_final)
summary(mod1)

#checking Spearman correlations for fourth hypothesis
#checking normality of the data
#normally distributed scores
shapiro.test(processed_final$d_sens_chall)
shapiro.test(processed_final$d_sens_crit)
shapiro.test(processed_final$criticism_d)
shapiro.test(processed_final$challange_d)

#not normally distributed ones
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
shapiro.test(processed_final$chall_ev_participant)
shapiro.test(processed_final$chall_ev_us)
shapiro.test(processed_final$crit_ev_participant)
shapiro.test(processed_final$crit_ev_us)

Hmisc::rcorr(data.matrix(processed_final[, c(
  "d_sens_chall", "d_sens_crit", "criticism_d", "challange_d",
  #"perc_score", "effort", "resilience", "challenge_seeking", "persistence",
  #"age", "gender", "semester", "semester_now", "srgpa_1", "failed", "ethnicity", "first_gen", 
  "chms_avg", "crms_avg", "chsc", "crsc_avg", "iqms_avg", 
  "chall_ev_participant", "chall_ev_us", "crit_ev_participant", "crit_ev_us",
  "mcsd", "grit_avg"
)]),
type = "spearman")

#checking challenge intention with binomial regression
mod1<- glm(chsc ~ scale(d_sens_chall), 
           family="binomial",
           data=processed_final)
summary(mod1)
report(mod1)


