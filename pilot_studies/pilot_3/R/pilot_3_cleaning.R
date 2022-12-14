require(pacman)
p_load('tidyverse', 'jsonlite')

#setting working directory
setwd("pilot_studies/pilot_3")


##Implicit processing
#reading implicit data
chall_data<- read.csv("data/implicit_raw_data/challenge_implicit_pilot_3.csv") %>% 
  select(-c(1))

crit_data<- read.csv("data/implicit_raw_data/criticism_implicit_pilot_3.csv") %>% 
  select(-c(1))

#defining important blocks
important_targets <- c("chall_pos_main_trials", "chall_neg_main_trials", "crit_pos_main_trials", "crit_neg_main_trials")

# implicit data -------------------------------------------------------------------------


#tidying implicit data and creating implicit variables
#calculating reaction-time-based (D) scores by computing average rt for critical blocks separately, 
#dividing by standard deviations across all target blocks and subtracting positive blocks from negative blocks

##CHALLENGE
chall_dscores <- 
  chall_data %>%  
  #selecting only relevant trials
  filter(sender %in% important_targets,
         match == "go_trial",
         correct == "TRUE") %>% 
  group_by(ResponseId) %>% 
  #creating standard deviation across blocks
  mutate(pers_sd = sd(duration)) %>% 
  #creating reaction-time-based D scores for blocks
  group_by(ResponseId, sender, pers_sd) %>% 
  summarise(pers_block_avg = mean(duration)) %>% 
  mutate(d = pers_block_avg/pers_sd) %>% 
  ungroup() %>% 
  select(-pers_block_avg, -pers_sd) %>%
  spread(sender, d) %>% 
  #creating D score difference
  mutate(challange_d = chall_neg_main_trials - chall_pos_main_trials) %>% 
  na.omit()


# signal detection theory based sdt score
sdt_chall <- 
  chall_data %>% 
  #filter only important trials and only go_trials 
  filter(sender %in%important_targets) %>%  
  group_by(ResponseId, sender) %>%
  #calculating true/false hits, adding and subtracting 0.25 from perfect respondents
  summarise(count_true_hit = sum(correct== "TRUE" & match == "go_trial") - 0.25,
            count_false_hit = sum(correct == "FALSE" & match == "no_go_trial") + 0.25,
            count_go = sum(match == "go_trial"),
            count_nogo = sum(match == "no_go_trial")) %>%
  #creating sdt raatios
  mutate(prop_go_chall = count_true_hit/count_go,
         prop_nogo_chall = count_false_hit/count_nogo) %>% 
  ungroup() %>% 
  #removing data based on data reduction criteria
  filter(prop_go_chall > (mean(prop_go_chall) - 3*sd(prop_go_chall)),
         prop_nogo_chall < (mean(prop_nogo_chall) + 3*sd(prop_nogo_chall))) %>% 
  #z scores and sdt score
  mutate(z_hit_chall = qnorm(prop_go_chall),
         z_false_alarm_chall = qnorm(prop_nogo_chall),
         d_chall = z_hit_chall - z_false_alarm_chall) %>% 
  select(ResponseId, sender, d_chall) %>%
  spread(sender, d_chall) %>% 
  #d' sensitivity score
  mutate(d_sens_chall = chall_pos_main_trials - chall_neg_main_trials) %>% 
  na.omit()



#join the sdt based sdt score and the reaction time based d score
challenge_implicit_final <- 
  left_join(sdt_chall, chall_dscores, by= "ResponseId") %>% 
  select(- starts_with(match = "chall_"))


##CRITICISM 

#calculating reaction-time-based (D) scores by computing average rt for critical blocks separately, 
#dividing by standard deviations across all critical blocks and subtracting positive blocks from negative blocks

crit_dscores <- 
  crit_data %>%  
  #selecting only relevant trials
  filter(sender %in% important_targets,
         match == "go_trial",
         correct == "TRUE") %>% 
  group_by(ResponseId) %>% 
  #creating standard deviation across blocks
  mutate(pers_sd = sd(duration)) %>% 
  group_by(ResponseId, sender, pers_sd) %>% 
  #creating reaction-time-based D scores for blocks
  summarise(pers_block_avg = mean(duration)) %>% 
  mutate(d = pers_block_avg/pers_sd) %>% 
  ungroup() %>% 
  select(-pers_block_avg, -pers_sd) %>%
  spread(sender, d) %>% 
  #creating D score difference
  mutate(criticism_d = crit_neg_main_trials - crit_pos_main_trials) %>% 
  na.omit()


# signal detection theory based d-score
sdt_crit <- 
  crit_data %>% 
  #filter only important trials and only go_trials 
  filter(sender %in%important_targets) %>% 
  group_by(ResponseId, sender) %>% 
  #calculating true/false hits, adding and subtracting 0.25 from perfect respondents
  summarise(count_true_hit = sum(correct== "TRUE" & match == "go_trial")- 0.25,
            count_false_hit = sum(correct == "FALSE" & match == "no_go_trial") + 0.25,
            count_go = sum(match == "go_trial"),
            count_nogo = sum(match == "no_go_trial")) %>%
  #creating sdt raatios
  mutate(prop_go_crit = count_true_hit/count_go,
         prop_nogo_crit = count_false_hit /count_nogo) %>%
  ungroup() %>%
  #removing data based on data reduction criteria
  filter(prop_go_crit > (mean(prop_go_crit) - 3*sd(prop_go_crit)),
         prop_nogo_crit < (mean(prop_nogo_crit) + 3*sd(prop_nogo_crit))) %>% 
  #z scores and sdt score
  mutate(z_hit_crit = qnorm(prop_go_crit),
         z_false_alarm_crit = qnorm(prop_nogo_crit),
         d_crit = z_hit_crit - z_false_alarm_crit) %>% 
  select(ResponseId, sender, d_crit) %>%
  spread(sender, d_crit) %>% 
  #d' sensitivity score
  mutate(d_sens_crit = crit_pos_main_trials - crit_neg_main_trials) %>% 
  na.omit()


criticism_implicit_final <- 
  left_join(sdt_crit, crit_dscores, by= "ResponseId") %>% 
  select(- starts_with(match = "crit_"))


###Behavioral processing

#read behavioral data
perc_only<- read_csv("data/behavioral_data/behavioral_data_pilot_3.csv") %>% 
  select(-c(1))

#cleaning and calculating PERC scores

#calculating perc score
perc<-perc_only %>% 
  #puzzle scores, missing values coded incorrect.
  mutate(t1p1_correct = if_else(t1p1 == 1, 1, 0),
         t1p1_correct = if_else(is.na(t1p1_correct), 0, t1p1_correct),
         t1p2_correct = if_else(t1p2 == 3, 1, 0),
         t1p2_correct = if_else(is.na(t1p2_correct), 0, t1p2_correct),
         t1p3_correct = if_else(t1p3 == 6, 1, 0),
         t1p3_correct = if_else(is.na(t1p3_correct), 0, t1p3_correct),
         t1p4_correct = if_else(t1p4 == 6, 1, 0),
         t1p4_correct = if_else(is.na(t1p4_correct), 0, t1p4_correct),
         t1pp1_correct = if_else(t1pp1 == 4, 1, 0),
         t1pp1_correct = if_else(is.na(t1pp1_correct), 0, t1pp1_correct),
         t1pp2_correct = if_else(t1pp2 == 5, 1, 0),
         t1pp2_correct = if_else(is.na(t1pp2_correct), 0, t1pp2_correct),
         t1pp3_correct = if_else(t1pp3 == 1, 1, 0),
         t1pp3_correct = if_else(is.na(t1pp3_correct), 0, t1pp3_correct),
         t1tp1_correct = if_else(t1tp1 == 8, 1, 0),
         t1tp1_correct = if_else(is.na(t1tp1_correct), 0, t1tp1_correct),
         t1tp2_correct = if_else(t1tp2 == 5, 1, 0),
         t1tp2_correct = if_else(is.na(t1tp2_correct), 0, t1tp2_correct),
         t1tp3_correct = if_else(t1tp3 == 4, 1, 0),
         t1tp3_correct = if_else(is.na(t1tp3_correct), 0, t1tp3_correct),
         t1tp4_correct = if_else(t1tp4 == 8, 1, 0),
         t1tp4_correct = if_else(is.na(t1tp4_correct), 0, t1tp4_correct),
         t1tp5_correct = if_else(t1tp5 == 4, 1, 0),
         t1tp5_correct = if_else(is.na(t1tp5_correct), 0, t1tp5_correct),
         t1tp6_correct = if_else(t1tp6 == 5, 1, 0),
         t1tp6_correct = if_else(is.na(t1tp6_correct), 0, t1tp6_correct),
         t1tp7_correct = if_else(t1tp7 == 8, 1, 0),
         t1tp7_correct = if_else(is.na(t1tp7_correct), 0, t1tp7_correct),
         t1tp8_correct = if_else(t1tp8 == 3, 1, 0),
         t1tp8_correct = if_else(is.na(t1tp8_correct), 0, t1tp8_correct),
         #baseline puzzle-solving ability.
         set1_scoreraw = (t1p1_correct + t1p2_correct + t1p3_correct + t1p4_correct)) %>%
  rowwise() %>% 
  #effort and persistence sum
  mutate(eiottime = (sum( `t1pp1t1_Page Submit`, `t1pp1t2_Page Submit`, `t1pp2t1_Page Submit`,`t1pp2t2_Page Submit`, `t1pp3t1_Page Submit`, `t1pp3t2_Page Submit`, na.rm = TRUE)),  
         perssec = (sum(`t1tp2time_Page Submit`, `t1tp3time_Page Submit`, `t1tp4time_Page Submit`, `t1tp5time_Page Submit`, na.rm = TRUE))) %>% 
  ungroup() %>% 
  #effort and persistence square root transformation
  mutate(eiottimetrans = sqrt(eiottime),
         perstrans = sqrt(perssec),
         #effort and persistence winsorization
         ewd = replace(eiottimetrans, (eiottimetrans > (mean(eiottimetrans)) + (3*sd(eiottimetrans))), (mean(eiottimetrans))+(3*sd(eiottimetrans))),
         pers_wd = replace(perstrans, perstrans > (mean(perstrans))+(3*sd(perstrans)), (mean(perstrans))+(3*sd(perstrans))),
         #effort and persistence normalization
         effort = (ewd - min(ewd))/(max(ewd)-(min(ewd))),
         persistence = (pers_wd - min(pers_wd))/(max(pers_wd)-(min(pers_wd))),
         #sum resilience
         resilience = (t1tp6_correct + t1tp7_correct + t1tp8_correct)/3,
         #no variance in resilience, so two types calculated
         perc_score = (t1difchc + effort + persistence + resilience))


#keeping only important variables
perc_final <- perc %>% 
  select(c(1, 22, 107: 132)) %>% 
  rename(challenge_seeking = t1difchc)


###Explicit processing


#read explicit data
explicit_raw<- read.csv("data/explicit_raw_data/raw_explicit_pilot_3.csv") %>% 
  select(-c(1))

#adding names to the missing ones
explicit <- 
  explicit_raw %>% 
  rename(intdel_1 = Q197_1,
         intdel_2 = Q197_2,
         intdel_3 = Q197_3,
         intdel_4 = Q197_4,
         intdel_5 = Q197_5,
         intdel_6 = Q197_6,
         intdel_7 = Q197_7,
         intdel_8 = Q197_8,
         intdel_9 = Q197_9,
         intdel_10 = Q197_10,
         intdel_11 = Q197_11,
         intdel_12 = Q197_12,
         intdel_13 = Q197_13,
         intdel_14 = Q197_14,
         intdel_15 = Q197_15,
         intdel_16 = Q197_16,
         intdel_17 = Q197_17,
         intdel_18 = Q197_18,
  )

##cleaning explicit data
explicit_coded <- 
  explicit %>% 
  mutate_at(vars(chall_gnat_feedback_1, chall_gnat_feedback_2, chall_gnat_feedback_3), 
            ~recode(.,`Igen, nagyon.` = 5,
                    `Ink??bb igen.` = 4,
                    `K??zepesen.` = 3,
                    `Ink??bb nem.` = 2,
                    `Egy??ltal??n nem.` = 1,
            )) %>% 
  mutate_at(vars(crit_gnat_feedback_1, crit_gnat_feedback_2, crit_gnat_feedback_3), 
            ~recode(.,`Igen, nagyon.` = 5,
                    `Igen.` = 4,
                    `K??zepesen.` = 3,
                    `Nem.` = 2,
                    `Egy??ltal??n nem.` = 1,
            )) %>% 
  mutate_at(vars(iq_1, iq_2, fms_1, fms_2, fms_3, fms_4, 
                 crms_1, crms_2, crms_3, crms_4, chms_1, chms_2, chms_3, chms_4), 
            ~recode(., `Teljes m??rt??kben egyet??rtek` = 6,
                    `Egyet??rtek` = 5,
                    `Ink??bb egyet??rtek` = 4,
                    `Ink??bb nem ??rtek egyet` = 3,
                    `Nem ??rtek egyet` = 2,
                    `Egy??ltal??n nem ??rtek egyet` = 1,
            )) %>% 
  
  mutate_at(vars(grit_1, grit_2, grit_3, grit_4, grit_5, grit_6, grit_7, grit_8), 
            ~recode(.,`Teljes m??rt??kben jellemz?? r??m` = 5,
                    `Nagyon jellemz?? r??m` = 4,
                    `Valamennyire jellemz?? r??m` = 3,
                    `Kev??ss?? jellemz?? r??m` = 2,
                    `Egy??ltal??n nem jellemz?? r??m` = 1,
            )) %>% 
  mutate_at(vars(intdel_1, intdel_2, intdel_3, intdel_4, intdel_5, intdel_6, intdel_7, intdel_8,
                 intdel_9, intdel_10, intdel_11, intdel_12, intdel_13, intdel_14, intdel_15, intdel_16,
                 intdel_17, intdel_18), 
            ~recode(.,`Teljesen egyet??rtek` = 5,
                    `Egyet??rtek` = 4,
                    `Egyet is ??rtek, meg nem is.` = 3,
                    `Nem ??rtek egyet` = 2,
                    `Egy??ltal??n nem ??rtek egyet` = 1,
            )) %>% 
  mutate_at(vars(fsc_1, fsc_2, crsc_1, crsc_2), 
            ~recode(., `Egy??ltal??n nem val??sz??n??, hogy ??gy gondoln??m.` = 1,
                    `Kev??sb?? val??sz??n??, hogy ??gy gondoln??m.` = 2,
                    `Tal??n ??gy gondoln??m.` = 3,
                    `Nagyon val??sz??n??, hogy ??gy gondoln??m.` = 4,
                    `Teljes m??rt??kben ??gy gondoln??m.` = 5,
            )) %>% 
  mutate_at(vars(mcsd_1, mcsd_2, mcsd_3, mcsd_5, mcsd_6,
                 mcsd_8, mcsd_11, mcsd_12), 
            ~recode(., `Igaz` = 0,
                    `Hamis` = 1
            )) %>% 
  mutate_at(vars(mcsd_4, mcsd_7, mcsd_9, mcsd_10, mcsd_13), 
            ~recode(., `Igaz` = 1,
                    `Hamis` = 0
            )) %>% 
  mutate_at(vars(chsc), 
            ~recode(., `A k??nny?? feladatot, ahol a legt??bb probl??mat meg tudom oldani.` = 0,
                    `A neh??z feladatot, ahol val??sz??n??leg tanulok valami ??jat.` = 1,
                    `Nem oldan??m meg a plusz feladatot extra pont??rt.` = 0
            )) %>% 
  mutate_at(vars(crit_word_ev_1, crit_word_ev_2, crit_word_ev_3, crit_word_ev_4, crit_word_ev_5,
                 chall_word_ev_1, chall_word_ev_2, chall_word_ev_3, chall_word_ev_4, chall_word_ev_5,
                 crit_word_us_ev_1, crit_word_us_ev_2, crit_word_us_ev_3, crit_word_us_ev_4, crit_word_us_ev_5,
                 chall_word_us_ev_1, chall_word_us_ev_2, chall_word_us_ev_3, chall_word_us_ev_4, chall_word_us_ev_5, ms_words_ev_1,
                 ms_words_ev_2, ms_words_ev_3), 
            ~recode(., `negat??v` = 1,
                    `ink??bb negat??v` = 2,
                    `semleges` = 3,
                    `ink??bb pozit??v` = 4,
                    `pozit??v` = 5
            ))


#reversing scores where it's necessary
explicit_reversed <- 
  explicit_coded %>% 
  mutate_at(vars(iq_1, iq_2, fms_3, fms_4, crms_3, crms_4, chms_3, chms_4), 
            ~recode(., `1` = 6,
                    `2` = 5,
                    `3` = 4,
                    `4` = 3,
                    `5` = 2,
                    `6` = 1,
            )) %>% 
  mutate_at(vars(fsc_1, crsc_1), 
            ~recode(., `1` = 5,
                    `2` = 4,
                    `2` = 4,
                    `5` = 1,
            )) %>% 
  mutate_at(vars(grit_2, grit_4, grit_7, grit_8, intdel_18), 
            ~recode(., 
                    `1` = 5,
                    `2` = 4,
                    `3` = 3,
                    `4` = 2,
                    `5` = 1,
            ))



#creating means of explicit measures
explicit_final <-
  explicit_reversed %>%
  mutate(iqms_avg = rowMeans(x = select(.data = ., starts_with(match = "iq"))),
         crms_avg = rowMeans(x = select(.data = ., starts_with(match = "crms_"))),
         chms_avg = rowMeans(x = select(.data = ., starts_with(match = "chms_"))),
         fms_avg = rowMeans(x = select(.data = ., starts_with(match = "fms_"))),
         fsc_avg = rowMeans(x = select(.data = ., starts_with(match = "fsc_"))),
         crsc_avg = rowMeans(x = select(.data = ., starts_with(match = "crsc_"))),
         chall_ev_participant = rowMeans(x = select(.data = ., starts_with(match = "chall_word_ev"))),
         crit_ev_participant = rowMeans(x = select(.data = ., starts_with(match = "crit_word_ev"))),
         chall_ev_us = rowMeans(x = select(.data = ., starts_with(match = "chall_word_us"))),
         crit_ev_us = rowMeans(x = select(.data = ., starts_with(match = "crit_word_us"))),
         mcsd = rowMeans(x = select(.data = ., starts_with(match = "mcsd_"))),
         grit_avg = rowMeans(x = select(.data = ., starts_with(match = "grit_"))),
         pre_ms_ev = rowMeans(x = select(.data = ., starts_with(match = "ms_words_ev"))),
         intuition_pid = rowMeans(x = select(.data = ., "intdel_9", "intdel_11", "intdel_12",
                                             "intdel_13", "intdel_14", "intdel_15", "intdel_16",
                                             "intdel_17", "intdel_18")),
         deliberation_pid = rowMeans(x = select(.data = ., "intdel_1", "intdel_2", "intdel_3", 
                                                "intdel_4", "intdel_5", "intdel_6", "intdel_7",
                                                "intdel_8", "intdel_10"))) %>% 
  filter(!is.na(chms_avg))



#join all data frames
processed_final <-
  left_join(criticism_implicit_final, challenge_implicit_final, by = "ResponseId") %>% 
  left_join(perc_final, by = "ResponseId") %>% 
  left_join(explicit_final, by = "ResponseId") %>% 
  #removing observations where implicit data is not present
  filter(!is.na(d_sens_chall),
         !is.na(d_sens_crit))


#writing out final data
write.csv(processed_final, "data/final_data/final_pilot_3.csv")
