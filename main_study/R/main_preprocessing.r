# Load packages
require(pacman)
p_load('tidyverse', 'jsonlite')

#setting working directory
setwd("main_study")

##Implicit processing
#reading implicit data
chall_data<- read.csv("data/implicit_raw_data/challenge_implicit_main.csv") %>% 
  select(-c(1))

crit_data<- read.csv("data/implicit_raw_data/criticism_implicit_main.csv") %>% 
  select(-c(1))



#defining important blocks of implicit data
important_targets <- c("chall_pos_main_trials", "chall_neg_main_trials", "crit_pos_main_trials", "crit_neg_main_trials")



##CHALLENGE
#tidying implicit data and creating implicit variables
#calculating reaction-time-based (D) scores by computing average rt for critical blocks separately, 
#dividing by standard deviations across all critical blocks and subtracting positive blocks from negative blocks

chall_dscores <- 
  chall_data %>%  
  filter(sender %in% important_targets,
         match == "go_trial",
         correct == "TRUE") %>% 
  group_by(ResponseId) %>% 
  mutate(pers_sd = sd(duration)) %>% 
  group_by(ResponseId, sender, pers_sd) %>% 
  summarise(pers_block_avg = mean(duration)) %>% 
  mutate(d = pers_block_avg/pers_sd) %>% 
  ungroup() %>% 
  dplyr::select(-pers_block_avg, -pers_sd) %>%
  spread(sender, d) %>% 
  mutate(challange_d = chall_neg_main_trials - chall_pos_main_trials)


# signal detection theory based sdt score
sdt_chall <- 
  chall_data %>% 
  #filter only important trials and only go_trials 
  filter(sender %in%important_targets) %>%  
  group_by(ResponseId, sender) %>%
  summarise(count_true_hit = sum(correct== "TRUE" & match == "go_trial") - 0.25,
            count_false_hit = sum(correct == "FALSE" & match == "no_go_trial") + 0.25,
            count_go = sum(match == "go_trial"),
            count_nogo = sum(match == "no_go_trial")) %>% 
  mutate(prop_go_chall = count_true_hit/count_go,
         prop_nogo_chall = count_false_hit/count_nogo) %>% 
  ungroup() %>%
  filter(prop_go_chall > (mean(prop_go_chall) - 3*sd(prop_go_chall)),
         prop_nogo_chall < (mean(prop_nogo_chall) + 3*sd(prop_nogo_chall))) %>% 
  mutate(z_hit_chall = qnorm(prop_go_chall),
         z_false_alarm_chall = qnorm(prop_nogo_chall),
         d_chall = z_hit_chall - z_false_alarm_chall) %>% 
  dplyr::select(ResponseId, sender, d_chall) %>%
  spread(sender, d_chall) %>% 
  mutate(d_sens_chall = chall_pos_main_trials - chall_neg_main_trials)



#join the sdt based sdt score and the reaction time based d score
challenge_implicit_final <- 
  left_join(sdt_chall, chall_dscores, by= "ResponseId") %>% 
  #left_join(chall_rtscores, by= "ResponseId") %>% 
  dplyr::select(- starts_with(match = "chall_"))



##CRITICISM 

#calculating reaction-time-based (D) scores by computing average rt for critical blocks separately, 
#dividing by standard deviations across all critical blocks and subtracting positive blocks from negative blocks

crit_dscores <- 
  crit_data %>%  
  filter(sender %in% important_targets,
         match == "go_trial",
         correct == "TRUE") %>% 
  group_by(ResponseId) %>% 
  mutate(pers_sd = sd(duration)) %>% 
  group_by(ResponseId, sender, pers_sd) %>% 
  summarise(pers_block_avg = mean(duration)) %>% 
  mutate(d = pers_block_avg/pers_sd) %>% 
  ungroup() %>% 
  dplyr::select(-pers_block_avg, -pers_sd) %>%
  spread(sender, d) %>% 
  mutate(criticism_d = crit_neg_main_trials - crit_pos_main_trials)




# signal detection theory based d-score
sdt_crit <- 
  crit_data %>% 
  #filter only important trials and only go_trials 
  filter(sender %in%important_targets) %>% 
  group_by(ResponseId, sender) %>% 
  summarise(count_true_hit = sum(correct== "TRUE" & match == "go_trial")- 0.25,
            count_false_hit = sum(correct == "FALSE" & match == "no_go_trial") + 0.25,
            count_go = sum(match == "go_trial"),
            count_nogo = sum(match == "no_go_trial")) %>%
  mutate(prop_go_crit = count_true_hit/count_go,
         prop_nogo_crit = count_false_hit /count_nogo) %>%
  ungroup() %>%
  filter(prop_go_crit > (mean(prop_go_crit) - 3*sd(prop_go_crit)),
         prop_nogo_crit < (mean(prop_nogo_crit) + 3*sd(prop_nogo_crit))) %>% 
  mutate(z_hit_crit = qnorm(prop_go_crit),
         z_false_alarm_crit = qnorm(prop_nogo_crit),
         d_crit = z_hit_crit - z_false_alarm_crit) %>% 
  dplyr::select(ResponseId, sender, d_crit) %>%
  spread(sender, d_crit) %>% 
  mutate(d_sens_crit = crit_pos_main_trials - crit_neg_main_trials)


criticism_implicit_final <- 
  left_join(sdt_crit, crit_dscores, by= "ResponseId") %>% 
  #left_join(crit_rtscores, by= "ResponseId") %>% 
  dplyr::select(- starts_with(match = "crit_"))


#joining implicit data and removing NAs
implicit_final<- left_join(challenge_implicit_final, criticism_implicit_final, by = "ResponseId") %>% 
  filter(!is.na(d_sens_chall),
         !is.na(d_sens_crit))



###Behavioral processing

#read behavioral data
perc_only<- read_csv("data/behavioral_data/behavioral_data_main.csv") %>% 
  select(-c(1))

#cleaning and calculating PERC scores

#calculating perc score
perc<-perc_only %>% 
  #puzzle scores, missing values coded incorrect.
  mutate(t1p1_correct = if_else(t1p1 == 6, 1, 0),
         t1p1_correct = if_else(is.na(t1p1_correct), 0, t1p1_correct),
         t1p2_correct = if_else(t1p2 == 3, 1, 0),
         t1p2_correct = if_else(is.na(t1p2_correct), 0, t1p2_correct),
         t1p3_correct = if_else(t1p3 == 1, 1, 0),
         t1p3_correct = if_else(is.na(t1p3_correct), 0, t1p3_correct),
         t1p4_correct = if_else(t1p4 == 6, 1, 0),
         t1p4_correct = if_else(is.na(t1p4_correct), 0, t1p4_correct),
         t1pp1_correct = if_else(t1pp1 == 6, 1, 0),
         t1pp1_correct = if_else(is.na(t1pp1_correct), 0, t1pp1_correct),
         t1pp2_correct = if_else(t1pp2 == 5, 1, 0),
         t1pp2_correct = if_else(is.na(t1pp2_correct), 0, t1pp2_correct),
         t1pp3_correct = if_else(t1pp3 == 1, 1, 0),
         t1pp3_correct = if_else(is.na(t1pp3_correct), 0, t1pp3_correct),
         t1tp1_correct = if_else(t1tp1 == 8, 1, 0),
         t1tp1_correct = if_else(is.na(t1tp1_correct), 0, t1tp1_correct),
         t1tp2_correct = if_else(t1tp2 == 7, 1, 0),
         t1tp2_correct = if_else(is.na(t1tp2_correct), 0, t1tp2_correct),
         t1tp3_correct = if_else(t1tp3 == 3, 1, 0),
         t1tp3_correct = if_else(is.na(t1tp3_correct), 0, t1tp3_correct),
         t1tp4_correct = if_else(t1tp4 == 5, 1, 0),
         t1tp4_correct = if_else(is.na(t1tp4_correct), 0, t1tp4_correct),
         t1tp5_correct = if_else(t1tp5 == 4, 1, 0),
         t1tp5_correct = if_else(is.na(t1tp5_correct), 0, t1tp5_correct),
         t1tp6_correct = if_else(t1tp6 == 5, 1, 0),
         t1tp6_correct = if_else(is.na(t1tp6_correct), 0, t1tp6_correct),
         t1tp7_correct = if_else(t1tp7 == 5, 1, 0),
         t1tp7_correct = if_else(is.na(t1tp7_correct), 0, t1tp7_correct),
         t1tp8_correct = if_else(t1tp8 == 1, 1, 0),
         t1tp8_correct = if_else(is.na(t1tp8_correct), 0, t1tp8_correct),
         #baseline puzzle-solving ability.
         set1_scoreraw = (t1p1_correct + t1p2_correct + t1p3_correct + t1p4_correct)/4,
         perc_correct = (t1p1_correct + t1p2_correct + t1p3_correct + t1p4_correct + t1pp1_correct+
                           t1pp2_correct + t1pp3_correct +t1tp1_correct + t1tp2_correct+ t1tp3_correct+
                           t1tp4_correct + t1tp5_correct + t1tp6_correct + t1tp7_correct + t1tp8_correct)/15) %>% 
  
  rowwise() %>% 
  #effort and persistence sum
  mutate(eiottime = (sum(`t1pp1t1_Page Submit`, `t1pp1t2_Page Submit`, `t1pp2t1_Page Submit`, `t1pp2t2_Page Submit`, `t1pp3t1_Page Submit`, `t1pp3t2_Page Submit`, na.rm =  TRUE)), 
         perssec = (sum(`t1tp2time_Page Submit`, `t1tp3time_Page Submit`, `t1tp4time_Page Submit`, `t1tp5time_Page Submit`, na.rm =  TRUE))) %>% 
  ungroup() %>% 
  #effort and persistence square root transformation
  mutate(eiottimetrans = sqrt(eiottime),
         perstrans = sqrt(perssec),
         #effort and persistence winsorization
         ewd = replace(eiottimetrans, eiottimetrans > (mean(eiottimetrans))+(3*sd(eiottimetrans)), (mean(eiottimetrans))+(3*sd(eiottimetrans))),
         pers_wd = replace(perstrans, perstrans > (mean(eiottimetrans))+(3*sd(perstrans)), (mean(perstrans))+(3*sd(perstrans))),
         #effort and persistence normalization
         effort = (ewd - min(ewd))/(max(ewd)-(min(ewd))),
         persistence = (pers_wd - min(pers_wd))/(max(pers_wd)-(min(pers_wd))),
         #sum resilience, make NAs zero
         resilience = (t1tp6_correct + t1tp7_correct + t1tp8_correct)/3,
         resilience = if_else(is.na(resilience), 0, resilience),
         #no variance in resilience, so two types calculated
         perc_score = (t1difchc + effort + persistence + resilience))

#keeping only important variables
perc_final <- perc %>% 
  dplyr::select(c(1, 22, 107: 133)) %>% 
  rename(challenge_seeking = t1difchc)


###Explicit processing

#read explicit data
explicit_raw<- read.csv("data/explicit_raw_data/raw_explicit_main.csv") %>% 
  select(-c(1))


#tidying explicit data
explicit <- 
  explicit_raw %>% 
  rename(
         intdel_1 = Q197_1,
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
         intdel_18 = Q197_18
  )


##cleaning explicit data
#reversing items
explicit_coded <- 
  explicit %>% 
  mutate_at(vars(chall_gnat_feedback_1, chall_gnat_feedback_2, chall_gnat_feedback_3), 
            ~recode(.,`Igen, nagyon.` = 5,
                    `Inkább igen.` = 4,
                    `Közepesen.` = 3,
                    `Inkább nem.` = 2,
                    `Egyáltalán nem.` = 1,
            )) %>% 
  mutate_at(vars(crit_gnat_feedback_1, crit_gnat_feedback_2, crit_gnat_feedback_3), 
            ~recode(.,`Igen, nagyon.` = 5,
                    `Igen.` = 4,
                    `Közepesen.` = 3,
                    `Nem.` = 2,
                    `Egyáltalán nem.` = 1,
            )) %>% 
  mutate_at(vars(iq_1, iq_2, fms_1, fms_2, fms_3, fms_4, 
                 crms_1, crms_2, crms_3, crms_4, chms_1, chms_2, chms_3, chms_4), 
            ~recode(., `Teljes mértékben egyetértek` = 6,
                    `Egyetértek` = 5,
                    `Inkább egyetértek` = 4,
                    `Inkább nem értek egyet` = 3,
                    `Nem értek egyet` = 2,
                    `Egyáltalán nem értek egyet` = 1,
            )) %>% 
  
  mutate_at(vars(grit_1, grit_2, grit_3, grit_4, grit_5, grit_6, grit_7, grit_8), 
            ~recode(.,`Teljes mértékben jellemző rám` = 5,
                    `Nagyon jellemző rám` = 4,
                    `Valamennyire jellemző rám` = 3,
                    `Kevéssé jellemző rám` = 2,
                    `Egyáltalán nem jellemző rám` = 1,
            )) %>% 

  mutate_at(vars(intdel_1, intdel_2, intdel_3, intdel_4, intdel_5, intdel_6, intdel_7, intdel_8,
                 intdel_9, intdel_10, intdel_11, intdel_12, intdel_13, intdel_14, intdel_15, intdel_16,
                 intdel_17, intdel_18), 
            ~recode(.,`Teljesen egyetértek` = 5,
                    `Inkább egyetértek` = 4,
                    `Közepesen egyetértek` = 3,
                    `Inkább nem értek egyet` = 2,
                    `Egyáltalán nem értek egyet` = 1,
            )) %>% 
  mutate_at(vars(rei_1, rei_2, rei_3, rei_4, rei_5, rei_6, rei_7, rei_8,
                 rei_9, rei_10, rei_11, rei_12), 
            ~recode(.,`Teljes mértékben igaz` = 5,
                    `Inkább igaz` = 4,
                    `Egyaránt igaz és hamis` = 3,
                    `Inkább nem igaz` = 2,
                    `Egyáltalán nem igaz` = 1,
            )) %>% 
  mutate_at(vars(fsc_1, fsc_2, crsc_1, crsc_2), 
            ~recode(., `Egyáltalán nem valószínű, hogy így gondolnám.` = 1,
                    `Kevésbé valószínű, hogy így gondolnám.` = 2,
                    `Talán így gondolnám.` = 3,
                    `Nagyon valószínű, hogy így gondolnám.` = 4,
                    `Teljes mértékben így gondolnám.` = 5,
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
            ~recode(., `A könnyű feladatot, ahol a legtöbb problémat meg tudom oldani.` = 0,
                    `A nehéz feladatot, ahol valószínűleg tanulok valami újat.` = 1,
                    `Nem oldanám meg a plusz feladatot extra pontért.` = 0
            )) %>% 
  mutate_at(vars(crit_word_ev_1, crit_word_ev_2, crit_word_ev_3, crit_word_ev_4, crit_word_ev_5,
                 chall_word_ev_1, chall_word_ev_2, chall_word_ev_3, chall_word_ev_4, chall_word_ev_5,
                 crit_word_us_ev_1, crit_word_us_ev_2, crit_word_us_ev_3, crit_word_us_ev_4, crit_word_us_ev_5,
                 chall_word_us_ev_1, chall_word_us_ev_2, chall_word_us_ev_3, chall_word_us_ev_4, chall_word_us_ev_5, ms_words_ev_1,
                 ms_words_ev_2, ms_words_ev_3), 
            ~recode(., `negatív` = 1,
                    `inkább negatív` = 2,
                    `semleges` = 3,
                    `inkább pozitív` = 4,
                    `pozitív` = 5
            ))



#reversing items where it's necessary

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
  mutate_at(vars(grit_2, grit_4, grit_7, grit_8, intdel_18, rei_2, rei_5, rei_6, rei_7,
                 rei_8, rei_9, rei_10), 
            ~recode(., 
                    `1` = 5,
                    `2` = 4,
                    `3` = 3,
                    `4` = 2,
                    `5` = 1,
            )) 



#creating means of explicit measures of interest
explicit_final <-
  explicit_reversed %>%
  mutate(iqms_avg = rowMeans(x = dplyr::select(.data = ., starts_with(match = "iq"))),
         crms_avg = rowMeans(x = dplyr::select(.data = ., starts_with(match = "crms_"))),
         chms_avg = rowMeans(x = dplyr::select(.data = ., starts_with(match = "chms_"))),
         fms_avg = rowMeans(x = dplyr::select(.data = ., starts_with(match = "fms_"))),
         fsc_avg = rowMeans(x = dplyr::select(.data = ., starts_with(match = "fsc_"))),
         crsc_avg = rowMeans(x = dplyr::select(.data = ., starts_with(match = "crsc_"))),
         chall_ev_participant = rowMeans(x = dplyr::select(.data = ., starts_with(match = "chall_word_ev"))),
         crit_ev_participant = rowMeans(x = dplyr::select(.data = ., starts_with(match = "crit_word_ev"))),
         chall_ev_us = rowMeans(x = dplyr::select(.data = ., starts_with(match = "chall_word_us"))),
         crit_ev_us = rowMeans(x = dplyr::select(.data = ., starts_with(match = "crit_word_us"))),
         mcsd = rowMeans(x = dplyr::select(.data = ., starts_with(match = "mcsd_"))),
         grit_avg = rowMeans(x = dplyr::select(.data = ., starts_with(match = "grit_"))),
         pid_avg = rowMeans(x = dplyr::select(.data = ., starts_with(match = "intdel_"))),
         pre_ms_ev = rowMeans(x = dplyr::select(.data = ., starts_with(match = "ms_words_ev"))),
         intdel_pid = rowMeans(x = dplyr::select(.data = ., starts_with(match = "intdel_"))),
         intdel_rei = rowMeans(x = dplyr::select(.data = ., starts_with(match = "rei_"))),
         intuition_pid = rowMeans(x = dplyr::select(.data = ., "intdel_9", "intdel_11", "intdel_12",
                                                    "intdel_13", "intdel_14", "intdel_15", "intdel_16",
                                                    "intdel_17")),
         deliberation_pid = rowMeans(x = dplyr::select(.data = ., "intdel_1", "intdel_2", "intdel_3", 
                                                       "intdel_4", "intdel_5", "intdel_6")),
         intuition_rei = rowMeans(x = dplyr::select(.data = ., "rei_1", "rei_2", "rei_3",
                                                    "rei_4", "rei_5", "rei_6")),
         deliberation_rei = rowMeans(x = dplyr::select(.data = ., "rei_7", "rei_8", "rei_9", 
                                                       "rei_10", "rei_11", "rei_12")))
         
         


#join all data frames
processed_final <-
  left_join(implicit_final, explicit_final, by = "ResponseId") %>% 
  left_join(perc_final, by = "ResponseId")


#writing out final data
write.csv(processed_final, "data/final_data/final_main.csv")

