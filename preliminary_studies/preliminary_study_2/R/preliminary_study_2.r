library(tidyverse)
library(tidytext)
library(janitor)
library(widyr)
library(readxl)
library(stringi)
library(ggplot2)
library(igraph)
library(ggraph)
library(tidygraph)
library(psych)
library(Hmisc)


#setting working directory

setwd("preliminary_studies/preliminary_study_2")

#Loading in data

assoc<-read.csv('data/raw_data_preliminary_2.csv') 

#Cleaning data


assoc<- data.frame(lapply(assoc,      # Convert everything to lower case with to lower function
                          function(variables) {
                            return(tolower(variables))
                          })) %>% 
  #recoding attitude values
  mutate_at(vars(att_challenge_1:att_challenge_5, att_criticism_1:att_criticism_5), 
            ~recode(.,`pozitív` = 5,
                    `inkább pozitív` = 4,
                    `semleges` = 3,
                    `inkább negatív` = 2,
                    `negatív` = 1
            ))

#cleaning words from accents
assoc$challenge_1 <- stri_trans_general(assoc$challenge_1, "Latin-ASCII")
assoc$challenge_2 <- stri_trans_general(assoc$challenge_2, "Latin-ASCII")
assoc$challenge_3 <- stri_trans_general(assoc$challenge_3, "Latin-ASCII")
assoc$challenge_4 <- stri_trans_general(assoc$challenge_4, "Latin-ASCII")
assoc$challenge_5 <- stri_trans_general(assoc$challenge_5, "Latin-ASCII")
assoc$criticism_1 <- stri_trans_general(assoc$criticism_1, "Latin-ASCII")
assoc$criticism_2 <- stri_trans_general(assoc$criticism_2, "Latin-ASCII")
assoc$criticism_3 <- stri_trans_general(assoc$criticism_3, "Latin-ASCII")
assoc$criticism_4 <- stri_trans_general(assoc$criticism_4, "Latin-ASCII")
assoc$criticism_5 <- stri_trans_general(assoc$criticism_5, "Latin-ASCII")




#reviewed grammar mistakes and parts of speech manually, and created same meaning words to the same part of speech
#for instance verbs and nouns with the same meaning were equalized based on the more frequently occurred class of word 
#- e.g. sad and sadness would all become sadness.
#After this, we translated the most frequently occurred words

try<-assoc %>% 
  mutate(challenge_1 = case_when(str_detect(challenge_1, "^fejl") ~ "fejlodes",
                                 str_detect(challenge_1, "^epit") ~ "epito",
                                 str_detect(challenge_1, "^kuz") ~ "kuzdelem",
                                 str_detect(challenge_1, "^akada") ~ "akadaly",
                                 str_detect(challenge_1, "^cel") ~ "cel",
                                 str_detect(challenge_1, "^nehez") ~ "nehezseg",
                                 str_detect(challenge_1, "^farad") ~ "faradtsag",
                                 str_detect(challenge_1, "^lehet") ~ "lehetoseg",
                                 str_detect(challenge_1, "^siker") ~ "siker",
                                 str_detect(challenge_1, "^gondol") ~ "gondolkodas",
                                 str_detect(challenge_1, "^tanul") ~ "tanulas",
                                 str_detect(challenge_1, "^megmer") ~ "megmerettetes",
                                 str_detect(challenge_1, "^teljesit") ~ "teljesitmeny",
                                 str_detect(challenge_1, "^proba") ~ "probalkozas",
                                 TRUE ~ (challenge_1))) %>% 
  mutate(challenge_2 = case_when(str_detect(challenge_2, "^fejl") ~ "fejlodes",
                                 str_detect(challenge_2, "^epit") ~ "epites",
                                 str_detect(challenge_2, "^kuz") ~ "kuzdelem",
                                 str_detect(challenge_2, "^akada") ~ "akadaly",
                                 str_detect(challenge_2, "^cel") ~ "cel",
                                 str_detect(challenge_2, "^nehez") ~ "nehezseg",
                                 str_detect(challenge_2, "^farad") ~ "faradtsag",
                                 str_detect(challenge_2, "^lehet") ~ "lehetoseg",
                                 str_detect(challenge_2, "^siker") ~ "siker",
                                 str_detect(challenge_2, "^gondol") ~ "gondolkodas",
                                 str_detect(challenge_2, "^tanul") ~ "tanulas",
                                 str_detect(challenge_2, "^megmer") ~ "megmerettetes",
                                 str_detect(challenge_2, "^teljesit") ~ "teljesitmeny",
                                 str_detect(challenge_2, "^proba") ~ "probalkozas",
                                 TRUE ~ (challenge_2))) %>% 
  mutate(challenge_3 = case_when(str_detect(challenge_3, "^fejl") ~ "fejlodes",
                                 str_detect(challenge_3, "^epit") ~ "epites",
                                 str_detect(challenge_3, "^kuz") ~ "kuzdelem",
                                 str_detect(challenge_3, "^akada") ~ "akadaly",
                                 str_detect(challenge_3, "^cel") ~ "cel",
                                 str_detect(challenge_3, "^nehez") ~ "nehezseg",
                                 str_detect(challenge_3, "^farad") ~ "faradtsag",
                                 str_detect(challenge_3, "^lehet") ~ "lehetoseg",
                                 str_detect(challenge_3, "^siker") ~ "siker",
                                 str_detect(challenge_3, "^gondol") ~ "gondolkodas",
                                 str_detect(challenge_3, "^tanul") ~ "tanulas",
                                 str_detect(challenge_3, "^megmer") ~ "megmerettetes",
                                 str_detect(challenge_3, "kozdelem") ~ "kuzdelem",
                                 str_detect(challenge_3, "^teljesit") ~ "teljesitmeny",
                                 str_detect(challenge_3, "^proba") ~ "probalkozas",
                                 TRUE ~ (challenge_3))) %>% 
  mutate(challenge_4 = case_when(str_detect(challenge_4, "^fejl") ~ "fejlodes",
                                 str_detect(challenge_4, "^epit") ~ "epites",
                                 str_detect(challenge_4, "^kuz") ~ "kuzdelem",
                                 str_detect(challenge_4, "^akada") ~ "akadaly",
                                 str_detect(challenge_4, "^cel") ~ "cel",
                                 str_detect(challenge_4, "^nehez") ~ "nehezseg",
                                 str_detect(challenge_4, "^farad") ~ "faradtsag",
                                 str_detect(challenge_4, "^lehet") ~ "lehetoseg",
                                 str_detect(challenge_4, "^siker") ~ "siker",
                                 str_detect(challenge_4, "^gondol") ~ "gondolkodas",
                                 str_detect(challenge_4, "^tanul") ~ "tanulas",
                                 str_detect(challenge_4, "^megmer") ~ "megmerettetes",
                                 str_detect(challenge_4, "^teljesit") ~ "teljesitmeny",
                                 str_detect(challenge_4, "^proba") ~ "probalkozas",
                                 TRUE ~ (challenge_4))) %>% 
  mutate(challenge_5 = case_when(str_detect(challenge_5, "^fejl") ~ "fejlodes",
                                 str_detect(challenge_5, "^epit") ~ "epites",
                                 str_detect(challenge_5, "^kuz") ~ "kuzdelem",
                                 str_detect(challenge_5, "^akada") ~ "akadaly",
                                 str_detect(challenge_5, "^cel") ~ "cel",
                                 str_detect(challenge_5, "^nehez") ~ "nehezseg",
                                 str_detect(challenge_5, "^farad") ~ "faradtsag",
                                 str_detect(challenge_5, "^lehet") ~ "lehetoseg",
                                 str_detect(challenge_5, "^siker") ~ "siker",
                                 str_detect(challenge_5, "^gondol") ~ "gondolkodas",
                                 str_detect(challenge_5, "^tanul") ~ "tanulas",
                                 str_detect(challenge_5, "^megmer") ~ "megmerettetes",
                                 str_detect(challenge_5, "^teljesit") ~ "teljesitmeny",
                                 str_detect(challenge_5, "^proba") ~ "probalkozas",
                                 TRUE ~ (challenge_5))) %>% 
  mutate(criticism_1 = case_when(str_detect(criticism_1, "^fejl") ~ "fejlodes",
                                 str_detect(criticism_1, "^ep") ~ "epito",
                                 str_detect(criticism_1, "^faj") ~ "fajdalom",
                                 str_detect(criticism_1, "^valt") ~ "valtozas",
                                 str_detect(criticism_1, "gondol") ~ "gondolkodas",
                                 str_detect(criticism_1, "^nehez") ~ "nehezseg",
                                 str_detect(criticism_1, "^erteke") ~ "ertekeles",
                                 str_detect(criticism_1, "^sert") ~ "sertodes",
                                 str_detect(criticism_1, "^bira") ~ "biralat",
                                 str_detect(criticism_1, "^tanul") ~ "tanulas",
                                 str_detect(criticism_1, "^itel") ~ "itelkezes",
                                 str_detect(criticism_1, "^rosszind") ~ "rosszindulat",
                                 str_detect(criticism_1, "^jav") ~ "javitas",
                                 str_detect(criticism_1, "^segit") ~ "segitseg",
                                 TRUE ~ (criticism_1))) %>% 
  mutate(criticism_2 = case_when(str_detect(criticism_2, "^fejl") ~ "fejlodes",
                                 str_detect(criticism_2, "^ep") ~ "epito",
                                 str_detect(criticism_2, "^faj") ~ "fajdalom",
                                 str_detect(criticism_2, "^valt") ~ "valtozas",
                                 str_detect(criticism_2, "gondol") ~ "gondolkodas",
                                 str_detect(criticism_2, "^nehez") ~ "nehezseg",
                                 str_detect(criticism_2, "^erteke") ~ "ertekeles",
                                 str_detect(criticism_2, "^sert") ~ "sertodes",
                                 str_detect(criticism_2, "^bira") ~ "biralat",
                                 str_detect(criticism_2, "^tanul") ~ "tanulas",
                                 str_detect(criticism_2, "^itel") ~ "itelkezes",
                                 str_detect(criticism_2, "^rosszind") ~ "rosszindulat",
                                 str_detect(criticism_2, "^jav") ~ "javitas",
                                 str_detect(criticism_2, "^segit") ~ "segitseg",
                                 TRUE ~ (criticism_2))) %>% 
  mutate(criticism_3 = case_when(str_detect(criticism_3, "^fejl") ~ "fejlodes",
                                 str_detect(criticism_3, "^ep") ~ "epito",
                                 str_detect(criticism_3, "^faj") ~ "fajdalom",
                                 str_detect(criticism_3, "^valt") ~ "valtozas",
                                 str_detect(criticism_3, "gondol") ~ "gondolkodas",
                                 str_detect(criticism_3, "^nehez") ~ "nehezseg",
                                 str_detect(criticism_3, "^erteke") ~ "ertekeles",
                                 str_detect(criticism_3, "^sert") ~ "sertodes",
                                 str_detect(criticism_3, "^bira") ~ "biralat",
                                 str_detect(criticism_3, "^tanul") ~ "tanulas",
                                 str_detect(criticism_3, "^itel") ~ "itelkezes",
                                 str_detect(criticism_3, "^rosszind") ~ "rosszindulat",
                                 str_detect(criticism_3, "^jav") ~ "javitas",
                                 str_detect(criticism_3, "^segit") ~ "segitseg",
                                 TRUE ~ (criticism_3))) %>% 
  mutate(criticism_4 = case_when(str_detect(criticism_4, "^fejl") ~ "fejlodes",
                                 str_detect(criticism_4, "^ep") ~ "epito",
                                 str_detect(criticism_4, "^faj") ~ "fajdalom",
                                 str_detect(criticism_4, "^valt") ~ "valtozas",
                                 str_detect(criticism_4, "gondol") ~ "gondolkodas",
                                 str_detect(criticism_4, "^nehez") ~ "nehezseg",
                                 str_detect(criticism_4, "^erteke") ~ "ertekeles",
                                 str_detect(criticism_4, "^sert") ~ "sertodes",
                                 str_detect(criticism_4, "^bira") ~ "biralat",
                                 str_detect(criticism_4, "^tanul") ~ "tanulas",
                                 str_detect(criticism_4, "^itel") ~ "itelkezes",
                                 str_detect(criticism_4, "^rosszind") ~ "rosszindulat",
                                 str_detect(criticism_4, "^jav") ~ "javitas",
                                 str_detect(criticism_4, "^segit") ~ "segitseg",
                                 TRUE ~ (criticism_4))) %>% 
  mutate(criticism_5 = case_when(str_detect(criticism_5, "^fejl") ~ "fejlodes",
                                 str_detect(criticism_5, "^ep") ~ "epito",
                                 str_detect(criticism_5, "^faj") ~ "fajdalom",
                                 str_detect(criticism_5, "^valt") ~ "valtozas",
                                 str_detect(criticism_5, "gondol") ~ "gondolkodas",
                                 str_detect(criticism_5, "^nehez") ~ "nehezseg",
                                 str_detect(criticism_5, "^erteke") ~ "ertekeles",
                                 str_detect(criticism_5, "^sert") ~ "sertodes",
                                 str_detect(criticism_5, "^bira") ~ "biralat",
                                 str_detect(criticism_5, "^tanul") ~ "tanulas",
                                 str_detect(criticism_5, "^itel") ~ "itelkezes",
                                 str_detect(criticism_5, "^rosszind") ~ "rosszindulat",
                                 str_detect(criticism_5, "^jav") ~ "javitas",
                                 str_detect(criticism_5, "^segit") ~ "segitseg",
                                 TRUE ~ (criticism_5))) %>% 
  #translating the most frequently occurred words
  mutate_at(vars(criticism_1, criticism_2, criticism_3, criticism_4, criticism_5),
            ~recode(.,`rossz` = "bad",
                    `segitseg` = "help",
                    `negativ` = "negative",
                    `fajdalom` = "pain",
                    `hiba` = "mistake",
                    `tanulas` = "learning",
                    `javitas` = "improvement",
                    `sertodes` = "offense",
                    `ertekeles` = "evaluation",
                    `velemeny` = "opinion",
                    `biralat` = "review",
                    `epito` = "constructive",
                    `pozitiv` = "positive",
                    `gondolkodas` = "thinking",
                    `fejlodes` = "development",
                    `visszajelzes` = "feedback",
                    `rombolo` = "destructive",
                    `rosszindulat` = "malice",
                    `film`= "movie",
                    `banto` = "insulting",
                    `itelkezes` = "judgement",
                    `valtozas` = "change",
                    `hasznos` = "useful"
            )) %>% 
  mutate_at(vars(challenge_1, challenge_2, challenge_3, challenge_4, challenge_5),
            ~recode(.,`tanulas` = "learning",
                    `fejlodes` = "development",
                    `lehetoseg` = "opportunity",
                    `siker` = "success",
                    `probalkozas` = "trying",
                    `kuzdelem` = "struggle",
                    `motivacio` = "motivation",
                    `verseny` = "competition",
                    `kaland` = "adventure",
                    `feladat` = "task",
                    `akadaly` = "obstacle",
                    `teljesitmeny` = "performance",
                    `nehezseg` = "difficulty",
                    `izgalom` = "excitement",
                    `ero` = "power",
                    `munka` = "work",
                    `kudarc` = "failure",
                    `kitartas` = "perseverance",
                    `cel` = "goal",
                    `megmerettetes` = "trial",
                    `gyozelem` = "victory",
                    `megoldas` = "solution",
                    `akarat` = "willpower",
                    `harc` = "fight",
                    `eredmeny` = "result",
                    `egyetem` = "university",
                    `stressz` = "stress",
                    `elet` = "life",
                    `erofeszites` = "effort",
                    `ujdonsag` = "novelty",
                    `felelem` = "fear",
                    `ido` = "time"
            ))


                                 
#double-checking the changes manually if there are any many words overwritten that shouldn't be.                              
# new<- left_join(assoc, try, by="id") %>% 
#   select(challenge_1.x, challenge_1.y,
#          challenge_2.x, challenge_2.y,
#          challenge_3.x, challenge_3.y,
#          challenge_4.x, challenge_4.y,
#          challenge_5.x, challenge_5.y,
#          criticism_1.x, criticism_1.y,
#          criticism_2.x, criticism_2.y,
#          criticism_3.x, criticism_3.y,
#          criticism_4.x, criticism_4.y,
#          criticism_5.x, criticism_5.y)


#merging associations by individuals
associations_by_id <-
  try %>% 
  select(id, matches("^challenge_\\d+|^criticism_\\d+")) %>% 
  #creating order means
  gather(assoc_order, word, -id) %>% 
  extract(assoc_order, 
          into = c("assoc", "order"), 
          regex = "(\\w+)(\\d+)",
          convert = TRUE) %>% 
  group_by(word, assoc) %>% 
  mutate(order_mean = mean(order)) %>% 
  #cleaning names
  mutate_at(vars(assoc), 
            ~recode(.,`challenge_` = "CHALLENGE",
                    `criticism_` = "CRITICISM",
                    `failure_` = "FAILURE")) %>% 
  select(-id)

#calculating word frequency
n_words <- associations_by_id %>% 
  count(assoc, word) %>%  
  arrange(assoc, -n) %>% 
  filter(!is.na(word)) %>% 
  mutate_at(vars(assoc), 
            ~recode(.,`challenge_` = "CHALLENGE",
                    `criticism_` = "CRITICISM",
                    `failure_` = "FAILURE"))

#joining frequency and order mean
n_order<- left_join(associations_by_id, n_words, by = c("word", "assoc")) %>% 
  select(-order) %>% 
  distinct()


#summarising attitudes by words and associations
word_attitudes <-
  bind_cols(
    try %>% 
      select(challenge_1:att_criticism_5, -starts_with("att_")) %>% 
      gather(variable, word) %>% 
      select(word, variable),
    try %>% 
      select(starts_with("att_")) %>% 
      gather(variable, attitude) %>% 
      select(attitude)) %>% 
  group_by(variable, word) %>% 
  summarise(attitude_mean = mean(attitude, na.rm = TRUE)) %>% 
  mutate(assoc = case_when(str_detect(variable, "^challenge_") ~ "CHALLENGE",
                           str_detect(variable, "^failure_") ~ "FAILURE",
                           str_detect(variable, "^criticism_") ~ "CRITICISM")) %>% 
  ungroup() %>% 
  select(assoc, word, attitude_mean) %>% 
  group_by(assoc, word) %>% 
  summarise(attitude_mean = mean(attitude_mean, na.rm = TRUE))


#joining all data frames  
assoc_round2_final<-left_join(n_order, word_attitudes, by = c("word", "assoc"))

#writing out word data
write.csv(assoc_round2_final, "data/assoc_round2_final.csv")

#cleaning explicit data

explicit_coded<-try%>% 
  mutate_at(vars(failure_sc_1, failure_sc_2, criticism_sc_1, criticism_sc_2), 
            ~recode(.,`teljes mértékben így gondolnám.` = 5,
                    `nagyon valószínű, hogy így gondolnám.` = 4,
                    `talán így gondolnám.` = 3,
                    `kevésbé valószínű, hogy így gondolnám.` = 2,
                    `egyáltalán nem valószínű, hogy így gondolnám.` = 1
            )) %>% 
  mutate_at(vars(challenge_sc), 
            ~recode(.,`a nehéz feladatot, ahol valószínűleg tanulok valami újat.` = 1,
                    `a könnyű feladatot, ahol a legtöbb problémat meg tudom oldani.` = 0
            )) %>% 
  mutate_at(vars(failure_sc_1, failure_sc_2, criticism_sc_1, criticism_sc_2), 
            ~recode(.,`teljes mértékben így gondolnám.` = 5,
                    `nagyon valószínű, hogy így gondolnám.` = 4,
                    `talán így gondolnám.` = 3,
                    `kevésbé valószínű, hogy így gondolnám.` = 2,
                    `egyáltalán nem valószínű, hogy így gondolnám.` = 1
            )) %>% 
  mutate_at(vars(iq_ms_1:ego_res_11), 
            ~recode(.,`teljes mértékben egyetértek` = 6,
                    `egyetértek` = 5,
                    `inkább egyetértek` = 4,
                    `inkább nem értek egyet` = 3,
                    `nem értek egyet` = 2,
                    `egyáltalán nem értek egyet` = 1
            ))

#reverse coding values where it's necessary
explicit_recoded<- explicit_coded %>% 
  mutate_at(vars( iq_ms_1, iq_ms_2, failure_ms_3, failure_ms_4, criticism_ms_3, criticism_ms_4, 
                  challenge_ms_3, challenge_ms_4),
            ~recode(., `1` = 6,
                    `2` = 5,
                    `3` = 4,
                    `4` = 3,
                    `5` = 2,
                    `6` = 1,
            )) %>% 
  mutate_at(vars(failure_sc_1, criticism_sc_1),
            ~recode(., `1` = 5,
                    `2` = 4,
                    `2` = 4,
                    `5` = 1))



#checking scale alphas
crms<-explicit_recoded %>% select(criticism_ms_1:criticism_ms_4)
alpha(crms)
chms<-explicit_recoded %>% select(challenge_ms_1:challenge_ms_4)
alpha(chms)



#creating final data with explicit indices
explicit_final <-
  explicit_recoded %>%
  mutate(iqms_avg = rowMeans(x = select(.data = ., starts_with(match = "iq_"))),
         crms_avg = rowMeans(x = select(.data = ., starts_with(match = "criticism_ms"))),
         chms_avg = rowMeans(x = select(.data = ., starts_with(match = "challenge_ms"))),
         fms_avg = rowMeans(x = select(.data = ., starts_with(match = "failure_ms"))),
         fsc_avg = rowMeans(x = select(.data = ., starts_with(match = "failure_sc"))),
         crsc_avg = rowMeans(x = select(.data = ., starts_with(match = "criticism_sc_"))),
         chall_attitude = rowMeans(x = select(.data = ., starts_with(match = "att_chall"))),
         crit_attitude = rowMeans(x = select(.data = ., starts_with(match = "att_crit"))),
         iq_mindset = scale(iqms_avg),
         criticism_mindset = scale(crms_avg),
         challenge_mindset = scale(chms_avg),
         failure_mindset = scale(fms_avg),
         failure_scenario = scale(fsc_avg),
         criticism_scenario = scale(crsc_avg),
         challenge_attitude = scale(chall_attitude),
         criticism_attitude = scale(crit_attitude),
         challenge_scenario = challenge_sc)

#writing out cleaned data
write.csv(explicit_final, "data/explicit_final_preliminary2.csv")

#analysing data

#none of our variables are normally distributed 
shapiro.test(explicit_final$challenge_attitude)
shapiro.test(explicit_final$criticism_attitude)
shapiro.test(explicit_final$iq_mindset)
shapiro.test(explicit_final$criticism_mindset)
shapiro.test(explicit_final$challenge_mindset)
shapiro.test(explicit_final$criticism_scenario)
shapiro.test(explicit_final$self_report_gpa)


#Running Spearman correlations
Hmisc::rcorr(data.matrix(explicit_final[, c("challenge_attitude", "criticism_attitude", "iq_mindset", "challenge_mindset", "challenge_scenario", 
                                            "criticism_mindset", "criticism_scenario",  "self_report_gpa")]),
             type = "spearman")

write.csv(explicit_final, "prestudy_2_explicit.csv")

