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

#setting working directory

setwd("preliminary_studies/preliminary_study_1")

#Loading in data

assoc<-read.csv('data/raw_data_preliminary_1.csv') 


#Cleaning data

assoc<- data.frame(lapply(assoc,      # Convert everything to lower case with to lower function
                          function(variables) {
                            return(tolower(variables))
                          })) %>% 
  #recoding attitude values
  mutate_at(vars(att_challenge_1:att_challenge_5, att_failure_1:att_failure_5, att_criticism_1:att_criticism_5), 
            ~recode(.,`pozitív` = 5,
                    `inkább pozitív` = 4,
                    `semleges` = 3,
                    `inkább negatív` = 2,
                    `negatív` = 1))

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
assoc$failure_1 <- stri_trans_general(assoc$failure_1, "Latin-ASCII")
assoc$failure_2 <- stri_trans_general(assoc$failure_2, "Latin-ASCII")
assoc$failure_3 <- stri_trans_general(assoc$failure_3, "Latin-ASCII")
assoc$failure_4 <- stri_trans_general(assoc$failure_4, "Latin-ASCII")
assoc$failure_5 <- stri_trans_general(assoc$failure_5, "Latin-ASCII")




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
  mutate(failure_1 = case_when(str_detect(failure_1, "^fejl") ~ "fejlodes",
                               str_detect(failure_1, "^faj") ~ "fajdalom",
                               str_detect(failure_1, "^valt") ~ "valtozas",
                               str_detect(failure_1, "^nehez") ~ "nehezseg",
                               str_detect(failure_1, "^tanul") ~ "tanulas",
                               str_detect(failure_1, "^itelk") ~ "itelkezes",
                               str_detect(failure_1, "^rosszind") ~ "rosszindulat",
                               str_detect(failure_1, "^segit") ~ "segitseg",
                               str_detect(failure_1, "^proba") ~ "proba",
                               str_detect(failure_1, "^szomor") ~ "szomorusag",
                               str_detect(failure_1, "^ujrakezd") ~ "ujrakezdes",
                               str_detect(failure_1, "^csalod") ~ "csalodottsag",
                               str_detect(failure_1, "szegyen") ~ "szegyen",
                               str_detect(failure_1, "^vesz") ~ "veszteseg",
                               str_detect(failure_1, "^buk") ~ "bukas",
                               TRUE ~ (failure_1))) %>% 
  mutate(failure_2 = case_when(str_detect(failure_2, "^fejl") ~ "fejlodes",
                               str_detect(failure_2, "^faj") ~ "fajdalom",
                               str_detect(failure_2, "^valt") ~ "valtozas",
                               str_detect(failure_2, "^nehez") ~ "nehezseg",
                               str_detect(failure_2, "^tanul") ~ "tanulas",
                               str_detect(failure_2, "^itelk") ~ "itelkezes",
                               str_detect(failure_2, "^rosszind") ~ "rosszindulat",
                               str_detect(failure_2, "^segit") ~ "segitseg",
                               str_detect(failure_2, "^proba") ~ "proba",
                               str_detect(failure_2, "^szomor") ~ "szomorusag",
                               str_detect(failure_2, "^ujrakezd") ~ "ujrakezdes",
                               str_detect(failure_2, "^csalod") ~ "csalodottsag",
                               str_detect(failure_2, "szegyen") ~ "szegyen",
                               str_detect(failure_2, "^vesz") ~ "veszteseg",
                               str_detect(failure_2, "^buk") ~ "bukas",
                               TRUE ~ (failure_2))) %>% 
  mutate(failure_3 = case_when(str_detect(failure_3, "^fejl") ~ "fejlodes",
                               str_detect(failure_3, "^faj") ~ "fajdalom",
                               str_detect(failure_3, "^valt") ~ "valtozas",
                               str_detect(failure_3, "^nehez") ~ "nehezseg",
                               str_detect(failure_3, "^tanul") ~ "tanulas",
                               str_detect(failure_3, "^itelk") ~ "itelkezes",
                               str_detect(failure_3, "^rosszind") ~ "rosszindulat",
                               str_detect(failure_3, "^segit") ~ "segitseg",
                               str_detect(failure_3, "^proba") ~ "proba",
                               str_detect(failure_3, "^szomor") ~ "szomorusag",
                               str_detect(failure_3, "^ujrakezd") ~ "ujrakezdes",
                               str_detect(failure_3, "^csalod") ~ "csalodottsag",
                               str_detect(failure_3, "szegyen") ~ "szegyen",
                               str_detect(failure_3, "^vesz") ~ "veszteseg",
                               str_detect(failure_3, "^buk") ~ "bukas",
                               TRUE ~ (failure_3))) %>% 
  mutate(failure_4 = case_when(str_detect(failure_4, "^fejl") ~ "fejlodes",
                               str_detect(failure_4, "^faj") ~ "fajdalom",
                               str_detect(failure_4, "^valt") ~ "valtozas",
                               str_detect(failure_4, "^nehez") ~ "nehezseg",
                               str_detect(failure_4, "^tanul") ~ "tanulas",
                               str_detect(failure_4, "^itelk") ~ "itelkezes",
                               str_detect(failure_4, "^rosszind") ~ "rosszindulat",
                               str_detect(failure_4, "^segit") ~ "segitseg",
                               str_detect(failure_4, "^proba") ~ "proba",
                               str_detect(failure_4, "^szomor") ~ "szomorusag",
                               str_detect(failure_4, "^ujrakezd") ~ "ujrakezdes",
                               str_detect(failure_4, "^csalod") ~ "csalodottsag",
                               str_detect(failure_4, "szegyen") ~ "szegyen",
                               str_detect(failure_4, "^vesz") ~ "veszteseg",
                               str_detect(failure_4, "^buk") ~ "bukas",
                               TRUE ~ (failure_4))) %>% 
  mutate(failure_5 = case_when(str_detect(failure_5, "^fejl") ~ "fejlodes",
                               str_detect(failure_5, "^faj") ~ "fajdalom",
                               str_detect(failure_5, "^valt") ~ "valtozas",
                               str_detect(failure_5, "^nehez") ~ "nehezseg",
                               str_detect(failure_5, "^tanul") ~ "tanulas",
                               str_detect(failure_5, "^itelk") ~ "itelkezes",
                               str_detect(failure_5, "^rosszind") ~ "rosszindulat",
                               str_detect(failure_5, "^segit") ~ "segitseg",
                               str_detect(failure_5, "^proba") ~ "proba",
                               str_detect(failure_5, "^szomor") ~ "szomorusag",
                               str_detect(failure_5, "^ujrakezd") ~ "ujrakezdes",
                               str_detect(failure_5, "^csalod") ~ "csalodottsag",
                               str_detect(failure_5, "szegyen") ~ "szegyen",
                               str_detect(failure_5, "^vesz") ~ "veszteseg",
                               str_detect(failure_5, "^buk") ~ "bukas",
                               TRUE ~ (failure_5))) %>% 
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
  mutate_at(vars(failure_1, failure_2, failure_3, failure_4, failure_5), 
            ~recode(.,`tanulas` = "learning",
                    `hiba` = "mistake",
                    `fajdalom` = "pain",
                    `csalodottsag` = "disappointment",
                    `vereseg` = "defeat",
                    `sikertelenseg` = "unsuccessfulness",
                    `bukas` = "failure",
                    `ujrakezdes` = "resumption",
                    `veszteseg` = "loss",
                    `szomorusag` = "sorrow",
                    `tapasztalat` = "experience",
                    `szegyen` = "shame",
                    `elet` = "life",
                    `banat` = "grief",
                    `feladas` = "give up",
                    `lehetoseg` = "opportunity",
                    `siker` = "success",
                    `probalkozas` = "trial",
                    `nehezseg` = "difficulty",
                    `felelem` = "pain",
                    `proba` = "attempt"
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
#          criticism_5.x, criticism_5.y,
#          failure_1.x, failure_1.y,
#          failure_2.x, failure_2.y,
#          failure_3.x, failure_3.y,
#          failure_4.x, failure_4.y,
#          failure_5.x, failure_5.y)


#merging associations by individuals
associations_by_id <-
  try %>% 
  select(id, matches("^challenge_\\d+|^criticism_\\d+|^failure_\\d+")) %>% 
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
assoc_round1_final<-left_join(n_order, word_attitudes, by = c("word", "assoc")) %>% 
  na.omit()

write.csv(assoc_round1_final, "data/assoc_round1_final.csv")

#creating graph
graph<-assoc_round1_final %>% 
  #filtering for words that occurred at least 10 times
  filter(n >=10) %>% 
  graph_from_data_frame(directed = FALSE) 


  graph %>% 
    ggraph(layout="kk",circular = FALSE) +
    geom_edge_link(aes(edge_alpha = attitude_mean,
                       alpha = 0.7,
                       edge_width = n),
                   edge_colour = "darkolivegreen4",) +
    scale_edge_width(range = c(1, 4)) +
    scale_size(range = c(4,6)) +
    geom_node_point(shape = 16,
                    size = 2,
                    colour ="peachpuff3") +
    geom_node_text(aes(label = name),
                   repel=TRUE,
                   point.padding = unit(0.1, "lines")) +
    theme_graph() +
    labs(title = "Study 1 - Word associations for challenge, criticism and failure",
       subtitle = "
       The wider the connection between the words, the more frequently they were associated to the category. 
       The colour of the connection shows whether the words were rated more positively (darker) or negatively (brighter).")




