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

setwd("preliminary_studies")


#Loading in preliminary study 1 data
assoc1<-read.csv('preliminary_study_1/data/raw_data_preliminary_1.csv') %>% 
  type_convert() %>% 
  filter(!is.na(challenge_1),
         !is.na(criticism_1))

assoc1<- data.frame(lapply(assoc1,      # Convert everything to lower case with to lower function
                     function(variables) {
                       return(tolower(variables))
                     })) %>% 
  #recoding attitude values
  mutate_at(vars(att_challenge_1:att_challenge_5, att_failure_1:att_failure_5, att_criticism_1:att_criticism_5), 
            ~recode(.,`pozitív` = 5,
                    `inkább pozitív` = 4,
                    `semleges` = 3,
                    `inkább negatív` = 2,
                    `negatív` = 1)) %>% 
  select(id, challenge_1:att_challenge_5, criticism_1:att_criticism_5)

#Loading in preliminary study 2 data
assoc2<-read.csv('preliminary_study_2/data/raw_data_preliminary_2.csv') %>% 
  type_convert() %>% 
  filter(!is.na(challenge_1),
         !is.na(criticism_1))

assoc2<- data.frame(lapply(assoc2,      # Convert everything to lower case with to lower function
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
            )) %>% 
  select(id, challenge_1:att_challenge_5, criticism_1:att_criticism_5)



#binding preliminary studies data together
assoc<-rbind(assoc1, assoc2)

#writing out raw collapsed data
#write.csv(assoc, "preliminary_studies_joined/data/raw_data_preliminary_studies.csv")

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
                    `failure_` = "FAILURE"))

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
assoc_joinned_final<-left_join(n_order, word_attitudes, by = c("word", "assoc"))

#writing out cleaned, joinned dataframe
#write.csv(assoc_joinned_final, "preliminary_studies_joined/data/assoc_prestudies_final.csv")

#creating co-occurrences table
pair<- associations_by_id %>% 
  group_by(assoc) %>% 
  pairwise_count(
    item = word,
    feature = id,
    sort = TRUE,
    upper = FALSE
  ) %>% 
  ungroup()

#visualizing co-occurrences
nested_pairs <-
  pair %>% 
  #filtering for observations that appeared at least 9 times in pairs.
  filter(n >=9) %>%
  group_nest(assoc) %>% 
  mutate(graph = map(data, ~.x %>%
                       as_tbl_graph()),
         plot = map(graph, 
                    ~.x %>% 
                      ggraph(layout="linear",circular = TRUE) +
                      geom_edge_link(aes(edge_width = n), 
                                     edge_colour = "olivedrab4") +
                      scale_edge_width(range = c(0.1, 3)) +
                      geom_node_point(shape = 16,
                                      size = 2,
                                      colour ="peachpuff3") +
                      geom_node_text(aes(label = name,
                                         size = 3),
                                     repel=TRUE,
                                     point.padding = unit(1, "lines")) +
                      guides(fill = FALSE, alpha = FALSE, edge_alpha = FALSE, size=FALSE) +
                      theme_graph() +
                      labs(title = "Association Co-occurrences to Criticism",
                           subtitle ="Co-occurrences of free word associations to challenge by Hungarian students. Wider lines show more frequent occurrences between the pairs.")
                      
                    )
  )
nested_pairs[[1,"plot"]] 
nested_pairs[[2,"plot"]] 

