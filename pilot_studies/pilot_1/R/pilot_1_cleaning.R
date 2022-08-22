library(tidyverse)
library(vroom)
library(janitor)
library(widyr)
library(readxl)

setwd("pilot_studies/pilot_1")

#defining important blocks
important_targets <- c("chall_pos", "chall_neg", "crit_pos", "crit_neg")

# read data -------------------------------------------------------------------------

#reading and cleaning implicit data

files <- list.files(path = "data/implicit_raw_data", pattern = "immtest.*.txt$", full.names = TRUE)

data<- read_excel("data/explicit_raw_data/data.xlsx")

gnat_raw <- 
  vroom(file = files, 
        id = "id", 
        col_names = c("block", "block_id", "word", "max_rt", "trial_type", "word_category", "rt", "error", "target")) %>% 
  extract(col = id, 
          into = c(NA, "id", NA), 
          regex = "^(.*data.)(.*)(.txt)$")


#keeping only important blocks
gnat_important <- 
  gnat_raw %>%  
  filter(target %in% important_targets) %>% 
  filter(trial_type == "go_trial")

#checking for block error rate above 40%
correct_block_rate <-
  gnat_important %>%  
  group_by(id, target) %>% 
  summarise(correct_block = 1 - mean(error))

#no more removal needed
correct_all_rate <-
  gnat_important %>% 
  filter(target %in% important_targets) %>%
  group_by(id) %>% 
  summarise(correct_all = 1 - mean(error))


#must delete these observations due to high block error rate
final_gnat <-
  gnat_important %>% 
  left_join(correct_block_rate, by = c("id", "target")) %>% 
  left_join(correct_all_rate, by = c("id")) %>% 
  filter(correct_block > 0.6 & correct_all > 0.7 & error == 0) %>% 
  filter(rt > 300 & rt <= 1400) 


#cleaning participant ids
explicit_raw <- 
  data %>%
  extract(col = participant, 
          into = c(NA, "id", NA), 
          regex = "^(.*s.)(.*)(.txt)$") %>% 
  clean_names()

#selecting important data
explicit_raw<-explicit_raw %>% 
  select(id, challengescenario_1:ego11_1)

##cleaning explicit data

#reversing items
explicit_reversed <- 
  explicit_raw %>% 
  mutate_at(vars(iq1_1, iq2_1, fms3_1, fms4_1, cr_ms3_1, cr_ms4_1, ch_ms3_1, ch_ms4_1), 
            ~recode(., `1` = 6,
                    `2` = 5,
                    `3` = 4,
                    `4` = 3,
                    `5` = 2,
                    `6` = 1,
            ))

explicit_reversed <- 
  explicit_reversed %>% 
  mutate_at(vars(failurescenario_1, criticismscenario_1), 
            ~recode(., `1` = 5,
                    `2` = 4,
                    `4` = 2,
                    `5` = 1,
            ))

#creating means of explicit measures
explicit_coded <-
  explicit_reversed %>%
  mutate(iqms_avg = rowMeans(x = select(.data = ., starts_with(match = "iq"))))%>%
  mutate(crms_avg = rowMeans(x = select(.data = ., starts_with(match = "cr_ms"))))%>%
  mutate(chms_avg = rowMeans(x = select(.data = ., starts_with(match = "ch_ms"))))%>%
  mutate(fms_avg = rowMeans(x = select(.data = ., starts_with(match = "fms"))))%>%
  mutate(fsc_avg = rowMeans(x = select(.data = ., starts_with(match = "failure")))) %>%
  mutate(crsc_avg = rowMeans(x = select(.data = ., starts_with(match = "criticism"))))

#final explicit data
explicit <-
  explicit_coded %>% 
  select(id, iqms_avg, crms_avg, chms_avg, fms_avg, fsc_avg, crsc_avg, challengescenario_1)

#calculating implicit mindset (D) scores by computing average rt for critical blocks separately, 
#dividing by standard deviations across all critical blocks and subtracting positive blocks from negative blocks

dscores <-
  final_gnat %>% 
  group_by(id) %>% 
  mutate(pers_sd = sd(rt)) %>% 
  group_by(id, target, pers_sd) %>% 
  summarise(pers_block_avg = mean(rt)) %>% 
  mutate(d = pers_block_avg/pers_sd) %>% 
  ungroup() %>% 
  select(-pers_block_avg, -pers_sd) %>%
  spread(target, d) %>% 
  mutate(challange_d = chall_neg - chall_pos,
         crit_d = crit_neg - crit_pos)

#collecting all data in one dataframe and subsetting ELTE sample

final_data <- 
  dscores %>% 
  left_join(explicit, by = "id") %>% 
  filter(!is.na(challange_d)) %>% 
  filter(!is.na(crit_d))

#writing out processed data
write.csv(final_data, "data/final_data/final_pilot_1.csv")

