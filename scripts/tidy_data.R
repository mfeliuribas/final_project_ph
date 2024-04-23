
# Load libraries
library("tidyverse")
library("here")
library("dplyr")


# Load data
data_raw <- read.csv(here("data", "data_raw.csv"))


# Visualize data
head(data_raw)
glimpse(data_raw)
summary(data_raw)


# Tidy data
data_tidy <- data_raw %>%
  pivot_longer(
    cols = starts_with("T"),
    names_to = "question",
    values_to = "value",
    values_drop_na = TRUE) %>% 
  relocate("question", .after = level_studies) %>%
  relocate("value", .after = question) %>%
  select(age:value) %>%
  mutate(phoneme = NA) %>%
  group_by(question) %>%
  mutate(phoneme = if_else(
    question %in% c("T1_2_1", "T1_2_2", "T1_2_3", "T1_2_4", "T1_2_5", "T1_2_6", 
                    "T1_2_7", "T1_2_8", "T1_2_9", "T1_2_10", "T1_2_11", 
                    "T2_2_1", "T2_2_2", "T2_2_3", "T2_2_4", "T2_2_5", "T2_2_6", 
                    "T2_2_7", "T2_2_8", "T2_2_9", "T2_2_10", "T2_2_11",
                    "T3_2_1", "T3_2_2", "T3_2_3", "T3_2_4", "T3_2_5", "T3_2_6", 
                    "T3_2_7", "T3_2_8", "T3_2_9", "T3_2_10", "T3_2_11",
                    "T4_2_1", "T4_2_2", "T4_2_3", "T4_2_4", "T4_2_5", "T4_2_6", 
                    "T4_2_7", "T4_2_8", "T4_2_9", "T4_2_10", "T4_2_11",
                    "T5_2_1", "T5_2_2", "T5_2_3", "T5_2_4", "T5_2_5", "T5_2_6", 
                    "T5_2_7", "T5_2_8", "T5_2_9", "T5_2_10", "T5_2_11",
                    "T6_2_1", "T6_2_2", "T6_2_3", "T6_2_4", "T6_2_5", "T6_2_6", 
                    "T6_2_7", "T6_2_8", "T6_2_9", "T6_2_10", "T6_2_11",
                    "T1._1", "T1._2", "T1._3", "T1._4", "T1._5", "T1._6", 
                    "T1._7", "T1._8", "T1._9", "T1._10", "T1._11",
                    "T2_1", "T2_2", "T2_3", "T2_4", "T2_5", "T2_6", 
                    "T2_7", "T2_8", "T2_9", "T2_10", "T2_11",
                    "T3._1", "T3._2", "T3._3", "T3._4", "T3._5", "T3._6", 
                    "T3._7", "T3._8", "T3._9", "T3._10", "T3._11",
                    "T4._1", "T4._2", "T4._3", "T4._4", "T4._5", "T4._6", 
                    "T4._7", "T4._8", "T4._9", "T4._10", "T4._11",
                    "T5._1", "T5._2", "T5._3", "T5._4", "T5._5", "T5._6", 
                    "T5._7", "T5._8", "T5._9", "T5._10", "T5._11",
                    "T6_1", "T6_2", "T6_3", "T6_4", "T6_5", "T6_6", 
                    "T6_7", "T6_8", "T6_9", "T6_10", "T6_11"),
    "z", "s")) %>%
  write_csv(here("data", "data_final.csv"))


# Create subsets
data_tidy <- read.csv(here("data", "data_final.csv"))

subset_estudis <- data_tidy %>% 
  filter(endsWith(question, "_1")) %>%
  write_csv(here("data", "subsets", "subset_estudis.csv"))

subset_classe <- data_tidy %>% 
  filter(endsWith(question, "_2")) %>%
  write_csv(here("data", "subsets", "subset_classe.csv"))

subset_intelligent <- data_tidy %>% 
  filter(endsWith(question, "_3")) %>%
  write_csv(here("data", "subsets", "subset_intelligent.csv"))

subset_simpatic <- data_tidy %>% 
  filter(endsWith(question, "_4")) %>%
  write_csv(here("data", "subsets", "subset_simpatic.csv"))

subset_amable <- data_tidy %>% 
  filter(endsWith(question, "_5")) %>%
  write_csv(here("data", "subsets", "subset_amable.csv"))

subset_fiar <- data_tidy %>% 
  filter(endsWith(question, "_6")) %>%
  write_csv(here("data", "subsets", "subset_fiar.csv"))

subset_ciutat <- data_tidy %>% 
  filter(endsWith(question, "_7")) %>%
  write_csv(here("data", "subsets", "subset_ciutat.csv"))

subset_pbc <- data_tidy %>% 
  filter(endsWith(question, "_8")) %>%
  write_csv(here("data", "subsets", "subset_pbc.csv"))

subset_oc <- data_tidy %>% 
  filter(endsWith(question, "_9")) %>%
  write_csv(here("data", "subsets", "subset_oc.csv"))

subset_accent <- data_tidy %>% 
  filter(endsWith(question, "_10")) %>%
  write_csv(here("data", "subsets", "subset_accent.csv"))

subset_agradable <- data_tidy %>% 
  filter(endsWith(question, "_11")) %>%
  write_csv(here("data", "subsets", "subset_agradable.csv"))

