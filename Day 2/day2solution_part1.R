### --- Load packages -------------------------------------
library(dplyr)
library(tidyr)
library(stringr)
library(stringi)

### --- Load data -------------------------------------
data <- read_document("day2.txt") %>% as.data.frame()

### --- Calculations -------------------------------------
table <- data %>%
  separate_wider_delim(data, cols = ., delim = ":", names = c("id", "match")) %>%
  mutate(game = stri_extract_first_regex(id, "[0-9]+")) %>%
  separate_longer_delim(match, delim = ";") %>%
  group_by(game) %>%
  mutate(option = 1:n()) %>%
  separate_longer_delim(match, delim = ",") %>%
  mutate(count = stri_extract_first_regex(match, "[0-9]+")) %>%
  mutate(color = str_extract_all(match, "[[a-z]]+")) %>%
  ungroup()

table_check <- table %>%
  mutate(count = as.numeric(count)) %>%
  group_by(game, option) %>%
  mutate(check = ifelse(color == "red" & count > 12, "fail",  
                    ifelse(color == "green" & count > 13, "fail", 
                        ifelse(color == "blue" & count > 14, "fail", "pass")))) %>%
  mutate(failn = ifelse(check == "fail", "1", "0")) %>%
  mutate(failn = as.numeric(failn)) %>%
  group_by(game) %>%
  mutate(failsum = sum(failn)) %>%
  filter(failsum == 0) %>%
  mutate(game = as.numeric(game))

table_unique <- unique(table_check$game)

answer  <- sum(table_unique)
 
answer

### --- Answer: 2204 -------------------------------------