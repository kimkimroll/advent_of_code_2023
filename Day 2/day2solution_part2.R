### --- Load packages -------------------------------------
library(dplyr)
library(tidyr)
library(stringr)
library(stringi)

### --- Load data -------------------------------------
data <- read_document("Day 2/day2.txt") %>% as.data.frame()

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

table_max1 <- table %>%
  filter(color == "red") %>%
  mutate(count = as.numeric(count)) %>%
  group_by(game) %>%
  top_n(1, abs(count)) %>%
  distinct(game, .keep_all = TRUE) %>%
  rename(max_red = count)

table_max2 <- table %>%
  filter(color == "blue") %>%
  mutate(count = as.numeric(count)) %>%
  group_by(game) %>%
  top_n(1, abs(count)) %>%
  distinct(game, .keep_all = TRUE) %>%
  rename(max_blue = count)

table_max3 <- table %>%
  filter(color == "green") %>%
  mutate(count = as.numeric(count)) %>%
  group_by(game) %>%
  top_n(1, abs(count)) %>%
  distinct(game, .keep_all = TRUE) %>%
  rename(max_green = count)
  
final <- table_max1 %>% left_join(table_max2, by = "game") %>% left_join(table_max3, by = "game") %>%
  mutate(total = max_red*max_blue*max_green)

answer  <- sum(final$total)

answer

### --- Answer: 71036 -------------------------------------
