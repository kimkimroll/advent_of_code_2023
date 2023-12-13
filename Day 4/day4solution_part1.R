### --- Load packages -------------------------------------
library(dplyr)
library(textreadr)
library(tidyr)
library(stringr)

### --- Load data -------------------------------------
data <- read_document("Day 4/day4.txt") %>% as.data.frame()

### --- Calculations -------------------------------------
df_split <- data %>%
  mutate(card = str_split(., "\\: ")) %>%
  unnest_wider(where(is.list), "") %>%
  mutate(split = str_split(card2, "\\| ")) %>% 
  unnest_wider(where(is.list), "") 

check <- df_split %>% 
  mutate(winners = str_split(split1, "\\ ")) %>% 
  mutate(mine = str_split(split2, "\\ ")) 

winners <- check$winners
mines <- check$mine

total <- check %>% rowwise(card1) %>% mutate(matches = list(intersect(winners, mine)))
total$matches <- lapply(total$matches, function(x) x[nchar(x) >= 1]) 

final <- total %>% 
  mutate(times = length(matches)) %>%
  mutate(points = ifelse(times == 0, 0, 2^(times-1)))

answer <- sum(final$points)

answer

### --- Answer: 27059 -------------------------------------