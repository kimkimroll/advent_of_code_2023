library(dplyr)
library(rvest)
library(stringi)

setwd("C:/Users/nvj5/OneDrive - CDC/adventofcode")

dataraw <- read_document("day1.txt")
data <- read_document("day1.txt") %>% as.data.frame()


numbers <- data %>%
  mutate(num = as.numeric(gsub(".*?([0-9]+).*", "\\1", dataraw))
  )

numbers <- data %>%
  rename(one = '.') %>%
  mutate(num2prep = stri_reverse(one)) %>%
  mutate(
    num1 = stri_extract_first_regex(one, "[0-9]+"),
    num2 = stri_extract_first_regex(num2prep, "[0-9]+")
        ) %>%
    mutate(
      num1 = as.numeric(num1), 
      num2 = as.numeric(num2)
         ) %>%
  mutate(
    num1fin = floor(num1 / (10 ^ floor(log10(num1)))),
    num2fin = floor(num2 / (10 ^ floor(log10(num2))))
  ) %>%
  mutate(total = paste(num1fin, num2fin, sep = "")) %>%
  mutate(totalsum = as.numeric(total))

answer <- sum(numbers$totalsum)

answer
