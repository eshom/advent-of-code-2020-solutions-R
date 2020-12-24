# Part 1
input              <- readLines("input.txt")
input[input == ""] <- ','
input              <- paste(input, collapse = "")

library(magrittr)
library(stringr)
yes <- scan(text = input, what = character(), sep = ',') %>% str_split("")
lapply(yes, unique) %>% sapply(length) %>% sum()

# Part 2
input  <- readLines("input.txt") %>% str_split("")
empty  <- sapply(input, identical, character(0))
groups <- data.table::rleid(empty)

library(collapse)
input_splt <- rsplit(input, groups)
lapply(input_splt, function(x) Reduce(intersect, x)) %>% sapply(length) %>%
        sum()
