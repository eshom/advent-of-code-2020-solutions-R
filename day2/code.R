library(magrittr)
library(collapse)
library(stringr)

input   <- readLines("input.txt")

# Part 1
pattern <- r"{(^[[:digit:]]+)-([[:digit:]]+)\s*([[:alpha:]])\s*:\s*([[:alpha:]]+)$}"
df      <- str_match(input, pattern) %>% qDF() %>%
        frename(V1 = "text", V2 = "policy_low", V3 = "policy_high",
                V4 = "policy_letter", V5 = "password") %>%
        ftransformv(c("policy_low", "policy_high"), as.integer)

df <- ftransform(df, letter_count = dapply(df, function(x) str_count(x[5], x[4]), MARGIN = 1))
df <- ftransform(df, isvalid = dapply(df, function(x) {
        as.integer(x[6]) >= as.integer(x[2]) && as.integer(x[6]) <= as.integer(x[3])
}, MARGIN = 1))

fsum(df$isvalid)

# Part 2
df <- ftransform(df, letter_pos1 = dapply(df, function(x) {
        substr(x[5], as.integer(x[2]), as.integer(x[2]))
}, MARGIN = 1),
letter_pos2 = dapply(df, function(x) {
        substr(x[5], as.integer(x[3]), as.integer(x[3]))
}, MARGIN = 1))

df <- ftransform(df, isvalid_pos = dapply(df, function(x) {
        (x[8] != x[9]) && (x[4] == x[8] || x[4] == x[9])
}, MARGIN = 1))

fsum(df$isvalid_pos)
