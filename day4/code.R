# Part 1

library(magrittr)
input <- readLines("input.txt") %>% paste(collapse = "\n") %>%
        strsplit("\n\n") %>% unlist()

library(stringr)
df <- str_match_all(input, r"{(\w+):(\w+|#\w+)}")

check_valid <- function(passport, valid_keys = c("byr", "iyr", "eyr", "hgt",
                                                 "hcl", "ecl", "pid", "cid"),
                        optional_keys = "cid") {
        keys <- setdiff(valid_keys, optional_keys)
        all(keys %in% passport[,2])
}

sapply(df, check_valid) %>% sum

# Part 2

check_valid <- function(passport, valid_keys = c("byr", "iyr", "eyr", "hgt",
                                                 "hcl", "ecl", "pid", "cid"),
                        optional_keys = "cid") {
        keys <- setdiff(valid_keys, optional_keys)
        is_valid <- all(keys %in% passport[,2]) # All required keys are present

        # Don't waste time doing other checks,
        # if not all keys are present
        if (!is_valid) return (is_valid)

        byr_valid <- function(x) {
                str_detect(x, r"{^\d{4}$}") &&
                        as.numeric(x) >= 1920 && as.numeric(x) <= 2002
        }
        iyr_valid <- function(x) {
                str_detect(x, r"{^\d{4}$}") &&
                        as.numeric(x) >= 2010 && as.numeric(x) <= 2020
        }
        eyr_valid <- function(x) {
                str_detect(x, r"{^\d{4}$}") &&
                        as.numeric(x) >= 2020 && as.numeric(x) <= 2030
        }
        hgt_valid <- function(x) {
                valid <- str_detect(x, r"{^\d+(cm|in)$}")
                if (!valid) return (valid)
                num <- str_extract(x, r"{^\d+}") %>% as.numeric()
                if (str_extract(x, "(cm|in)") == "cm") {
                        return (valid && num >= 150 && num <= 193)
                } else {
                        return (valid && num >= 59 && num <= 76)
                }
        }
        hcl_valid <- function(x) {
                str_detect(x, r"{^#([0-9]|[a-f]){6}$}")
        }
        ecl_valid <- function(x) {
                str_detect(x, "^(amb|blu|brn|gry|grn|hzl|oth)$")
        }
        pid_valid <- function(x) {
                str_detect(x, r"{^\d{9}$}")
        }

        is_valid <- apply(passport, 1, function(x) {
                key <- x[2]
                val <- x[3]
                switch(key, byr = byr_valid(val), iyr = iyr_valid(val),
                       eyr = eyr_valid(val), hgt = hgt_valid(val),
                       hcl = hcl_valid(val), ecl = ecl_valid(val),
                       pid = pid_valid(val))
        }) %>% unlist() %>% all()

        is_valid
}

sapply(df, check_valid) %>% sum
