library(magrittr)
library(stringr)
library(collapse)

# Part 1

input  <- readLines("input.txt")
ticket <- str_match(input, "(^(F|B){7})((L|R){3}$)") %>% qDF() %>%
        get_vars(c("V2", "V4")) %>% frename(V2 = "row", V4 = "col")

ticket <- ftransform(ticket, row = str_split(row, ""), col = str_split(col, ""))

ticket_bin <- rapply(ticket, f = recode_char, how = "replace",
                     F = 0, B = 1, L = 0, R = 1)

seat <- rapply(ticket_bin, how = "replace", f = function(x) {
        2^(rev(seq_along(x) - 1)[as.numeric(x) %>% as.logical()]) %>% sum()
}) %>% unlist() %>%
        relist(list(row = 1:nrow(ticket_bin), col = 1:nrow(ticket_bin))) %>% qDF

seat <- ftransform(seat, id = dapply(seat, function(x) x[1] * 8 + x[2], MARGIN = 1))

fmax(seat$id)

# Part 2

seat <- roworder(seat, row, col) %>% ftransform(id_diff = fdiff(id))

fsubset(seat, id_diff != 1)
