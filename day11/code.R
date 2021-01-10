read_input <- function(x) {
        out <- readLines(x)
        do.call(rbind, strsplit(out, ""))
}

represent_num <- function(x) {
        ifelse(x == "L", 0,
        ifelse(x == "#", 1, NA))
}

represent_char <- function(x) {
        ifelse(is.na(x), ".",
        ifelse(x == 0, "L", "#"))
}

get_adjacent <- function(x) {
        require(matrixcalc)
        require(abind)
        do.call(abind, list(
                shift.right(x, fill = NA_real_),
                shift.left(x, fill = NA_real_),
                shift.up(x, fill = NA_real_),
                shift.down(x, fill = NA_real_),
                shift.right(shift.down(x, fill = NA_real_), fill = NA_real_),
                shift.left(shift.down(x, fill = NA_real_), fill = NA_real_),
                shift.right(shift.up(x, fill = NA_real_), fill = NA_real_),
                shift.left(shift.up(x, fill = NA_real_), fill = NA_real_),
                along = 3))
}

change_seat <- function(x) {
        n            <- apply(get_adjacent(x), 1:2, sum, na.rm = TRUE)
        empty_change <- x == 0 & n == 0
        occup_change <- x == 1 & n >= 4
        x[empty_change] <- 1
        x[occup_change] <- 0
        x
}

part1 <- function(filename) {
        new <- change_seat(represent_num(read_input(filename)))
        len <- dim(new)
        old <- matrix(nrow = len[1], ncol = len[2])
        while (!identical(old, new)) {
                old[1:prod(len)] <- new
                new[1:prod(len)] <- change_seat(old)
        }
        sum(new, na.rm = TRUE)
}

part1("input.txt")
