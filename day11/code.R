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

what_see <- function(x) {
        out <- rep(NA, length(x))
        if (is.na(x[1])) {
                ;
        } else {
                out[1] <- x[!is.na(x)][2]
        }
        for (i in 2:length(x)) {
                if (is.na(x[i])) next
                out[i] <- x[-(1:(i - 1))][!is.na(x[-(1:(i - 1))])][2]
        }
        out
}

get_all_see <- function(x) {
        require(abind)
        do.call(abind, list(
                t(apply(x, 1, what_see)), # right
                t(apply(x, 1, function(x) rev(what_see(rev(x))))), # left
                apply(x, 2, what_see), # down
                apply(x, 2, function(x) rev(what_see(rev(x)))), # up
                diags_to_matrix(lapply(diags_topright(x), what_see),
                        diags_indice_topright(x),
                        dim(x)), # down right
                diags_to_matrix(lapply(
                        diags_topright(x),
                        function(x) rev(what_see(rev(x)))),
                        diags_indice_topright(x),
                        dim(x)), # up left
                diags_to_matrix(lapply(diags_topleft(x), what_see),
                        diags_indice_topleft(x),
                        dim(x)), # up right
                diags_to_matrix(lapply(diags_topleft(x),
                                       function(x) rev(what_see(rev(x)))),
                        diags_indice_topleft(x),
                        dim(x)), # down left
                along = 3
        ))
}

diags_to_matrix <- function(d, i, dim) {
        out <- array(dim = dim)
        mapply(function(d, i) out[i] <<- d, d, i)
        out
}

diags_topright <- function(x) {
        split(x, row(x) - col(x))
}

diags_indice_topright <- function(x) {
        d <- row(x) - col(x)
        lapply(split(d, d), function(x) which(matrix(d %in% x, ncol = ncol(d)),
                                              arr.ind = TRUE))
}

diags_topleft <- function(x) {
        split(x, row(x) + col(x))
}

diags_indice_topleft <- function(x) {
        d <- row(x) + col(x)
        lapply(split(d, d), function(x) {
                which(matrix(d %in% x, ncol = ncol(d)),
                        arr.ind = TRUE
                )
        })
}

change_seat2 <- function(x) {
        n <- apply(get_all_see(x), 1:2, sum, na.rm = TRUE)
        empty_change <- x == 0 & n == 0
        occup_change <- x == 1 & n >= 5
        x[empty_change] <- 1
        x[occup_change] <- 0
        x
}

part2 <- function(filename) {
        new <- change_seat2(represent_num(read_input(filename)))
        len <- dim(new)
        old <- matrix(nrow = len[1], ncol = len[2])
        while (!identical(old, new)) {
                old[1:prod(len)] <- new
                new[1:prod(len)] <- change_seat2(old)
        }
        sum(new, na.rm = TRUE)
}

part1("input.txt")
part2("input.txt")
