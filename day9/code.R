input <- scan("input.txt", 0)
PRE_N <- 25

preamble_comb <- function(x, n) {
        x <- x[1:n]
        combn(x, 2)
}

has_property <- function(x, i, n) {
        stopifnot(i > n)
        comb_sums <- colSums(preamble_comb(x[(i - n):length(x)], n))
        x[i] %in% comb_sums
}

i        <- (1+PRE_N):length(input)
unproper <- c(rep(FALSE, PRE_N), !sapply(i, function(i) has_property(input, i, PRE_N)))
part1    <- input[unproper][1]
print(part1)

get_weakness_poss <- function(x, num) {
        x <- x[x < num] # We don't need values above the number we found
        l <- list()

        appendl <- function(x) l[[length(l) + 1]] <<- x
        y       <- x
        for (i in seq_along(x)) {
                appendl(y)
                y <- y[-1]
                z <- y
                for (j in rev(seq_along(x))) {
                        appendl(z)
                        z <- z[-length(z)]
                }
        }
        return (unique(l))
}

get_weakness_num <- function(x, num) {
        res  <- get_weakness_poss(x, num)
        weak <- sapply(res, function(x) sum(x) == num)
        sum(range(unlist(res[weak])))
}

part2 <- get_weakness_num(input, part1)
print(part2)
