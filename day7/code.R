input  <- readLines("input.txt")
SEARCH <- "shiny gold" # The data to search for

library(stringr)
input2df <- function(input) {
        bags   <- str_extract_all(input, r"{\w+\s+\w+(?=\s+bags?)}")
        bagnum <- str_extract_all(input, r"{\d+}")
        bagnum <- lapply(bagnum, as.integer)
        bagnum <- lapply(bagnum, function(x) replace(x, length(x) == 0, 0))
        outer  <- sapply(bags, function(x) x[1])
        inner  <- lapply(bags, function(x) x[-1])
        inner[inner == "no other"] <- NA

        data.frame(outer, inner = I(inner), innernum = I(bagnum))
}

g_df <- input2df(input) # Global data to use

get_inner <- function(bag) {
        if (is.na(bag)) return (NA)
        unlist(with(g_df, inner[outer == bag]))
}

get_outer <- function(bag) {
        if (is.na(bag)) return (NA)
        i <- which(sapply(g_df$inner, function(x) any(x == bag)))
        g_df$outer[i]
}

# Does 'outer' bag contain 'bag'?
is_in <- function(outer, bag) {
        any(get_inner(outer) == bag, na.rm = TRUE)
}

is_in_recursive <- function(outer, bag) {
        contain <- is_in(outer, bag)
        if (contain) return (TRUE)
        inbags <- get_inner(outer)
        if (length(inbags) == 1 && is.na(inbags)) return (FALSE)
        any(rapply(lapply(inbags, is_in_recursive, bag), isTRUE))
}

library(parallel)
part1 <- sum(unlist(mclapply(g_df$outer, is_in_recursive, SEARCH,
                             mc.cores = detectCores() - 2)))
print(part1) # Solution for part 1

get_innernum <- function(bag) {
        if (is.na(bag)) return (NA)
        unlist(with(g_df, innernum[outer == bag]))
}

# How many of 'bag' does 'outer' contain?
get_nbags_specific <- function(outer, bag) {
        if (is.na(outer)) return (NA)
        n <- get_innernum(outer)
        if (length(n) == 1 && n == 0) return (0)
        b   <- get_inner(outer)
        out <- n[which(b == bag)]
        if (length(out) == 0) return (0)
        out
}

get_nbags <- function(outer) {
        if (is.na(outer)) return (NA)
        n <- get_innernum(outer)
        if (length(n) == 1 && n == 0) return (0)
        out <- sum(n)
        out
}
