library(magrittr)

# Part 1
linelen <- readLines("input.txt", n = 1) %>% nchar()
input   <- readLines("input.txt") %>% paste(collapse = "") %>% strsplit("") %>%
        unlist() %>% matrix(ncol = linelen, byrow = TRUE)

#istree <- input == '#'

# Every step is 3 right and 1 down
steps_before_rep <- linelen / 3
bind_times       <- ceiling(nrow(input) / steps_before_rep)

input <- replicate(bind_times, input)
dims  <- dim(input)
dim(input) <- c(dims[1], prod(dims[-1]))

istree <- input == '#'

library(collapse)
treestep <- mapply(function(i, j) ss(istree, i, j), 1:nrow(istree),
                   seq(1, ncol(istree), 3) %>% ss(1:nrow(istree)))

sum(treestep)

# Part 2

count_slope_trees <- function(mat, down = 1, right = 3) {
        steps_before_rep <- ncol(mat) / right
        bind_times       <- ceiling(nrow(mat) / steps_before_rep)

        mat      <- replicate(bind_times, mat)
        dims     <- dim(mat)
        dim(mat) <- c(dims[1], prod(dims[-1]))
        mat      <- mat == '#'

        i <- seq(1, nrow(mat), down)
        j <- seq(1, ncol(mat), right) %>% ss(seq_along(i))
        mapply(function(a, b) ss(mat, a, b), i, j) %>% sum()
}

input   <- readLines("input.txt") %>% paste(collapse = "") %>% strsplit("") %>%
        unlist() %>% matrix(ncol = linelen, byrow = TRUE)

args <- list(down = c(1, 1, 1, 1, 2),
             right = c(1, 3, 5, 7, 1),
             mat = input)

mapply(count_slope_trees, list(args$mat), args$down, args$right) %>% prod()
