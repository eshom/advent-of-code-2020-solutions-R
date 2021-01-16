read_input <- function(filename) {
        timestamp <- scan(filename, n = 1, quiet = TRUE)
        id        <- scan(filename, character(), skip = 1, sep = ",",
                          quiet = TRUE)
        id        <- as.numeric(id[id != "x"])
        list(timestamp = timestamp, id = id)
}

arrival_matrix <- function(x, n = 100000) {
        r             <- x$time:(x$time + n - 1)
        out           <- sapply(x$id, function(x) (r %% x) == 0)
        dimnames(out) <- list(r, x$id)
        out
}

earliest_arrival <- function(x) {
        a <- which(x, arr.ind = TRUE)
        a <- a[order(a[, "row"]), ]
        c(minutes = a[1, 1] - 1, id = as.numeric(colnames(x)[a[1, 2]]))
}

part1 <- function(filename) {
        prod(earliest_arrival(arrival_matrix(read_input(filename))))
}

read_ids <- function(filename, text) {
        if (missing(text)) {
                id <- scan(filename, character(), skip = 1, sep = ",",
                           quiet = TRUE)
        } else {
                id <- scan(text = text, what = character(), sep = ",",
                           quiet = TRUE)
        }

        minute <- seq_along(id) - 1
        minute <- minute[id != "x"]
        id <- as.numeric(id[id != "x"])
        attr(id, "minute") <- minute
        id
}

find_timestamp <- function(filename, text) {
        if (!missing(text)) {
                ids <- read_ids(text = text)
        } else {
                ids <- read_ids(filename)
        }
        names(ids) <- attributes(ids)$minute
        attributes(ids)$minute <- NULL
        generate_seq <- function(from, by, n) {
                out <- numeric(n)
                for (i in seq(n) - 1) {
                        out[i + 1] <- i * by + from
                }
                out
        }
        reduce_common <- function(x, a) {
                x[(x + as.numeric(names(a))) %% a == 0]
        }
        common_interval <- function(a, b) {
                prod(a, b)
        }
        common_seq <- function(a, b, to = 10^6) {
                x <- seq(0 - as.numeric(names(a)), to, a)
                x[(x + as.numeric(names(b))) %% b == 0]
        }
        first <- common_seq(ids[1], ids[2])[1]
        interval <- common_interval(ids[1], ids[2])
        for (i in 3:length(ids)) {
                x <- generate_seq(first, interval, 10^6)
                tmp <- reduce_common(x, ids[i])
                first <- tmp[1]
                interval <- diff(tmp)[1]
        }
        first
}

part2 <- function(filename) {
        find_timestamp(filename)
}

part1("input.txt")
part2("input.txt")
