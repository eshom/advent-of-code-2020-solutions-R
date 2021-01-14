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

part1("input.txt")
