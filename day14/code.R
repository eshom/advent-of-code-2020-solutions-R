read_input <- function(filename) {
        input    <- readLines(filename)
        memline  <- grepl(r"{^mem}", input)
        memcells <- regmatches(input, regexec(r"{mem\[(\d+)\]}", input))
        memaddr  <- as.numeric(sapply(memcells, function(x) x[2]))
        memsize  <- max(na.omit(memaddr))
        mask     <- regmatches(input, regexec(r"{^mask\s=\s([X10]{36})$}",
                                              input))
        mask     <- sapply(mask, function(x) x[2])
        value    <- regmatches(input, regexec(r"{^mem\[\d+\]\s=\s(\d+)$}",
                                              input))
        value    <- as.numeric(sapply(value, function(x) x[2]))

        for (i in 2:length(mask)) {
                if (is.na(mask[i])) {
                        mask[i] <- mask[i - 1]
                }
        }

        data.frame(mask, memaddr, value, memsize)[memline, ]
}

init_memory <- function(df) {
        out <- vector("list", unique(df$memsize))
        lapply(out, function(x) logical(36L))
}

## Only works for 36 bits. Not vectorised.
as_mask <- function(str) {
        m   <- gregexpr(r"{[10]}", str)
        out <- rev(regmatches(str, m)[[1]])
        out <- as.logical(as.numeric(out))
        attr(out, "i") <- 37L - rev(m[[1]])
        out
}

apply_mask <- function(mem, mask) {
        mem[attr(mask, "i")] <- mask
        mem
}

# Extends 32 bits to 36 bits. Not vectorised.
as_binary <- function(decimal) {
        c(as.logical(intToBits(decimal)), logical(4))
}

as_decimal <- function(binary) {
        sum(2^(0:35) * binary)
}

mem_write <- function(addr, value, maskstr) {
        g_memory[[addr]] <<- apply_mask(as_binary(value), as_mask(maskstr))
}

part1 <- function(filename) {
        input <- read_input(filename)
        g_memory <- init_memory(input)
        ## Changes the enclosing environment so 'g_memory' is visible
        environment(mem_write) <- environment()
        invisible(apply(input,
                        1,
                        function(x) mem_write(as.numeric(x["memaddr"]),
                                              as.numeric(x["value"]),
                                              x["mask"])))
        sum(sapply(g_memory, as_decimal))
}

decode_mask <- function(strvec) {
        m    <- strvec
        nx   <- sum(m == "X")
        comb <- do.call(expand.grid, rep(list(0:1), nx))
        t(apply(comb, 1, function(x, m) {
                m[m == "X"] <- x
                as.logical(as.numeric(m))
        }, m = m))
}

apply_mask2 <- function(mem, maskstr) {
        mask <- as_mask(maskstr)
        mem[attr(mask, "i")] <- mem[attr(mask, "i")] | mask
        mem <- as.character(as.numeric(mem))
        mem[-attr(mask, "i")] <- "X"
        mem
}

mem_write2 <- function(addr, value) {
        g_memory[[as.character(addr)]] <<- as_binary(value)
}

init_memory2 <- function(df) {
        new.env(parent = emptyenv(), size = unique(df$memsize), hash = TRUE)
}

part2 <- function(filename) {
        input <- read_input(filename)
        g_memory <- init_memory2(input)
        ## Changes the enclosing environment so 'g_memory' is visible
        environment(mem_write2) <- environment()
        invisible(apply(input,
                        1,
                        function(x) {
                                tmp <- apply_mask2(as_binary(
                                        as.numeric(x["memaddr"])), x["mask"])
                                addrs <- apply(decode_mask(tmp), 1, as_decimal)
                                sapply(addrs, mem_write2,
                                       as.numeric(x["value"]))
                        }))
        sum(unlist(eapply(g_memory, as_decimal, USE.NAMES = FALSE)))
}

part1("input.txt")
part2("input.txt")
