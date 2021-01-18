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

part1("input.txt")
