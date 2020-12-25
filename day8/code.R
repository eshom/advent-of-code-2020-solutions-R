input <- readLines("input.txt")

library(stringr)
mat <- str_match(input, r"{(^.{3})\s([-+]\d{1,3})}")[,-1]
df  <- data.frame(op = mat[,1], arg = as.numeric(mat[,2]))

# The following ops always return a vector of length 2.
# [1] the accumulator value addition. [2] the next relative instruction.
acc <- function(arg) {
        c(arg, 1)
}

jmp <- function(arg) {
        c(0, arg)
}

nop <- function(arg) {
        c(0, 1)
}

# Run an instruction and return the next accumulator value and instruction num
run_instruction <- function(df, a, i) {
        dat <- df[i,]
        out <- switch(dat$op,
                      acc = acc(dat$arg),
                      jmp = jmp(dat$arg),
                      nop = nop(dat$arg))

        c(a + out[1], i + out[2])
}

# Follow instructions n times. Return the results for each step.
run_for <- function(df, n) {
        out <- matrix(0, nrow = n, ncol = 2)
        out[1,] <- run_instruction(df, 0, 1)
        for (i in 2:n) {
                if (out[i-1, 2] > nrow(df)) return (out[i-1,])
                out[i,] <- run_instruction(df, out[i-1, 1], out[i-1, 2])
        }
        colnames(out) <- c("acc", "inst")
        out
}

# Returns the results right before the infinite loop, if any.
detect_infinite_loop <- function(o) {
        df <- data.frame(acc = o[,1], inst = o[,2], id = 1:nrow(o))
        df <- df[order(df$inst),]
        df <- df[duplicated(df$inst),]
        df[which.min(df$id),] # return the first duplicate instruction
}
part1 <- detect_infinite_loop(run_for(df, 1000))
part1[,1]

corrupt_i <- which(df$op == "nop" | df$op == "jmp")

replace_and_run_for <- function(df, row_replace, n_runfor) {
        df$op[row_replace] <- switch(df$op[row_replace], nop = "jmp",
                                     jmp = "nop")
        out <- run_for(df, n_runfor)
        if (n_runfor != 1 && class(out) == "numeric") return (out[1])
        return (NULL)
}

library(parallel)
unlist(mclapply(corrupt_i, function(x) replace_and_run_for(df, x, 1000),
                mc.cores = detectCores() - 2))
