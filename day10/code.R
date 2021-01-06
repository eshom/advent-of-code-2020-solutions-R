adapter       <- scan("input.txt")
adaptdiff     <- cbind(adapter - 1, adapter - 2, adapter - 3)

colnames(adaptdiff) <- c("d1", "d2", "d3")

plug <- function(adapt, jolt) {
        res        <- which(adapt == jolt, arr.ind = TRUE)
        res[, 1]   <- adapt[res[1, 1], res[1, 2]] + res[, 2]
        out        <- res[which.min(res[, 1]), ]
        names(out) <- c("newjolt", "diff")
        out
}

countdiff <- function(adapt) {
        jolt <- 0
        diffdist <- rep(0, 3)
        res <- numeric(2)
        while (TRUE) {
                res[1:2] <- tryCatch(plug(adapt, jolt),
                        error = function(cond) c(-1, -1)
                )
                if (identical(res, c(-1, -1))) {
                        return(diffdist)
                }
                jolt[1] <- res[1]
                diffdist[res[2]] <- diffdist[res[2]] + 1
        }
}

part1 <- countdiff(adaptdiff)
part1[3] <- part1[3] + 1 #include personal device
part1[1] * part1[3]

# Part 2 needs a different approach entirely
# Adapted from another solution (couldn't figure this out myself)
sorted <- sort(adapter)
jolts <- c(0, sorted, max(adapter) + 3)

library(memoise)
plug_arrangements <- function(jolts) {
        if (length(jolts) == 1) {
                return(1)
        }
        possible <- which(jolts[2:4] - jolts[1] <= 3)

        sum(sapply(possible, function(x) {
                plug_arrangements(jolts[-1:-x])
        }))
}
plug_arrangements <- memoise(plug_arrangements)
plug_arrangements(jolts)
