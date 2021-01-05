adapter       <- scan("input.txt")
dev           <- max(adapter) + 3
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
