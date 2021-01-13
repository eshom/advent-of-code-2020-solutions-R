read_input <- function(filename) {
        input <- readLines(filename)
        out   <- stringr::str_match(input, "(^[NSEWLRF])(\\d+$)")[, -1]
        data.frame(action = out[, 1], value = as.integer(out[, 2]))
}

## coord - vector of length 3: east, north, face.
print.coord <- function(x) {
        face <- switch(x[3] + 1, "E", "S", "W", "N")
        cat("East: ", x[1], " North: ", x[2], " Face: ", face, "\n")
}

init_coord <- function(e = 0, n = 0, f = "E") {
        f <- switch(f, E = 0, S = 1, W = 2, N = 3)
        out <- c(e, n, f)
        class(out) <- c("coord", class(out))
        out
}

go_north <- function(coord, n) {
        coord[2] <- coord[2] + n
        coord
}

go_south <- function(coord, n) {
        coord[2] <- coord[2] - n
        coord
}

go_east <- function(coord, n) {
        coord[1] <- coord[1] + n
        coord
}

go_west <- function(coord, n) {
        coord[1] <- coord[1] - n
        coord
}

go_forward <- function(coord, n) {
        switch(coord[3] + 1,
               go_east(coord, n),
               go_south(coord, n),
               go_west(coord, n),
               go_north(coord, n))
}

turn_left <- function(coord, degrees) {
        coord[3] <- (coord[3] - (degrees / 90)) %% 4
        coord
}

turn_right <- function(coord, degrees) {
        coord[3] <- (coord[3] + (degrees / 90)) %% 4
        coord
}

do_action <- function(coord, action, value) {
        switch(action,
               N = go_north(coord, value),
               S = go_south(coord, value),
               E = go_east(coord, value),
               W = go_west(coord, value),
               L = turn_left(coord, value),
               R = turn_right(coord, value),
               F = go_forward(coord, value))
}

part1 <- function(filename) {
        df       <- read_input(filename)
        out      <- vector("list", nrow(df))
        out[[1]] <- do_action(init_coord(), df$action[1], df$value[1])
        for (i in 2:nrow(df)) {
                out[[i]] <- do_action(out[[i - 1]], df$action[i],
                                      df$value[i])
        }
        last <- out[[length(out)]]
        sum(abs(last[1]), abs(last[2]))
}

init_coord2 <- function(e = 0, n = 0) {
        out <- c(e, n)
        class(out) <- c("coord2", class(out))
        out
}

print.coord2 <- function(x) {
        cat("East: ", x[1], " North: ", x[2], "\n")
}

rotate_left <- function(coords, d) {
        mat <- matrix(c(cos(pi / 180 * d), sin(pi / 180 * d),
                        -sin(pi / 180 * d), cos(pi / 180 * d)), ncol = 2)
        coords$waypoint <- as.vector(mat %*% coords$waypoint)
        class(coords$waypoint) <- c("coord2", class(coords$waypoint))
        coords
}

rotate_right <- function(coords, d) {
        d   <- -d
        mat <- matrix(c(
                cos(pi / 180 * d), sin(pi / 180 * d),
                -sin(pi / 180 * d), cos(pi / 180 * d)), ncol = 2)
        coords$waypoint <- as.vector(mat %*% coords$waypoint)
        class(coords$waypoint) <- c("coord2", class(coords$waypoint))
        coords
}

move_coord <- function(coords, n) {
        coords$ship <- coords$ship + (coords$waypoint * n)
        coords
}

do_action2 <- function(coords, action, value) {
        out <- switch(action,
               N = go_north(coords$waypoint, value),
               S = go_south(coords$waypoint, value),
               E = go_east(coords$waypoint, value),
               W = go_west(coords$waypoint, value),
               L = rotate_left(coords, value),
               R = rotate_right(coords, value),
               F = move_coord(coords, value))
        if (!(action %in% c("F", "L", "R"))) {
                coords$waypoint <- out
                return(coords)
        }
        out
}

part2 <- function(filename) {
        df  <- read_input(filename)
        out <- vector("list", nrow(df))
        out[[1]] <- do_action2(list(ship     = init_coord2(),
                                    waypoint = init_coord2(10, 1)),
                               df$action[1], df$value[1])
        for (i in 2:nrow(df)) {
                out[[i]] <- do_action2(out[[i - 1]], df$action[i],
                        df$value[i])
        }
        last <- out[[length(out)]]$ship
        sum(abs(last[1]), abs(last[2]))
}


part1("input.txt")
part2("input.txt")
