

# bottom is the y position of the lowest element in each column,
#  relative to the first element, left to right
# height is the height of the figure
figures <- list(
  # ####
  list(
    bottom = c(0, 0, 0, 0),
    height = c(1, 1, 1, 1),
    pos = matrix(c(1,1, 2,1, 3,1, 4,1), ncol = 2, byrow = TRUE, dimnames = list(NULL, c("x", "y"))),
    pos2 = c(11, 12, 13, 14)
  ),
  # .#.
  # ###
  # .#.
  list(
    bottom = c(0, -1, 0),
    height = c(2, 3, 2),
    pos = matrix(c(1,2, 2,1, 2,2, 2,3, 3,2), ncol = 2, byrow = TRUE, dimnames = list(NULL, c("x", "y"))),
    pos2 = c(21, 12, 22, 32, 23)
  ),
  # ..#
  # ..#
  # ###
  list(
    bottom = c(0, 0, 0),
    height = c(1, 1, 3),
    pos = matrix(c(1,1, 2,1, 3,1, 3,2, 3,3), ncol = 2, byrow = TRUE, dimnames = list(NULL, c("x", "y"))),
    pos2 = c(11, 12, 13, 23, 33)
  ),
  # #
  # #
  # #
  # #
  list(
    bottom = 0,
    height = 4,
    pos = matrix(c(1,1, 1,2, 1,3, 1,4), ncol = 2, byrow = TRUE, dimnames = list(NULL, c("x", "y"))),
    pos2 = c(11, 21, 31, 41)
  ),
  # ##
  # ##
  list(
    bottom = c(0, 0),
    height = c(2, 2),
    pos = matrix(c(1,1, 1,2, 2,1, 2,2), ncol = 2, byrow = TRUE, dimnames = list(NULL, c("x", "y"))),
    pos2 = c(11, 21, 12, 22)
  )
)


move_rock <- function(rock, direction, all_rocks = NULL, full_check = FALSE) {
  moved_rock <- rock + direction
  if (any(moved_rock %% 10 < 1) || any(moved_rock %% 10 > 7) || any(moved_rock < 10)) return()

  if (full_check) {
    if (any(moved_rock %in% all_rocks)) return()
  }
  moved_rock
}


play <- function(wind, figures, N = 2022, cutoff = 500, plot = FALSE, verbose = FALSE) {
  wind_counter <- 1
  wl <- length(wind)
  wind <- c(-1, 1)[match(wind, c("<", ">"))]
  rock_counter <- 1
  rl <- length(figures)

  # save rock positions as coordinates
  # keep final position of every rock as a list
  rocks <- list()
  rocks_ind <- integer()
  all_rocks_ind <- integer()

  tops <- list()

  # keep vector of top most element in each column
  top <- rep(0, 7)

  for (i in 1:N) {
    if (verbose) message(i)
    # initialize rock
    rock <- figures[[rock_counter]]
    # first coordinate is column, second row
    left_corner_pos <- (max(top) + 3) * 10 + 2
    rock_pos <- rock$pos2 + left_corner_pos
    if (plot) {Sys.sleep(0.5); print_rocks(rocks, rock_pos)}


    # first four moves left-right and three moves down are from definition
    # over the existing tower, so are sure not to
    # collide with any other rock, we don't need to check it

    for (j in 1:3) {
      # wind push
      r <- move_rock(rock_pos, wind[wind_counter])
      if (!is.null(r)) rock_pos <- r
      wind_counter <- (wind_counter %% wl) + 1

      # fall down
      rock_pos <- move_rock(rock_pos, -10)
      if (plot) {Sys.sleep(0.5); print_rocks(rocks, rock_pos)}
    }

    # one more wind push
    r <- move_rock(rock_pos, wind[wind_counter])
    if (!is.null(r)) rock_pos <- r
    wind_counter <- (wind_counter %% wl) + 1


    while (TRUE) {
      # fall down
      # if cannot fall down - final position, save it, update top and proceed with next rock
      # check the direct hit at the top first
      top_ind <- 10 * top + 1:7
      if (any((rock_pos - 10) %in% top_ind)) {
        r <- NULL
      } else {
        r <- move_rock(rock_pos, -10, rocks_ind, TRUE)
      }

      if (!is.null(r)) {
        rock_pos <- r
        if (plot) {Sys.sleep(0.5); print_rocks(rocks, rock_pos)}
      } else {
        rocks[[i]] <- rock_pos
        rocks_ind <- c(rocks_ind, rock_pos)

        rock_pos_x <- rock_pos %% 10
        rock_pos_y <- rock_pos %/% 10
        ind <- min(rock_pos_x):max(rock_pos_x)
        # watch out NOT to replace with local maximum!!!
        top[ind] <- pmax(top[ind], min(rock_pos_y) + rock$height - 1)
        if (plot) {Sys.sleep(0.5); print_rocks(rocks, rock_pos)}
        break
      }

      # wind push
      r <- move_rock(rock_pos, wind[wind_counter], rocks_ind, TRUE)
      if (!is.null(r)) rock_pos <- r
      wind_counter <- (wind_counter %% wl) + 1

    }
    rock_counter <- (rock_counter %% rl) + 1
    tops[[i]] <- max(top)

    if (length(rocks_ind) > 2 * cutoff) {
      # left cutoff last elements, remove the rest
      all_rocks_ind <- c(all_rocks_ind, head(rocks_ind, -cutoff))
      rocks_ind <- tail(rocks_ind, cutoff)
    }

  }

  list(rocks = rocks, top = top, tops = unlist(tops), rocks_ind = all_rocks_ind)
}


# number of rocks
sum(rep(sapply(figures, function(x) nrow(x$pos)), length.out = 2022))


wind_test <- strsplit(readLines("input17_test.txt"), "")[[1]]
out_test <- play(wind_test, figures)
max(out_test$top)
max(do.call(rbind, out_test$rocks)[,2])
length(out_test$rocks_ind)

# compare with test data
test_tops <- as.integer(readLines("input17_test_heights.txt"))
head(test_tops, 10)
head(out_test$tops, 10)
table(test_tops == out_test$tops)


# some other test input
wind_test2 <- strsplit(readLines("input17_test2.txt"), "")[[1]]
out_test2 <- play(wind_test2, figures)
max(out_test2$top)
max(do.call(rbind, out_test2$rocks)[,2])
length(out_test2$rocks_ind) - 7

wind <- strsplit(readLines("input17.txt"), "")[[1]]
out <- play(wind, figures)
max(out$top)
length(out$rocks_ind)
out$rocks[2022]
max(do.call(rbind, out$rocks)[,2])
nrow(do.call(rbind, out$rocks))
# 3202





play2 <- function(wind, figures, N = 2022, cutoff = 500, plot = FALSE, verbose = FALSE) {
  wind_counter <- 1
  wl <- length(wind)
  wind <- c(-1, 1)[match(wind, c("<", ">"))]
  rock_counter <- 1
  rl <- length(figures)

  # save rock positions as coordinates
  # keep final position of every rock as a list
  rocks <- list()
  rocks_ind <- integer()
  all_rocks_ind <- integer()

  tops <- list()

  # keep vector of top most element in each column
  top <- rep(0, 7)

  for (i in 1:N) {
    if (verbose) message(i)
    # initialize rock
    rock <- figures[[rock_counter]]
    # first coordinate is column, second row
    left_corner_pos <- (max(top) + 3) * 10 + 2
    rock_pos <- rock$pos2 + left_corner_pos
    if (plot) {Sys.sleep(0.5); print_rocks(rocks, rock_pos)}


    # first four moves left-right and three moves down are from definition
    # over the existing tower, so are sure not to
    # collide with any other rock, we don't need to check it

    for (j in 1:3) {
      # wind push
      r <- move_rock(rock_pos, wind[wind_counter])
      if (!is.null(r)) rock_pos <- r
      wind_counter <- (wind_counter %% wl) + 1

      # fall down
      rock_pos <- move_rock(rock_pos, -10)
      if (plot) {Sys.sleep(0.5); print_rocks(rocks, rock_pos)}
    }

    # one more wind push
    r <- move_rock(rock_pos, wind[wind_counter])
    if (!is.null(r)) rock_pos <- r
    wind_counter <- (wind_counter %% wl) + 1


    while (TRUE) {
      # fall down
      # if cannot fall down - final position, save it, update top and proceed with next rock
      # check the direct hit at the top first
      top_ind <- 10 * top + 1:7
      if (any((rock_pos - 10) %in% top_ind)) {
        r <- NULL
      } else {
        r <- move_rock(rock_pos, -10, rocks_ind, TRUE)
      }

      if (!is.null(r)) {
        rock_pos <- r
        if (plot) {Sys.sleep(0.5); print_rocks(rocks, rock_pos)}
      } else {
        rocks[[i]] <- rock_pos
        rocks_ind <- c(rocks_ind, rock_pos)

        rock_pos_x <- rock_pos %% 10
        rock_pos_y <- rock_pos %/% 10
        ind <- min(rock_pos_x):max(rock_pos_x)
        # watch out NOT to replace with local maximum!!!
        top[ind] <- pmax(top[ind], min(rock_pos_y) + rock$height - 1)
        if (plot) {Sys.sleep(0.5); print_rocks(rocks, rock_pos)}
        break
      }

      # wind push
      r <- move_rock(rock_pos, wind[wind_counter], rocks_ind, TRUE)
      if (!is.null(r)) rock_pos <- r
      wind_counter <- (wind_counter %% wl) + 1

    }
    rock_counter <- (rock_counter %% rl) + 1
    tops[[i]] <- max(top)

    if (length(rocks_ind) > 2 * cutoff) {
      # left cutoff last elements, remove the rest
      all_rocks_ind <- c(all_rocks_ind, head(rocks_ind, -cutoff))
      rocks_ind <- tail(rocks_ind, cutoff)
    }

  }

  list(rocks = rocks, top = top, tops = unlist(tops), rocks_ind = all_rocks_ind)
}



max(play2(wind_test, figures)$top)
max(play2(wind_test2, figures)$top)
max(play2(wind, figures)$top)
max(play2(wind, figures, N = 10000)$top)
max(play(wind, figures, N = 10000)$top)

microbenchmark::microbenchmark(
  play(wind, figures),
  play2(wind, figures),
  times = 10
)

microbenchmark::microbenchmark(
  play(wind, figures, N = 5000),
  play2(wind, figures, N = 5000),
  times = 5
)


microbenchmark::microbenchmark(
  play(wind, figures, N = 10000),
  play2(wind, figures, N = 10000),
  times = 5
)

# optimization - first three moves without full check - twice better!
# checking direct hit first - on 2022 slightly worse, but on 5k slightly better ~5%
#   and on 10k even better 15%, and probably would be increasing
# replace figures[[rock_counter]] at the initialization - minimal improvement
# coerce wind vector to -1/1 integer vector - minimal improvement
# keeping coordinates as integers, 10 * y + x - better!
# initialize all rocks vector, length could be n_rocks * 5, keep position counter!
#   worse for 2k, 5k and 10k - skipping
# we could keep coordinates as vectors right from the start - better!
# top as well - seems not worth, didn't check
# merge first steps into one? but they are quite quick, skipping for now
# try to cut off some indices from being checked, like after every hundred of rows
#   or so (risky that we truncate too much)
#   or after full row is met (sure option, but rather scarce probably)
#   however it is quite complicated to implement, as I would need to control every row
#   this should show larger effects for larger N
#   MUCH better, when cutting every 500 indices, and seems to work


# option - cut off indices every full row




# check times

for (n in c(1e3, 2e3, 5e3, 1e4, 2e4, 3e4, 4e4, 5e4)) {
  tt <- system.time(play2(wind, figures, N = n))
  message("For n ", n, " time ", tt["elapsed"])
}

# this was before truncating indices
# time is rising exponentially
# For n 1000 time 0.0900000000001455
# For n 2000 time 0.449999999998909
# For n 5000 time 2.31000000000131
# For n 10000 time 9.69999999999891
# For n 20000 time 51.8099999999995
# For n 30000 time 129.540000000001
# For n 40000 time 260.959999999999
# For n 50000 time 436.820000000002

# and this is after
# For n 1000 time 0.0499999999992724
# For n 2000 time 0.109999999998763
# For n 5000 time 0.300000000001091
# For n 10000 time 0.740000000001601
# For n 20000 time 1.32999999999993
# For n 30000 time 2
# For n 40000 time 2.76000000000022
# For n 50000 time 3.54999999999927


Rprof()
play2(wind, figures, N = 2e4)
Rprof(NULL)
summaryRprof()

# still checking indices is the slowest part - but less significantly




# 2 -----------------------------------------------------------------------


# 1 000 000 000 000 is definitely too many for any algorithm
# we need to find some kind of a cycle


out_test <- play(wind_test, figures, 20000)

# we need to check whether there is a cycle of length 40 (or multiples?)
# first we need to create a vector of rows
rocks_test <- unlist(out_test$rocks)
rows_test <- sapply(seq_len(max(rocks_test) %/% 10), function(i) {
  r <- rocks_test[rocks_test %/% 10 == i]
  if (length(r) == 0) {
    0
  } else {
    r <- sort(r %% 10)
    sum(rev(r) * 10^(seq_along(r) - 1))
  }
})
rows <- rows_test
any(rows == 1234567)
# no full rows

# a cycle could start later!!
# and i suppose it is, because we are starting from floor 0, that does not repeat after
for (l in seq(length(wind_test), length(rows) / 2, by = length(wind_test))) {
  message("checking cycle ", l)
  last_start <- length(rows) - 2 * l
  is_cycle <- FALSE

  for (i in 1:last_start) {
    start <- i
    end <- start + l - 1
    is_cycle <- TRUE
    while (end + l <= length(rows)) {
      if (!isTRUE(identical(rows[start:end], rows[(start:end) + l]))) {
        is_cycle <- FALSE
        break
      } else {
        start <- start + l
        end <- end + l
      }
    }
    if (is_cycle) {
      message("cycle for l ", l, " at i ", i)
      break
    }
  }

  if (is_cycle) break
}
# cycle for l 2120 at i 26

# combine tower height with number of rocks
initial_height <- 25
cycle_length <- 2120

# calculate initial number of rocks
which(out_test$tops == initial_height)
initial_rocks <- which(out_test$tops == initial_height)

# now calculate number of rocks until first cycle ends
which(out_test$tops == initial_height + cycle_length)
# three! but which one is really end of cycle?
# let's assume the last one
cycle_rocks <- tail(which(out_test$tops == initial_height + cycle_length), 1) - initial_rocks

n_cycles <- (1000000000000 - initial_rocks) %/% cycle_rocks


# calculate remainder
remainder <- (1000000000000 - initial_rocks) %% cycle_rocks

# calculate height of tower after this number of rocks drop over initial
remainder_height <- out_test$tops[initial_rocks + remainder] - initial_height

print(initial_height + n_cycles * cycle_length + remainder_height, digits = 20)
# 1514285714288




# let's try to find shorter cycles, not bothering about wind vector
for (l in seq_len(length(rows) %/% 2)) {
  message("checking cycle ", l)
  last_start <- length(rows) - 2 * l
  is_cycle <- FALSE

  for (i in 1:last_start) {
    start <- i
    end <- start + l - 1
    is_cycle <- TRUE
    while (end + l <= length(rows)) {
      if (!isTRUE(identical(rows[start:end], rows[(start:end) + l]))) {
        is_cycle <- FALSE
        break
      } else {
        start <- start + l
        end <- end + l
      }
    }
    if (is_cycle) {
      message("cycle for l ", l, " at i ", i)
      break
    }
  }
  if (is_cycle) break
}
# there is one!
# cycle for l 53 at i 26

initial_height <- 25
cycle_length <- 53
initial_rocks <- which(out_test$tops == initial_height)
cycle_rocks <- tail(which(out_test$tops == initial_height + cycle_length), 1) - initial_rocks
n_cycles <- (1000000000000 - initial_rocks) %/% cycle_rocks
remainder <- (1000000000000 - initial_rocks) %% cycle_rocks
remainder_height <- out_test$tops[initial_rocks + remainder] - initial_height
print(initial_height + n_cycles * cycle_length + remainder_height, digits = 20)
# 1514285714288








## input
wind <- strsplit(readLines("input17.txt"), "")[[1]]


# wind has length that is prime number, so there won't be any regular subcycles
numbers::isPrime(length(wind))

out <- play(wind, figures, length(wind) * 3)

rocks <- unlist(out$rocks)
rows <- sapply(seq_len(max(rocks) %/% 10), function(i) {
  r <- rocks[rocks %/% 10 == i]
  if (length(r) == 0) {
    0
  } else {
    r <- sort(r %% 10)
    sum(rev(r) * 10^(seq_along(r) - 1))
  }
})
any(rows == 1234567)
# there are full rows, but what could we do with that knowledge?

# let's try to find shorter cycles, not bothering about wind vector
for (l in seq_len(length(rows) %/% 2)) {
  message("checking cycle ", l)
  last_start <- length(rows) - 2 * l
  is_cycle <- FALSE

  for (i in 1:last_start) {
    start <- i
    end <- start + l - 1
    is_cycle <- TRUE
    while (end + l <= length(rows)) {
      if (!isTRUE(identical(rows[start:end], rows[(start:end) + l]))) {
        is_cycle <- FALSE
        break
      } else {
        start <- start + l
        end <- end + l
      }
    }
    if (is_cycle) {
      message("cycle for l ", l, " at i ", i)
      break
    }
  }
  if (is_cycle) break
}
# cycle for l 2778 at i 349

initial_height <- 348
cycle_length <- 2778

# calculate initial number of rocks
which(out$tops == initial_height)
initial_rocks <- which(out$tops == initial_height)

# now calculate number of rocks until first cycle ends
which(out$tops == initial_height + cycle_length)
# none!
rows[initial_height + (-5:5)]
# cycle is in the middle of the figure!
# it means that we could MOVE cycle upwards, let's try 2 - up to different row
initial_height <- 350
cycle_length <- 2778
initial_rocks <- which(out$tops == initial_height)
cycle_rocks <- which(out$tops == initial_height + cycle_length) - initial_rocks

n_cycles <- (1000000000000 - initial_rocks) %/% cycle_rocks

remainder <- (1000000000000 - initial_rocks) %% cycle_rocks
remainder_height <- out$tops[initial_rocks + remainder] - initial_height

print(initial_height + n_cycles * cycle_length + remainder_height, digits = 20)

# 1591977077352
