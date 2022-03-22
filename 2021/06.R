
# 1 -----------------------------------------------------------------------


simulate_fish <- function(path, n = 80) {
  state <- as.integer(strsplit(readLines(path), ",")[[1]])

  for (i in seq_len(n)) {
    ind <- state == 0
    state[!ind] <- state[!ind] - 1
    state[ind] <- 6
    state <- append(state, rep(8, sum(ind)))
  }
  length(state)
}

simulate_fish("input06_test.txt", 18)
simulate_fish("input06_test.txt", 80)

simulate_fish("input06.txt", 80)
# 380758



# 2 -----------------------------------------------------------------------

# better solution - simply count the number of every timer
simulate_fish2 <- function(path, n = 80) {
  state <- as.integer(strsplit(readLines(path), ",")[[1]])

  # timers from 0 to 9
  timers <- numeric(9)
  for (i in 0:8) {
    timers[i + 1] <- sum(state == i)
  }

  for (i in seq_len(n)) {
    new_fish <- timers[1]
    timers <- c(timers[-1], new_fish)
    timers[7] <- timers[7] + new_fish
  }
  sum(timers)
}

simulate_fish2("input06_test.txt", 18)
simulate_fish2("input06_test.txt", 80)
simulate_fish2("input06.txt", 80)
simulate_fish2("input06_test.txt", 256)

res <- simulate_fish2("input06.txt", 256)
format(res, scientific = FALSE)
# 1710623015163
