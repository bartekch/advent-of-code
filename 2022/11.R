


test_monkeys <- list(
  list(
    items = c(79, 98),
    ops = function(old) old * 19,
    div_test = 23,
    success = 2,
    fail = 3
  ),
  list(
    items = c(54, 65, 75, 74),
    ops = function(old) old + 6,
    div_test = 19,
    success = 2,
    fail = 0
  ),
  list(
    items = c(79, 60, 97),
    ops = function(old) old * old,
    div_test = 13,
    success = 1,
    fail = 3
  ),
  list(
    items = 74,
    ops = function(old) old + 3,
    div_test = 17,
    success = 0,
    fail = 1
  )
)


monkeys <- list(
  list(
    items = c(53, 89, 62, 57, 74, 51, 83, 97),
    ops = function(old) old * 3,
    div_test = 13,
    success = 1,
    fail = 5
  ),
  list(
    items = c(85, 94, 97, 92, 56),
    ops = function(old) old + 2,
    div_test = 19,
    success = 5,
    fail = 2
  ),
  list(
    items = c(86, 82, 82),
    ops = function(old) old + 1,
    div_test = 11,
    success = 3,
    fail = 4
  ),
  list(
    items = c(94, 68),
    ops = function(old) old + 5,
    div_test = 17,
    success = 7,
    fail = 6
  ),
  list(
    items = c(83, 62, 74, 58, 96, 68, 85),
    ops = function(old) old + 4,
    div_test = 3,
    success = 3,
    fail = 6
  ),
  list(
    items = c(50, 68, 95, 82),
    ops = function(old) old + 8,
    div_test = 7,
    success = 2,
    fail = 4
  ),
  list(
    items = c(75),
    ops = function(old) old * 7,
    div_test = 5,
    success = 7,
    fail =  0
  ),
  list(
    items = c(92, 52, 85, 89, 68, 82),
    ops = function(old) old * old,
    div_test = 2,
    success = 0,
    fail = 1
  )
)



play_turn <- function(monkeys, i) {
  for (item in monkeys[[i]]$items) {
    monkeys[[i]]$counter <- monkeys[[i]]$counter + 1
    item <- monkeys[[i]]$ops(item)
    item <- item %/% 3
    if (item %% monkeys[[i]]$div_test == 0) {
      new_monkey <- monkeys[[i]]$success + 1
    } else {
      new_monkey <- monkeys[[i]]$fail + 1
    }
    monkeys[[new_monkey]]$items <- c(monkeys[[new_monkey]]$items, item)
    monkeys[[i]]$items <- tail(monkeys[[i]]$items, -1)
  }
  monkeys
}

play_game <- function(monkeys, n = 20) {
  for (i in seq_along(monkeys)) {
    monkeys[[i]]$counter <- 0
  }

  for (r in seq_len(n)) {
    for (i in seq_along(monkeys)) {
      monkeys <- play_turn(monkeys, i)
    }
  }

  counters <- sapply(monkeys, function(m) m$counter)
  prod(tail(sort(counters), 2))
}


play_game(test_monkeys, 20)

play_game(monkeys, 20)
# 110220



# 2 -----------------------------------------------------------------------

# we need to use the greatest common divisor of all divisors in order for this to work!

play_turn <- function(monkeys, i, gcd) {
  for (item in monkeys[[i]]$items) {
    monkeys[[i]]$counter <- monkeys[[i]]$counter + 1

    # first apply mod operation
    item <- item %% gcd

    item <- monkeys[[i]]$ops(item)

    if (item %% monkeys[[i]]$div_test == 0) {
      new_monkey <- monkeys[[i]]$success + 1
    } else {
      new_monkey <- monkeys[[i]]$fail + 1
    }
    monkeys[[new_monkey]]$items <- c(monkeys[[new_monkey]]$items, item)
    monkeys[[i]]$items <- tail(monkeys[[i]]$items, -1)
  }
  monkeys
}

play_game <- function(monkeys, n = 20) {
  gcd <- 1
  for (i in seq_along(monkeys)) {
    monkeys[[i]]$counter <- 0
    gcd <- gcd * monkeys[[i]]$div_test
  }

  for (r in seq_len(n)) {
    for (i in seq_along(monkeys)) {
      monkeys <- play_turn(monkeys, i, gcd)
    }
  }

  counters <- sapply(monkeys, function(m) m$counter)
  print(counters)
  prod(tail(sort(counters), 2))
}

play_game(test_monkeys, 10000)

play_game(monkeys, 10000)
# 19457438264
