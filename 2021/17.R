
read_input <- function(path) {
  l <- readLines(path)
  t <- strsplit(strsplit(gsub("target area: |x=|y=", "", l), ", ")[[1]], "\\.\\.")
  list(x = sort(as.integer(t[[1]])), y = sort(as.integer(t[[2]])))
}



# 1 -----------------------------------------------------------------------

trajectory <- function(vx, vy, target) {
  n <- 0
  hit <- FALSE
  initial_vy <- vy
  x <- 0
  y <- 0
  while (!hit) {
    if (y < target$y[1]) break

    if (x >= target$x[1] && x <= target$x[2] && y >= target$y[1] && y <= target$y[2]) {
      hit <- TRUE
      break
    }

    n <- n + 1

    x <- x + vx
    vx <- max(vx - 1, 0)

    y <- y + vy
    vy <- vy - 1
  }

  if (hit) {
    y_trajectory <- seq_len(n) * (2 * initial_vy - seq_len(n) + 1) / 2
    max(y_trajectory)
  } else {
    NA_real_
  }
}


find_highest <- function(target) {
  current_max <- list(value = 0, vx = 0, vy = 0)
  # this works under the assumption that target is right and under
  # horizontal velocity is limited by the further side of target - we cannot move past it in the first shot
  # vertical velocity is limited from both sides by lower side of target
  # if we are shooting downwards this is the same case as horizontal
  # if w are shooting upwards, we now that after following upward quadratic curve
  # the probe would come back to horizontal position y with the same speed value
  # as initial ne but reversed direction - and this is the same case as the first one
  for (vx in 1:target$x[2]) {
    for (vy in target$y[1]:abs(target$y[1])) {
      res <- trajectory(vx, vy, target)
      if (!is.na(res) && res > current_max$value) {
        current_max$value <- res
        current_max$vx <- vx
        current_max$vy <- vy
      }
    }
  }
  current_max
}

test <- read_input("input17_test.txt")
find_highest(test)

input <- read_input("input17.txt")
find_highest(input)
# $value
# [1] 12246
#
# $vx
# [1] 13
#
# $vy
# [1] 156




# 2 -----------------------------------------------------------------------

count_hits <- function(target) {
  n <- 0
  for (vx in 1:target$x[2]) {
    for (vy in target$y[1]:abs(target$y[1])) {
      res <- trajectory(vx, vy, target)
      if (!is.na(res)) {
        n <- n + 1
      }
    }
  }
  n
}
count_hits(test)
count_hits(input)
# 3528




# it is possible to solve it theoretically, at least partially

# maximal horizontal distance is:
# v_x + (v_x - 1) + ... + 1 = v_x * (v_x + 1) / 2
# so minimum initial v_x is:
# sqrt(1 + 8 * t_x1)/2

# for each vx we could calculate after how many steps it would reach target
# warning! it could be infinity if we reach velocity 0 at target

# for the given n we could calculate range of vy that would result in a hit
# however for those vx that result in unbounded n range we would need to perform
# search similar to the implemented solution, although we could limit search grid
# but solving couple more quadratic inequalities
