

read_input <- function(path) {
  s <- readLines(path)
  s <- gsub(":", ",", s)
  s <- lapply(strsplit(gsub("[^-,[:digit:]]", "", s), ","), function(x) {
    x <- as.integer(x)
    data.frame(sx = x[1], sy = x[2], bx = x[3], by = x[4])
  })
  s <- do.call(rbind, s)
  s$d <- abs(s$sx - s$bx) + abs(s$sy - s$by)
  s
}


# 1 -----------------------------------------------------------------------


check_row <- function(sensors, row) {
  # go through sensor by sensor and gather ranges in the given row
  row_cover <- lapply(seq_len(nrow(sensors)), function(i) {
    sensor <- sensors[i, ]
    range_in_row <- sensor$d - abs(sensor$sy - row)
    if (range_in_row < 0) {
      NULL
    } else {
      data.frame(
        from = sensor$sx - range_in_row,
        to = sensor$sx + range_in_row
      )
    }
  })
  row_cover <- row_cover[!sapply(row_cover, is.null)]

  if (length(row_cover) == 0) return(0)

  # get unique coverage
  row_cover <- Reduce(
    x = row_cover,
    f = function(x, y) {
      remove <- integer()
      for (i in seq_len(nrow(x))) {
        r <- x[i, ]
        if (y$from >= r$from && y$from <= r$to) y$from <- r$to + 1
        if (y$to >= r$from && y$to <= r$to) y$to <- r$from - 1
        if (y$from <= r$from && y$to >= r$to) {
          remove <- c(i, remove)
        }
        if (y$to < y$from) return(x)
      }
      if (length(remove) > 0) x <- x[-remove, ]
      rbind(x, y)
    }
  )

  # remove any beacons in that row
  beacons <- unique(sensors[, c("bx", "by")])
  beacon_c <- 0
  for (i in seq_len(nrow(beacons))) {
    beacon <- beacons[i, ]
    if (beacon$by == row) {
      if (any(beacon$bx >= row_cover$from & beacon$bx <= row_cover$to)) {
        beacon_c <- beacon_c + 1
      }
    }
  }

  # calculate lengths
  sum(row_cover$to - row_cover$from) + nrow(row_cover) - beacon_c
}


sensors_test <- read_input("input15_test.txt")
check_row(sensors_test, 10)
check_row(sensors_test, 0)


sensors <- read_input("input15.txt")
check_row(sensors, 2000000)
# 5108096



# 2 -----------------------------------------------------------------------



check_row2 <- function(sensors, row, limit) {
  # go through sensor by sensor and gather ranges in the given row
  row_cover <- lapply(seq_len(nrow(sensors)), function(i) {
    sensor <- sensors[i, ]
    range_in_row <- sensor$d - abs(sensor$sy - row)
    if (range_in_row < 0) {
      NULL
    } else {
      data.frame(
        from = sensor$sx - range_in_row,
        to = sensor$sx + range_in_row
      )
    }
  })
  row_cover <- row_cover[!sapply(row_cover, is.null)]

  if (length(row_cover) == 0) return(0)

  # get unique coverage
  row_cover <- Reduce(
    x = row_cover,
    f = function(x, y) {
      remove <- integer()
      for (i in seq_len(nrow(x))) {
        r <- x[i, ]
        if (y$from >= r$from && y$from <= r$to) y$from <- r$to + 1
        if (y$to >= r$from && y$to <= r$to) y$to <- r$from - 1
        if (y$from <= r$from && y$to >= r$to) {
          remove <- c(i, remove)
        }
        if (y$to < y$from) return(x)
      }
      if (length(remove) > 0) x <- x[-remove, ]
      rbind(x, y)
    }
  )

  row_cover <- row_cover[row_cover$to >= limit[1], ]
  row_cover <- row_cover[row_cover$from <= limit[2], ]
  row_cover$from <- pmax(row_cover$from, limit[1])
  row_cover$to <- pmin(row_cover$to, limit[2])
  row_cover <- row_cover[row_cover$from <= row_cover$to, ]

  # calculate how many positions are available
  left <- (limit[2] - limit[1] + 1) - (sum(row_cover$to - row_cover$from) + nrow(row_cover))
  if (left == 1) {
    # find exact column
    row_cover <- row_cover[order(row_cover$from), ]
    if (row_cover$from[1] > limit[1]) return(c(limit[1], row))
    if (tail(row_cover$to, 1) < limit[2]) return(c(limit[2], row))
    for (i in seq_len(nrow(row_cover) - 1)) {
      if (row_cover$to[i] + 1 < row_cover$from[i + 1]) return(c(row_cover$to[i] + 1, row))
    }
  } else {
    NULL
  }
}

sensors_test <- read_input("input15_test.txt")
for (y in 0:20) {
  out <- check_row2(sensors_test, y, c(0, 20))
  if (!is.null(out)) {
    print(out)
    print(out[1] * 4000000 + out[2])
    break
  }
}

sensors <- read_input("input15.txt")
for (y in 0:4000000) {
  if (y %% 10000 == 0) message(Sys.time(), " y: ", y)
  out <- check_row2(sensors, y, c(0, 4000000))
  if (!is.null(out)) {
    print(out)
    print(out[1] * 4000000 + out[2])
    break
  }
}
# 2638485 2650264
print(2638485 * 4000000 + 2650264, digits = 20)
# 10553942650264

# working, but took 2,5 hours - better algorithm must exist





# generating full index vector-------------------------------------------------
# does not seem to be any better

check_row3 <- function(sensors, row, limit) {
  ind <- seq(limit[1], limit[2])

  # go through sensor by sensor and cut out indices
  for (i in seq_len(nrow(sensors))) {
    sensor <- sensors[i, ]
    range_in_row <- sensor$d - abs(sensor$sy - row)
    if (range_in_row > 0) {
      ind <- ind[ind < sensor$sx - range_in_row | ind > sensor$sx + range_in_row]
      if (length(ind) == 0) break
    }
  }
  ind
}

sensors_test <- read_input("input15_test.txt")
for (y in 0:20) {
  out <- check_row3(sensors_test, y, c(0, 20))
  if (length(out) == 1) {
    print(out)
    print(out * 4000000 + y)
    break
  }
}

sensors <- read_input("input15.txt")
check_row3(sensors, 2650264, c(0, 4000000))


microbenchmark::microbenchmark(
  check_row2(sensors, 2000000, c(0, 4000000)),
  check_row3(sensors, 2000000, c(0, 4000000))
)

# Unit: milliseconds
# expr        min         lq       mean     median       uq      max neval
# check_row2(sensors, 2e+06, c(0, 4e+06))   3.291401   3.526352   4.073163   3.750351   3.9393  31.0773   100
# check_row3(sensors, 2e+06, c(0, 4e+06)) 126.210101 138.332151 161.227426 146.806651 187.0190 276.7414   100

# much worse - doesn't make sense







# checking ends instead of merging ----------------------------------------

# calculating row cover in vectors instead of loops is significantly better
# using matrices instead of data frames results in considerable speed up

check_row4 <- function(sensors, row, limit) {
  range_in_row <- sensors$d - abs(sensors$sy - row)
  row_cover <- cbind(
    sensors$sx - range_in_row,
    sensors$sx + range_in_row
  )
  row_cover <- row_cover[range_in_row >= 0, ]

  rf <- pmax(row_cover[, 1] - 1, limit[1])
  rt <- pmin(row_cover[, 2] + 1, limit[2])

  # go through ranges one by one and check whether points just outside its range
  # are covered by other sensors
  for (i in seq_len(nrow(row_cover))) {
    if (!any(rf[i] >= row_cover[, 1] & rf[i] <= row_cover[, 2])) return(c(rf[i], row))
    if (!any(rt[i] >= row_cover[, 1] & rt[i] <= row_cover[, 2])) return(c(rt[i], row))
  }
}



check_row2(sensors_test, 11, c(0, 20))
check_row4(sensors_test, 11, c(0, 20))

for (y in 0:20) {
  out <- check_row4(sensors_test, y, c(0, 20))
  if (!is.null(out)) {
    print(out)
    print(out[1] * 4000000 + out[2])
    break
  }
}

check_row4(sensors, 2650264, c(0, 4000000))
microbenchmark::microbenchmark(
  check_row2(sensors, 2000000, c(0, 4000000)),
  check_row4(sensors, 2000000, c(0, 4000000))
)
# faster, but mainly due to processing sensors in vectors and usage of matrices

for (y in 0:4000000) {
  if (y %% 10000 == 0) message(Sys.time(), " y: ", y)
  out <- check_row5(sensors, y, c(0, 4000000))
  if (!is.null(out)) {
    print(out)
    print(out[1] * 4000000 + out[2])
    break
  }
}





# intersections -----------------------------------------------------

# instead of checking row by row we could go alongside each sensor range perimeter
# and check points just outside
# but this solution should be similar in speed, because perimeters are huge
# don't have time to implement

# we could also check lines intersections
# https://www.reddit.com/r/adventofcode/comments/zmjzu7/2022_day_15_part_2_no_search_formula/
# https://github.com/BuonHobo/advent-of-code/blob/master/2022/15/Alex/second.py


find_inters <- function(s, limit) {
  # y = ax + b, a is 1 for ascending, -1 for descending, we are only interested in b
  s$asc_u <- s$sy - s$sx + s$d + 1
  s$asc_b <- s$sy - s$sx - s$d - 1
  s$des_u <- s$sy + s$sx + s$d + 1
  s$des_b <- s$sy + s$sx - s$d - 1

  # searched point lies on intersection of two pairs of overlapping lines - asc and desc
  # overlapping, because if not, then there will be more points
  # also not overlapping when searched point is on the border
  # - we could easily check it with the previous method
  lasc <- c(s$asc_u, s$asc_b)
  lasc <- unique(lasc[duplicated(lasc)])
  ldesc <- c(s$des_u, s$des_b)
  ldesc <- unique(ldesc[duplicated(ldesc)])

  for (a in lasc) {
    for (b in ldesc) {
      ints <- c((b - a) / 2, (a + b) / 2)
      if (any(ints < limit[1]) || any(ints > limit[2])) next
      # intersection has to be below one of bottom lines OR above one of upper
      # for all sensors
      i1 <- ints[2] <= s$asc_b + ints[1]
      i2 <- ints[2] <= s$des_b - ints[1]
      i3 <- ints[2] >= s$asc_u + ints[1]
      i4 <- ints[2] >= s$des_u - ints[1]
      if (all(i1 | i2 | i3 | i4)) return(ints)
    }
  }

}
find_inters(sensors_test, c(0, 20))

find_inters(sensors, c(0, 4e6))
# 2638485 2650264
