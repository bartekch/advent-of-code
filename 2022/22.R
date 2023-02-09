

read_input <- function(path) {
  g <- readLines(path)

  ind <- which(g == "")

  code <- paste(tail(g, -ind), collapse = "")
  steps <- strsplit(code, "[RL]")[[1]]
  turns <- strsplit(code, "[[:digit:]]+")[[1]][-1]
  code <- character(length(steps) * 2 - 1)
  code[seq(1, to = length(code), by = 2)] <- steps
  code[seq(2, to = length(code), by = 2)] <- turns

  grid <- strsplit(head(g, ind - 1), "")
  max_l <- max(sapply(grid, length))
  grid <- lapply(grid, function(g) {
    l <- length(g)
    if (l < max_l) {
      g <- c(g, rep(" ", max_l - l))
    }
    g
  })
  grid <- do.call(rbind, grid)
  wall <- (grid == "#")
  floor <- (grid == ".")
  space <- (grid == " ")


  list(
    code = code,
    grid = list(
      wall = wall,
      floor = floor,
      space = space
    )
  )
}



find_path <- function(g) {
  code <- g$code

  wall <- g$grid$wall
  space <- g$grid$space
  floor <- g$grid$floor
  nr <- nrow(space)
  nc <- ncol(space)

  # first coordinate is row, with numbers going up from the first row downwards
  # second coordinate is column, with numbers going up from the first column rightwards

  pos <- matrix(c(1, head(which(floor[1, ]), 1)), ncol = 2)
  dr <- matrix(c(0, 1), ncol = 2)

  for (cc in code) {
    if (cc == "R") {
      dr <- dr %*% matrix(c(0, 1, -1, 0), ncol = 2)

    } else if (cc == "L") {
      dr <- dr %*% matrix(c(0, -1, 1, 0), ncol = 2)

    } else {
      steps <- as.integer(cc)

      while (steps > 0) {
        next_pos <- pos + dr

        if (next_pos[1] < 1 || next_pos[1] > nr || next_pos[2] < 1 || next_pos[2] > nc || space[next_pos]) {
          if (dr[1] == 0) {
            # find in row
            ind <- which(!space[pos[1], ])
            if (dr[2] == 1) {
              # going into right side, out of left side
              next_pos[2] <- head(ind, 1)
            } else {
              # going into left side, out of right side
              next_pos[2] <- tail(ind, 1)
            }
          } else {
            # find in column
            ind <- which(!space[, pos[2]])
            if (dr[1] == 1) {
              # going into bottom, out of top
              next_pos[1] <- head(ind, 1)
            } else {
              # going into top, out of bottom
              next_pos[1] <- tail(ind, 1)
            }
          }
        }
        if (wall[next_pos]) break
        pos <- next_pos
        steps <- steps - 1
      }
    }
  }

  message("final pos ", pos[1], " ", pos[2], " direction ", dr[1], " ", dr[2])
  if (dr[1] == 1) {
    dr <- 1
  } else if (dr[1] == -1) {
    dr <- 3
  } else if (dr[2] == 1) {
    dr <- 0
  } else {
    dr <- 2
  }
  1000 * pos[1] + 4 * pos[2] + dr
}



g_test <- read_input("input22_test.txt")
find_path(g_test)



g <- read_input("input22.txt")
find_path(g)
# final pos 146 23 direction 0 1
# [1] 146092



# 2 -----------------------------------------------------------------------

find_path2 <- function(g) {
  code <- g$code

  wall <- g$grid$wall
  space <- g$grid$space
  floor <- g$grid$floor
  nr <- nrow(space)
  nc <- ncol(space)

  # first coordinate is row, with numbers going up from the first row downwards
  # second coordinate is column, with numbers going up from the first column rightwards

  # sides - my input is different than test!!!
  #  12
  #  3
  # 45
  # 6


  pos <- matrix(c(1, head(which(floor[1, ]), 1)), ncol = 2)
  dr <- matrix(c(0, 1), ncol = 2)

  for (cc in code) {
    if (cc == "R") {
      dr <- dr %*% matrix(c(0, 1, -1, 0), ncol = 2)

    } else if (cc == "L") {
      dr <- dr %*% matrix(c(0, -1, 1, 0), ncol = 2)

    } else {
      steps <- as.integer(cc)

      while (steps > 0) {
        next_pos <- pos + dr
        # by default we are continuing with the same direction
        # but it may change when switching sides
        next_dr <- dr

        if (next_pos[1] < 1 || next_pos[1] > nr || next_pos[2] < 1 || next_pos[2] > nc || space[next_pos]) {
          # let's do it side by side - a lot of code but easier to think about
          # first we need to identify side
          if (pos[1] <= 50) {
            if (pos[2] <= 100) {
              side <- 1
            } else {
              side <- 2
            }
          } else if (pos[1] <= 100) {
            side <- 3
          } else if (pos[1] <= 150) {
            if (pos[2] <= 50) {
              side <- 4
            } else {
              side <- 5
            }
          } else {
            side <- 6
          }

          if (side == 1) {
            if (dr[2] == 1) {
              # into right, continuing into 2, no change

            } else  if (dr[2] == -1) {
              # into left, out of left side of 4 top<->bottom, direction right
              next_pos[1] <- 150 - pos[1] + 1
              next_pos[2] <- 1
              next_dr <- matrix(c(0, 1), ncol = 2)

            } else if (dr[1] == -1) {
              # into top, out of left side of 6, direction right
              next_pos[1] <- pos[2] + 100
              next_pos[2] <- 1
              next_dr <- matrix(c(0, 1), ncol = 2)

            } else {
              # into bottom, continuing into 3, no change
            }


          } else if (side == 2) {
            if (dr[2] == 1) {
              # into right, out of right side of 5 top<->bottom, direction left
              next_pos[1] <- 150 - pos[1] + 1
              next_pos[2] <- 100
              next_dr <- matrix(c(0, -1), ncol = 2)

            } else  if (dr[2] == -1) {
              # into left, continuing into 1, no change

            } else if (dr[1] == -1) {
              # into top, out of bottom side of 6, direction still up
              next_pos[1] <- 200
              next_pos[2] <- pos[2] - 100

            } else {
              # into bottom, out of right side of 3, direction left
              next_pos[1] <- pos[2] - 50
              next_pos[2] <- 100
              next_dr <- matrix(c(0, -1), ncol = 2)
            }

          } else if (side == 3) {
            if (dr[2] == 1) {
              # into right, out of bottom side of 2, direction up
              next_pos[1] <- 50
              next_pos[2] <- pos[1] + 50
              next_dr <- matrix(c(-1, 0), ncol = 2)

            } else  if (dr[2] == -1) {
              # into left, out of top side of 4, direction down
              next_pos[1] <- 101
              next_pos[2] <- pos[1] - 50
              next_dr <- matrix(c(1, 0), ncol = 2)

            } else if (dr[1] == -1) {
              # into top, continuing into 1, no change

            } else {
              # into bottom, continuing into 5, no change
            }

          } else if (side == 4) {
            if (dr[2] == 1) {
              # into right, continuing into 5, no change

            } else  if (dr[2] == -1) {
              # into left, out of left side of 1 top<->bottom, direction right
              next_pos[1] <- 150 - pos[1] + 1
              next_pos[2] <- 51
              next_dr <- matrix(c(0, 1), ncol = 2)

            } else if (dr[1] == -1) {
              # into top, out of left side of 3, direction right
              next_pos[1] <- pos[2] + 50
              next_pos[2] <- 51
              next_dr <- matrix(c(0, 1), ncol = 2)

            } else {
              # into bottom, continuing into 6, no change
            }

          } else if (side == 5) {
            if (dr[2] == 1) {
              # into right, out of right side of 2 top<->bottom, direction left
              next_pos[1] <- 150 - pos[1] + 1
              next_pos[2] <- 150
              next_dr <- matrix(c(0, -1), ncol = 2)

            } else  if (dr[2] == -1) {
              # into left, continuing into 4, no change

            } else if (dr[1] == -1) {
              # into top,  continuing into 3, no change


            } else {
              # into bottom, out of right side of 6, direction left
              next_pos[1] <- pos[2] + 100
              next_pos[2] <- 50
              next_dr <- matrix(c(0, -1), ncol = 2)
            }

          } else if (side == 6) {
            if (dr[2] == 1) {
              # into right, out of bottom side of 5, direction up
              next_pos[1] <- 150
              next_pos[2] <- pos[1] - 100
              next_dr <- matrix(c(-1, 0), ncol = 2)

            } else  if (dr[2] == -1) {
              # into left, out of top side of 1, direction down
              next_pos[1] <- 1
              next_pos[2] <- pos[1] - 100
              next_dr <- matrix(c(1, 0), ncol = 2)

            } else if (dr[1] == -1) {
              # into top, continuing into 4, no change

            } else {
              # into bottom, out of top side of 2, direction still down
              next_pos[1] <- 1
              next_pos[2] <- pos[2] + 100
            }
          }
        }


        if (wall[next_pos]) break
        pos <- next_pos
        dr <- next_dr
        steps <- steps - 1
      }
    }
  }

  message("final pos ", pos[1], " ", pos[2], " direction ", dr[1], " ", dr[2])
  if (dr[1] == 1) {
    dr <- 1
  } else if (dr[1] == -1) {
    dr <- 3
  } else if (dr[2] == 1) {
    dr <- 0
  } else {
    dr <- 2
  }
  1000 * pos[1] + 4 * pos[2] + dr
}


# solution is specific for grid in my input, I didn't bother to recognise patters
g <- read_input("input22.txt")
find_path2(g)
# final pos 110 85 direction 0 -1
# [1] 110342
