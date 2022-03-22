
parse_input <- function(path) {
  do.call(rbind, lapply(strsplit(readLines(path), ""), as.integer))
}

# 1 -----------------------------------------------------------------------

find_low_points <- function(input) {
  nr <- nrow(input)
  nc <- ncol(input)

  ind <- input < cbind(Inf, input[, -nc]) &
    input < cbind(input[, -1], Inf) &
    input < rbind(Inf, input[-nr, ]) &
    input < rbind(input[-1, ], Inf)

  sum(input[ind] + 1)
}


find_low_points(parse_input("input09_test.txt"))
find_low_points(parse_input("input09.txt"))
# 465



# 2 -----------------------------------------------------------------------

# for each point
# check whether it is 9, if not continue
# check whether it is already in basin, if not assign new basin number
# check point on the right and on the bottom, if they are not 9 assign them this
#   point basin number

find_basins <- function(input) {
  nr <- nrow(input)
  nc <- ncol(input)

  basins <- matrix(0, nr, nc)
  n_basins <- 0

  for (i in 1:nr) {
    for (j in 1:nc) {
      if (input[i,j] == 9) next

      current_basin <- basins[i,j]
      if (current_basin == 0) {
        # check neighbour on the right, because it could have basin assigned already
        if (j < nc && basins[i, j+1] > 0) {
          current_basin <- basins[i,j] <- basins[i, j+1]
        } else {
          n_basins <- n_basins + 1
          current_basin <- basins[i,j] <- n_basins
        }
      }

      if (j < nc) {
        if (basins[i, j+1] > 0) {
          # if neighbour on the right has already basin assigned, we should merge them
          basins[basins == basins[i, j+1]] <- current_basin
        } else if (input[i,j+1] < 9) {
          # otherwise assign the current basin
          basins[i, j+1] <- current_basin
        }
      }
      if (i < nr) {
        if (input[i+1,j] < 9) basins[i+1, j] <- current_basin
      }
    }
  }

  basins_size <- table(basins[basins > 0])
  prod(head(sort(basins_size, decreasing = TRUE), 3))
}

find_basins(parse_input("input09_test.txt"))
find_basins(parse_input("input09.txt"))
# 1269555
