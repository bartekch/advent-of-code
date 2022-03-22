

read_input <- function(path) {
  l <- readLines(path)
  t(sapply(strsplit(l, ""), as.integer))
}


# 1 -----------------------------------------------------------------------



# work from bottom right - either by row or by column
# for each rectangle keep the lowest risk path
# for new point check both rectangles - to the right and to the left
# choose lower and add current value position

# this would be true only if we could move only right and bottom
# but we could move also left and up!
# so we need to check all neighbours


find_lowest_risk <- function(density) {
  rows <- nrow(density)
  cols <- ncol(density)

  # add border with Infinities so we don't need to check border conditions
  lowest_risk <- matrix(Inf, nrow = rows + 2, ncol = cols + 2)
  lowest_risk[rows + 1, cols + 1] <- density[rows, cols]

  # we need to repeat the whole process until there is no changes
  while (TRUE) {
    change <- FALSE

    for (i in (rows + 1):2) {
      for (j in (cols + 1):2) {
        current_density <- density[i - 1, j - 1]

        lowest_neighbour <- min(
          lowest_risk[i - 1, j],
          lowest_risk[i + 1, j],
          lowest_risk[i, j - 1],
          lowest_risk[i, j + 1]
        )

        if (lowest_risk[i, j] > lowest_neighbour + current_density) {
          change <- TRUE
          lowest_risk[i, j] <- lowest_neighbour + current_density
        }
      }
    }

    if (!change) break
  }


  # subtract starting value
  lowest_risk[2, 2] - density[1, 1]
}


test <- read_input("input15_test.txt")
find_lowest_risk(test)

input <- read_input("input15.txt")
find_lowest_risk(input)
# 717




# 2 -----------------------------------------------------------------------

build_map <- function(map, m = 5) {
  row <- do.call(cbind, lapply(seq_len(m) - 1, function(i) map + i))
  map <- do.call(rbind, lapply(seq_len(m) - 1, function(i) row + i))
  map <- map %% 10 + map %/% 10 # loop 9->1 not 9->0->1
  map
}


test <- read_input("input15_test.txt")
test <- build_map(test)
find_lowest_risk(test)

input <- read_input("input15.txt")
input <- build_map(input)
find_lowest_risk(input)
# 2993
