
read_input <- function(path) {
  g <- readLines(path)
  g <- do.call(rbind, lapply(strsplit(g, ","), as.integer))
  # minimal coordinates has to be 1
  for (i in 1:3) {
    m <- min(g[, i])
    if (m < 1) {
      g[, i] <- g[, i] + abs(m) + 1
    }
  }
  gg <- array(FALSE, dim = apply(g, 2, max))
  gg[g] <- TRUE
  gg
}




# 1 -----------------------------------------------------------------------



count_sides <- function(grid) {
  n_droplets <- sum(grid)
  common_sides <- sum(grid[-1, , ] & grid[-dim(grid)[1], , ]) +
    sum(grid[, -1, ] & grid[, -dim(grid)[2], ]) +
    sum(grid[, , -1] & grid[, , -dim(grid)[3]])

  n_droplets * 6 - common_sides * 2
}


grid_test <- read_input("input18_test.txt")
count_sides(grid_test)


grid <- read_input("input18.txt")
count_sides(grid)
# 3650



# 2 -----------------------------------------------------------------------


# keep information about reachable cubes
# keep information about visited cubes
# keep list of candidates for reachable cubes, as list of dimensions
# initialize list of candidates with all empty cubes on the external sides of a grid
# (should we add one empty line to be sure? I don't think it's necessary)
# in a loop until list of candidates is empty
#   - get candidate from the list
#   - mark it as visited
#   - mark it as reachable
#   - for each of its six neighbours, add them to candidates list if they have not been
#     visited yet and are not cubes
# count external surface area - cubes that are adjacent to reachable cubes

count_sides2 <- function(grid) {
  d <- dim(grid)

  reachable <- array(FALSE, dim = d)
  visited <- reachable

  # initialize candidates
  empty_grid <- which(!grid, arr.ind = TRUE)
  ind <- empty_grid[, 1] == 1 |
    empty_grid[, 1] == d[1] |
    empty_grid[, 2] == 1 |
    empty_grid[, 2] == d[2] |
    empty_grid[, 3] == 1 |
    empty_grid[, 3] == d[3]
  candidates <- empty_grid[ind, , drop = FALSE]

  neighbours_coords <- matrix(
    c(1,0,0, -1,0,0, 0,1,0, 0,-1,0, 0,0,1, 0,0,-1),
    ncol = 3,
    byrow = TRUE
  )

  while (nrow(candidates) > 0) {
    candidate <- candidates[1, , drop = FALSE]
    candidates <- candidates[-1, , drop = FALSE]

    reachable[candidate] <- TRUE
    visited[candidate] <- TRUE

    neighbours <- candidate[rep(1, 6), ] + neighbours_coords
    ind <- neighbours[, 1] >= 1 &
      neighbours[, 1] <= d[1] &
      neighbours[, 2] >= 1 &
      neighbours[, 2] <= d[2] &
      neighbours[, 3] >= 1 &
      neighbours[, 3] <= d[3]
    neighbours <- neighbours[ind, , drop = FALSE]

    for (i in seq_len(nrow(neighbours))) {
      neighbour <- neighbours[i, , drop = FALSE]
      if (!grid[neighbour] && !visited[neighbour]) {
        candidates <- rbind(candidates, neighbour)
      }
    }
  }


  # count external surface area - cubes that are adjacent to reachable cubes
  # plus those on borders
  grid_ind <- which(grid, arr.ind = TRUE)
  cubes_borders <- sum(grid_ind[, 1] == 1) +
    sum(grid_ind[, 1] == d[1]) +
    sum(grid_ind[, 2] == 1) +
    sum(grid_ind[, 2] == d[2]) +
    sum(grid_ind[, 3] == 1) +
    sum(grid_ind[, 3] == d[3])

  reachable_sides <- sum(grid[-1, , ] & reachable[-d[1], , ]) +
    sum(grid[-d[1], , ] & reachable[-1, , ]) +
    sum(grid[, -1, ] & reachable[, -d[2], ]) +
    sum(grid[, -d[2], ] & reachable[, -1, ]) +
    sum(grid[, , -1] & reachable[, , -d[3]]) +
    sum(grid[, , -d[3]] & reachable[, , -1])

  cubes_borders + reachable_sides
}


count_sides2(grid_test)
count_sides2(grid)
# 2118
