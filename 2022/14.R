


read_input <- function(path) {
  p <- readLines(path)
  p <- lapply(strsplit(p, " -> "), function(x) {
    x <- lapply(strsplit(x, ","), as.integer)
    out <- lapply(seq_len(length(x) - 1), function (i) {
      start <- x[[i]]
      end <- x[[i + 1]]
      cbind(
        seq(start[2], end[2]),
        seq(start[1], end[1])
      )
    })
    do.call(rbind, out)
  })
  p <- do.call(rbind, p)
  unique(p)
}



make_grid <- function(points) {
  points[,2] <- points[,2] - min(points[,2]) + 1
  grid <- matrix(0, nrow = max(points[, 1]), ncol = max(points[, 2]))
  grid[points] <- 1
  grid
}

make_plot <- function(grid) {
  grid <- matrix(ifelse(grid == 0, ".", ifelse(grid == 1, "#", "o")), nrow(grid))
  grid <- paste(apply(grid, 1, paste, collapse = ""), collapse = "\n")
  cat(grid)
  invisible(NULL)
}



# 1 -----------------------------------------------------------------------


fill_sand <- function(points) {
  grid <- make_grid(points)
  height <- nrow(grid)
  width <- ncol(grid)

  start <- matrix(c(0, 500 - min(points[,2]) + 1), 1)

  # sand particles
  while (TRUE) {
    sand <- start
    rest <- FALSE
    # single particle fall
    while (!rest) {
      # check whether particle fall out over the borders
      if (sand[1] == height || sand[2] == 0 || sand[2] > width) break

      # move particle
      if (grid[sand[1] + 1, sand[2]] == 0) {
        # down
        sand[1] <- sand[1] + 1

      } else if (sand[2] == 1 || grid[sand[1] + 1, sand[2] - 1] == 0) {
        # left
        sand[1] <- sand[1] + 1
        sand[2] <- sand[2] - 1

      } else if (sand[2] == width || grid[sand[1] + 1, sand[2] + 1] == 0) {
        # right
        sand[1] <- sand[1] + 1
        sand[2] <- sand[2] + 1

      } else {
        # rest
        rest <- TRUE
      }
    }

    # if particle came to rest, mark its position, otherwise stop
    if (rest) {
      grid[sand] <- 2
    } else {
      break
    }
  }

  grid
}

paths_test <- read_input("input14_test.txt")
paths_test |> make_grid() |> make_plot()
out <- fill_sand(paths_test)
make_plot(out)
sum(out == 2)


paths <- read_input("input14.txt")
paths |> make_grid() |> make_plot()
out <- fill_sand(paths)
make_plot(out)
sum(out == 2)
# 838




# 2 -----------------------------------------------------------------------

fill_sand2 <- function(points) {
  grid <- make_grid(points)

  # expand horizontally - maximal value is twice the height of the cave (plus floor)
  # with the start point in the middle
  height <- nrow(grid) + 2
  start <- matrix(c(0, 500 - min(points[,2]) + 1), 1)
  add_left <- max(height - start[2] + 1, 0) # +1 because indexing is from 1 not 0
  add_right <- max(start[2] + height - ncol(grid), 0)

  start[2] <- start[2] + add_left
  grid <- cbind(
    matrix(0, nrow = height - 2, ncol = add_left),
    grid,
    matrix(0, nrow = height - 2, ncol = add_right)
  )

  # add floor
  grid <- rbind(grid, 0, 1)
  height <- nrow(grid)


  # sand particles
  while (TRUE) {
    sand <- start
    rest <- FALSE
    # single particle fall
    while (!rest) {
      # don't need to check the borders - impossible in this case

      # move particle
      if (grid[sand[1] + 1, sand[2]] == 0) {
        # down
        sand[1] <- sand[1] + 1

      } else if (grid[sand[1] + 1, sand[2] - 1] == 0) {
        # left
        sand[1] <- sand[1] + 1
        sand[2] <- sand[2] - 1

      } else if (grid[sand[1] + 1, sand[2] + 1] == 0) {
        # right
        sand[1] <- sand[1] + 1
        sand[2] <- sand[2] + 1

      } else {
        # rest
        rest <- TRUE
      }
    }

    # if particle came to rest and its the source, stop, otherwise mark its position
    if (rest && identical(sand, start)) {
      break
    } else {
      grid[sand] <- 2
    }
  }

  grid
}


paths_test <- read_input("input14_test.txt")
out <- fill_sand2(paths_test)
make_plot(out)
sum(out == 2) + 1


paths <- read_input("input14.txt")
out <- fill_sand2(paths)
make_plot(out)
sum(out == 2) + 1
# 27539

# we could probably ptimize it by working not on a grid but on a list of points
# but anyway it is quick enough
