

read_input <- function(path) {
  g <- do.call(rbind, strsplit(readLines(path), ""))
  mode(g) <- "integer"
  g
}



# 1 -----------------------------------------------------------------------


count_vis <- function(g) {
  nc <- ncol(g)
  nr <- nrow(g)
  v <- (nc + nr) * 2 - 4

  for (i in 2:(nr - 1)) {
    for (j in 2:(nc - 1)) {
      h <- g[i, j]
      # top
      if (all(h > g[1:(i - 1), j])) {
        v <- v + 1
        next
      }
      # bottom
      if (all(h > g[(i + 1):nr, j])) {
        v <- v + 1
        next
      }
      # left
      if (all(h > g[i, 1:(j - 1)])) {
        v <- v + 1
        next
      }
      # right
      if (all(h > g[i, (j + 1):nc])) {
        v <- v + 1
        next
      }
    }
  }
  v
}


g_test <- read_input("input08_test.txt")
count_vis(g_test)


g <- read_input("input08.txt")
count_vis(g)
# 1763



# 2 -----------------------------------------------------------------------


count_scenic <- function(g) {
  nc <- ncol(g)
  nr <- nrow(g)
  score <- matrix(0, nr, nc)

  for (i in 2:(nr - 1)) {
    for (j in 2:(nc - 1)) {
      h <- g[i, j]

      # top
      t <- (h <= g[(i - 1):1, j])
      t <- if (any(t)) head(which(t), 1) else length(t)

      # bottom
      b <- (h <= g[(i + 1):nr, j])
      b <- if (any(b)) head(which(b), 1) else length(b)

      # left
      l <- (h <= g[i, (j - 1):1])
      l <- if (any(l)) head(which(l), 1) else length(l)

      # right
      r <- (h <= g[i, (j + 1):nc])
      r <- if (any(r)) head(which(r), 1) else length(r)

      score[i, j] <- t * b * l * r
    }
  }
  max(score)
}

count_scenic(g_test)
count_scenic(g)
# 671160
