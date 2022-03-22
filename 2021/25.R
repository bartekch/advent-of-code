

# 0 - empty
# 1 - east
# 2 - south

read_input <- function(path) {
  l <- readLines(path)
  out <- do.call(rbind, lapply(strsplit(l, ""), function(x) c("." = 0L, ">" = 1L, "v" = 2L)[x]))
  colnames(out) <- NULL
  out
}

plot_m <- function(m) {
  cat(paste(apply(matrix(c(".", ">", "v")[m + 1], ncol = ncol(m)), 1, paste, collapse = ""), collapse = "\n"))
  invisible()
}

single_step <- function(m) {
  nc <- ncol(m)
  nr <- nrow(m)

  # check which cucumber could move east
  m <- cbind(m, m[, 1])
  inde <- (m[, 1:nc] == 1) & (m[, 2:(nc + 1)] == 0)
  # move
  m[, 1:nc][inde] <- 0L
  m[, 2:(nc + 1)][inde] <- 1L
  # those cucumbers from the last column, that moved, should be merged into the first one
  m[, 1][inde[, nc]] <- 1L
  m <- m[, -(nc + 1)]


  # check which cucumber could move south
  m <- rbind(m, m[1, ])
  inds <- (m[1:nr, ] == 2) & (m[2:(nr + 1), ] == 0)
  # move
  m[1:nr, ][inds] <- 0L
  m[2:(nr + 1), ][inds] <- 2L
  # those cucumbers from the last row, that moved, should be merged into the first one
  m[1, ][inds[nr, ]] <- 2L
  m <- m[-(nr + 1), ]

  # if there were no movements we are finished - return NULL
  if (any(inde) || any(inds)) {
    m
  }
}


move_n <- function(m, n) {
  for (i in seq_len(n)) {
    m <- single_step(m)
  }
  m
}

solve <- function(m) {
  n <- 0
  while (TRUE) {
    n <- n + 1
    out <- single_step(m)
    if (is.null(out)) break
    m <- out
  }
  list(m = m, n = n)
}

test <- read_input("input25_test.txt")
plot_m(test)
plot_m(single_step(test))
plot_m(move_n(test, 50))
res <- solve(test)


input <- read_input("input25.txt")
res <- solve(input)
plot_m(res$m)
res$n
# 353
