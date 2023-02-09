

read_input <- function(path){
  gg <- readLines(path)
  gg <- do.call(rbind, strsplit(gg, ""))
  start <- which(gg == "S", arr.ind = TRUE)
  gg[gg == "S"] <- "a"
  end <- which(gg == "E", arr.ind = TRUE)
  gg[gg == "E"] <- "z"
  gg <- matrix(match(gg, letters), ncol = ncol(gg))
  list(
    start = start,
    end = end,
    h = gg
  )
}



# 1 -----------------------------------------------------------------------




solve_hill <- function(hill) {
  h <- hill$h
  nr <- nrow(h)
  nc <- ncol(h)

  # matrix of costs - counting backwards, so E is 0
  # rest is infinity initially - always greater than any other number
  costs <- matrix(Inf, nr, nc)
  costs[hill$end] <- 0

  candidates <- matrix(FALSE, nr, nc)
  candidates[hill$end] <- TRUE

  while (any(candidates)) {
    cand <- head(which(candidates, arr.ind = TRUE), 1)
    hc <- h[cand]
    cc <- costs[cand]

    ns <- list()
    # up
    if (cand[1] > 1) {
      ns <- c(ns, list(matrix(c(cand[1] - 1, cand[2]), 1)))
    }

    # down
    if (cand[1] < nr) {
      ns <- c(ns, list(matrix(c(cand[1] + 1, cand[2]), 1)))
    }

    # left
    if (cand[2] > 1) {
      ns <- c(ns, list(matrix(c(cand[1], cand[2] - 1), 1)))
    }

    # right
    if (cand[2] < nc) {
      ns <- c(ns, list(matrix(c(cand[1], cand[2] + 1), 1)))
    }


    for (ind in ns) {
      if (h[ind] >= hc - 1 && costs[ind] > cc + 1) {
        costs[ind] <- cc + 1
        candidates[ind] <- TRUE
      }
    }

    candidates[cand] <- FALSE
  }

  costs
}

hill_test <- read_input("input12_test.txt")
solve_hill(hill_test)[hill_test$start]


hill <- read_input("input12.txt")
solve_hill(hill)[hill$start]
# 352


# 2 -----------------------------------------------------------------------

min(solve_hill(hill_test)[which(hill_test$h == 1)])

min(solve_hill(hill)[which(hill$h == 1)])
# 345
