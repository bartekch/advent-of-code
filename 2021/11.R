

read_input <- function(path) {
  m <- readLines(path)
  m <- strsplit(m, "")
  m <- lapply(seq_along(m), function(i) {
    data.frame(
      row = i,
      col = seq_along(m),
      energy = as.integer(m[[i]])
    )
  })
  m <- do.call(rbind, m)
  m$id <- seq_len(nrow(m))
  m$flashed <- FALSE
  m$flash_counter <- 0L
  m
}


# 1 -----------------------------------------------------------------------

single_step <- function(octopuses) {
  # increase energy level
  octopuses$energy <- octopuses$energy + 1L

  while (TRUE) {
    to_flash <- octopuses[octopuses$energy > 9L & !octopuses$flashed, ]
    if (nrow(to_flash) == 0) break

    # flash single one at a time
    to_flash <- head(to_flash, 1)
    to_flash_ind <- which(octopuses$id == to_flash$id)
    octopuses$flashed[to_flash_ind] <- TRUE
    octopuses$flash_counter[to_flash_ind] <- octopuses$flash_counter[to_flash_ind] + 1L

    adj_ind <- which(octopuses$row %in% (to_flash$row + -1:1) & octopuses$col %in% (to_flash$col + -1:1))
    octopuses$energy[adj_ind] <- octopuses$energy[adj_ind] + 1L
  }
  octopuses$energy[octopuses$flashed] <- 0L
  octopuses$flashed <- FALSE

  octopuses
}


octopuses_matrix <- function(octopuses) {
  tmp <- matrix(octopuses$energy, nrow = max(octopuses$row), ncol = max(octopuses$col), byrow = TRUE)
  cat(paste(apply(tmp, 1, paste, collapse = ""), collapse = "\n"))
  invisible()
}


navigate <- function(octopuses, steps) {
  for (i in seq_len(steps)) {
    octopuses <- single_step(octopuses)
  }
  octopuses
}

# test
octopuses <- read_input("input11_test.txt")

res <- navigate(octopuses, 10)
octopuses_matrix(res)
sum(res$flash_counter)

res <- navigate(octopuses, 100)
octopuses_matrix(res)
sum(res$flash_counter)


# actual data
octopuses <- read_input("input11.txt")

res <- navigate(octopuses, 100)
octopuses_matrix(res)
sum(res$flash_counter)
# 1634



# 2 -----------------------------------------------------------------------


wait_until_all <- function(octopuses, max_n = 1000) {
  step <- 1
  while (TRUE) {
    octopuses <- single_step(octopuses)
    if (all(octopuses$energy == 0)) break
    step <- step + 1
  }
  step
}

wait_until_all(read_input("input11_test.txt"))

wait_until_all(read_input("input11.txt"))
# 210
