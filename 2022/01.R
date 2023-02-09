

read_input <- function(path) {
  d <- readLines(path)
  d <- as.numeric(d)
  d <- split(d, cumsum(is.na(d)))
  lapply(d, function(x) Filter(Negate(is.na), x))
}




# 1 -----------------------------------------------------------------------

cals <- read_input("input01.txt")
max(sapply(cals, sum))
# 71471


# 2 -----------------------------------------------------------------------

sapply(cals, sum) |>
  sort() |>
  tail(3) |>
  sum()
# 211189
