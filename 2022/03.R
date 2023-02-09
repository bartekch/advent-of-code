

read_input <- function(path) {
  d <- readLines(path)
  strsplit(d, "")
}

ll <- c(letters, LETTERS)


# 1 -----------------------------------------------------------------------

r <- read_input("input03.txt")


lapply(r, function(x) {
  l <- length(x) / 2
  intersect(head(x, l), tail(x, l))
}) |>
  unlist() |>
  match(ll) |>
  sum()
# 7821


# 2 -----------------------------------------------------------------------

r |>
  split(rep(seq(length(r) / 3), each = 3)) |>
  lapply(function(x) {
    Reduce(intersect, x)
  }) |>
  unlist() |>
  match(ll) |>
  sum()
# 2752
