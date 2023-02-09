
read_input <- function(path) {
  d <- readLines(path)
  d <- strsplit(d, "[-,]")
  lapply(d, as.integer)
}


# 1 -----------------------------------------------------------------------


p <- read_input("input04.txt")

sapply(p, function(x) {
  (x[1] >= x[3] & x[2] <= x[4]) ||
    (x[1] <= x[3] & x[2] >= x[4])
}) |>
    sum()
# 444



# 2 -----------------------------------------------------------------------


sapply(p, function(x) {
  x[1] <= x[4] & x[2] >= x[3]
}) |>
  sum()
# 801
