
# 1 -----------------------------------------------------------------------

find_least_fuel <- function(path) {
  input <- as.integer(strsplit(readLines(path), ",")[[1]])

  pos <- seq(min(input), max(input))

  fuel <- sapply(pos, function(i) {
    sum(abs(input - i))
  })
  min(fuel)
}

find_least_fuel("input07_test.txt")
find_least_fuel("input07.txt")
# 328318



# 2 -----------------------------------------------------------------------

find_least_fuel2 <- function(path) {
  input <- as.integer(strsplit(readLines(path), ",")[[1]])

  pos <- seq(min(input), max(input))

  fuel <- sapply(pos, function(i) {
    tmp <- abs(input - i)
    sum(tmp * (tmp + 1)) / 2
  })
  min(fuel)
}
find_least_fuel2("input07_test.txt")
find_least_fuel2("input07.txt")
# 89791146
