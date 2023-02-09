



snafu_to_dec <- function(x) {
  x <- strsplit(as.character(x), "")[[1]]
  x[x == "-"] <- -1
  x[x == "="] <- -2
  x <- as.integer(x)
  sum(x * 5^((length(x) - 1):0))
}


dec_to_snafu <- function(x) {
  x5 <- character()
  while(x > 0) {
    x5 <- c(x5, x %% 5)
    x <- x %/% 5
  }
  if (length(x5) == 1) {
    if (x5 == "4") {
      return("1-")
    } else if (x5 == "3") {
      return("1=")
    } else {
      return(x5)
    }
  }

  for (i in seq_along(x5)) {
    increase_next <- FALSE
    if (x5[i] == "4") {
      x5[i] <- "-"
      increase_next <- TRUE
    } else if (x5[i] == "3") {
      x5[i] <- "="
      increase_next <- TRUE
    } else if (x5[i] == "5") {
      x5[i] <- "0"
      increase_next <- TRUE
    }

    if (increase_next) {
      if (i == length(x5)) {
        x5 <- c(x5, "1")
      } else {
        x5[i + 1] <- as.integer(x5[i + 1]) + 1

      }
    }
  }
  paste(rev(x5), collapse = "")
}



code_test <- readLines("input25_test.txt")
sapply(code_test, snafu_to_dec)
sapply(code_test, snafu_to_dec) |> sapply(dec_to_snafu)

sapply(code_test, snafu_to_dec) |> sum() |> dec_to_snafu()


code <- readLines("input25.txt")
sapply(code, snafu_to_dec) |> sum() |> dec_to_snafu()
# 2-=2-0=-0-=0200=--21
