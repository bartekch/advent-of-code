

read_input <- function(path) {
  d <- readLines(path)
  d <- d[d!= ""]
  d <- split(d, rep(seq_len(length(d) / 2), each = 2))
  lapply(d, function(x) list(
    jsonlite::fromJSON(x[1], simplifyVector = FALSE),
    jsonlite::fromJSON(x[2], simplifyVector = FALSE)
  ))
}



# 1 -----------------------------------------------------------------------

test_pair <- function(pl, pr) {

  if (is.integer(pl) && is.integer(pr)) {
    if (pl < pr) return(TRUE)
    if (pl > pr) return(FALSE)
    return(NA)

  } else if (is.list(pl) && is.list(pr)) {
    for (i in seq_along(pl)) {
      # if right side runs out of elements - incorrect order
      if (i > length(pr)) return(FALSE)

      res <- test_pair(pl[[i]], pr[[i]])
      if (isTRUE(res)) {
        return(TRUE)
      } else if (isFALSE(res)) {
        return(FALSE)
      }
      # if inconclusive, continue
    }

    # if there are still some elements left in the right list, order is correct
    # otherwise inconclusive
    if (length(pl) < length(pr)) {
      return(TRUE)
    } else {
      return(NA)
    }

  } else {
    if (is.list(pl)) {
      return(test_pair(pl, list(pr)))
    } else {
      return(test_pair(list(pl), pr))
    }
  }

  TRUE
}


packets_test <- read_input("input13_test.txt")
sum(which(sapply(packets_test, function(x) test_pair(x[1], x[2]))))

packets <- read_input("input13.txt")
sum(which(sapply(packets, function(x) test_pair(x[1], x[2]))))
# 5390




# 2 -----------------------------------------------------------------------


# implement custom comparison for packet class
# however sort works only for atomic, so we need to use strings
# and parse to jsons inside

read_input2 <- function(path) {
  d <- readLines(path)
  d <- d[d!= ""]
  d <- c(d, "[[2]]", "[[6]]")
  class(d) <- "packet"
  d
}

# an example from SO
# indexing
'[.packet' <- function(x, i) {
  structure(unclass(x)[i], class = "packet")
}

# comparing
'>.packet' <- function(a, b) {
  test_pair(jsonlite::fromJSON(a, simplifyVector = FALSE), jsonlite::fromJSON(b, simplifyVector = FALSE))
}

# if we can't find a difference, then there is no difference
'==.packet' <- function(a, b) identical(a, b)





packets_test <- read_input2("input13_test.txt")
prod(match(c("[[2]]", "[[6]]"), sort(packets_test, decreasing = TRUE)))

packets <- read_input2("input13.txt")
prod(match(c("[[2]]", "[[6]]"), sort(packets, decreasing = TRUE)))
# 19261
