

# sequence is NOT unique - so we must identify numbers not only by their value!

order_sequence <- function(s) {
  l <- length(s)
  names(s) <- seq_len(l)

  for (i in seq_len(l)) {
    pos <- which(names(s) == i)
    val <- s[pos]

    if (val == 0) next

    # thinks this way - we remove an element from the vector
    # there are l-1 elements remaining
    # we find new position mod l-1
    # element on this position is pushed forward
    # and current element takes its place
    # remember about indexing from 1 - we need to modify mod operation

    new_pos <- (pos + val - 1) %% (l - 1) + 1

    if (new_pos == pos) next

    if (new_pos == 1) {
      s <- c(val, s[-pos])
    } else {
      s <- c(head(s[-pos], new_pos - 1), val, tail(s[-pos], -(new_pos - 1)))
    }
  }

  s
}


find_res <- function(s) {
  ind0 <- which(s == 0)
  l <- length(s)
  pos <- (ind0 + c(1000, 2000, 3000) - 1) %% l + 1
  print(s[pos], digits = 20)
  sum(s[pos])
}

s_test <- as.integer(readLines("input20_test.txt"))
order_sequence(s_test) |> find_res()

s <- as.integer(readLines("input20.txt"))
order_sequence(s) |> find_res()
#    1 3031 3963
# 4191 8244 2862
# 15297




# 2 -----------------------------------------------------------------------

order_sequence2 <- function(s) {
  s <- s * 811589153
  l <- length(s)
  names(s) <- seq_len(l)

  for (i in rep(seq_len(l), times = 10)) {
    pos <- which(names(s) == i)
    val <- s[pos]

    if (val == 0) next

    # thinks this way - we remove an element from the vector
    # there are l-1 elements remaining
    # we find new position mod l-1
    # element on this position is pushed forward
    # and current element takes its place
    # remember about indexing from 1 - we need to modify mod operation
    new_pos <- (pos + val - 1) %% (l - 1) + 1

    if (new_pos == pos) next

    if (new_pos == 1) {
      s <- c(val, s[-pos])
    } else {
      s <- c(head(s[-pos], new_pos - 1), val, tail(s[-pos], -(new_pos - 1)))
    }
  }

  s
}


order_sequence2(s_test) |> find_res()
order_sequence2(s) |> find_res() |> print(digits = 20)
#           2617          3799          3298
# -2035465595724  4967737205513   -34898333579
# [1] 2897373276210
