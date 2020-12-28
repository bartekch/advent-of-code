
# Part 1 ------------------------------------------------------------------



getLoopSize <- function(key, maxit = 1e4) {
  i <- 0
  x <- 1
  while (x != key && i < maxit) {
    x <- (x * 7) %% 20201227
    i <- i + 1
  }
  if (i == maxit) stop("maxint hit")
  i
}


calculateKey <- function(subject, N) {
  key <- 1
  for (i in seq_len(N)) {
    key <- (key * subject) %% 20201227
  }

  key
}


# example
getLoopSize(5764801)
getLoopSize(17807724)
calculateKey(17807724, 8)
calculateKey(5764801, 11)

calculateKey(5764801, getLoopSize(17807724))
calculateKey(17807724, getLoopSize(5764801))


# run
card_public <- 13233401
door_public <- 6552760

calculateKey(card_public, getLoopSize(door_public, maxit = 1e7))
calculateKey(door_public, getLoopSize(card_public, maxit = 1e8))
# 17673381


