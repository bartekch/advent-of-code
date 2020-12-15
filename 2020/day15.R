
# Part 1 ------------------------------------------------------------------

# Given your starting numbers, what will be the 2020th number spoken?
  
playGame <- function(input, N) {
  
  # keep the vector with the last time the number has been spoken, indexed with those numbers
  # we are starting from 0 so remember to adjust index!
  numbers_last <- rep(NA_integer_, N + 1)
  numbers_last[head(input, -1) + 1] <- seq_len(length(input) - 1)
  
  previous_number <- tail(input, 1)
  
  for (i in (length(input) + 1):N) {
    # check if the previous number has been spoken
    last_time <- numbers_last[previous_number + 1]
    
    if (is.na(last_time)) {
      current_number <- 0
    } else {
      current_number <- i - last_time - 1
    }
    
    numbers_last[previous_number + 1] <- i - 1
    previous_number <- current_number
  }
  
  current_number
}


# examples
playGame(c(0,3,6), 4)
playGame(c(0,3,6), 5)
playGame(c(0,3,6), 6)
playGame(c(0,3,6), 7)
playGame(c(0,3,6), 8)
playGame(c(0,3,6), 9)
playGame(c(0,3,6), 10)

playGame(c(0,3,6), 2020)

playGame(c(1,3,2), 2020)
playGame(c(2,1,3), 2020)
playGame(c(1,2,3), 2020)
playGame(c(2,3,1), 2020)
playGame(c(3,2,1), 2020)
playGame(c(3,1,2), 2020)
#


# run
input <- "0,14,1,3,7,9"
input <- as.integer(unlist(strsplit(input, ",")))

playGame(input, 2020)
# 763



# Part 2 ------------------------------------------------------------------

# examples
system.time(
  playGame(c(0,3,6), 1e6)
)
system.time(
  playGame(c(0,3,6), 1e7)
)

playGame(c(0,3,6), 3e7)
playGame(c(1,3,2), 3e7)
playGame(c(2,1,3), 3e7)
playGame(c(1,2,3), 3e7)
playGame(c(2,3,1), 3e7)
playGame(c(3,2,1), 3e7)
playGame(c(3,1,2), 3e7)
#

# run
playGame(input, 3e7)
# 1876406

# Conclusions:
# - do NOT use indexing with names instead of ordinary indexing - much slower
# - do NOT perform unnecessary operations (oldening) when the operations could be done smarter
