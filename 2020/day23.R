
# Part 1 ------------------------------------------------------------------

playCups <- function(cups, N = 100) {
  l <- length(cups)
  current_cup_position <- 1
  
  for (i in 1:N) {
    current_cup_value <- cups[current_cup_position]
    
    # pick up three cups
    if (current_cup_position <= l - 3) {
      picked_cups <- cups[current_cup_position + 1:3]
      cups <- c(head(cups, current_cup_position), tail(cups, l - current_cup_position - 3))
      
    } else if (current_cup_position == l - 2) {
      picked_cups <- cups[c(current_cup_position + 1:2, 1)]
      cups <- cups[2:current_cup_position]
      current_cup_position <- current_cup_position - 1
      
    } else if (current_cup_position == l - 1) {
      picked_cups <- cups[c(current_cup_position + 1, 1:2)]
      cups <- cups[3:current_cup_position]
      current_cup_position <- current_cup_position - 2
      
    } else {
      picked_cups <- head(cups, 3)
      cups <- tail(cups, -3)
      current_cup_position <- current_cup_position - 3
    }
    
    # find destination cup
    tmp_ind <- cups < current_cup_value
    destination_cup_position <- which.max(cups[tmp_ind])
    if (length(destination_cup_position) == 0) {
      destination_cup_position <- which.max(cups)
    } else {
      destination_cup_position <- which(tmp_ind)[destination_cup_position]
    }
    if (destination_cup_position < current_cup_position) {
      current_cup_position <- current_cup_position + 3
    }
    
    # insert picked cups
    cups <- c(cups[1:destination_cup_position], picked_cups, tail(cups, -destination_cup_position))
    
    # find the position of the new current cup
    current_cup_position <- current_cup_position + 1
    if (current_cup_position > l) {
      current_cup_position <- 1
    }
  }
  
  # set up 1 as the first cup
  ind <- which(cups == 1)
  cups <- c(1, tail(cups, -ind), head(cups, ind - 1))
  
  #
  cups
}


# example
example <- as.integer(unlist(strsplit("389125467", "")))
res_ex <- playCups(example)
paste(res_ex[-1], collapse = "")
# [1] "67384529"


# run
input <- as.integer(unlist(strsplit("318946572", "")))
res_run <- playCups(input)
paste(res_run[-1], collapse = "")
# [1] "52864379"




# Part 2 ------------------------------------------------------------------

# example
# extend the cups
example2 <- c(example, seq(from = max(example) + 1, by = 1L, length.out = 1e6 - length(example)))
system.time(
  res_ex2 <- playCups(example2, N = 1e3)
)
# N = 1e3
#   user  system elapsed 
# 24.634   0.961  25.913 
# N = 1e4
# user  system elapsed 
# 261.847   9.642 276.976 
# we need N = 1e7, so it looks infeasible in that way



### Rcpp implementation

library("Rcpp")

Rcpp::sourceCpp("day23.cpp")

#3 Part 1

# example
res_ex_cpp <- playCupsCpp(example, maxit = 100)
ind <- which(res_ex_cpp == 1)
res_ex_cpp <- c(1, tail(res_ex_cpp, -ind), head(res_ex_cpp, ind - 1))
paste(res_ex_cpp[-1], collapse = "")


# run
res_run_cpp <- playCupsCpp(input, maxit = 100)
ind <- which(res_run_cpp == 1)
res_run_cpp <- c(1, tail(res_run_cpp, -ind), head(res_run_cpp, ind - 1))
paste(res_run_cpp[-1], collapse = "")



## 'part 2

# example
system.time(
  res_ex2_cpp <- playCupsCpp(example2, maxit = 1e4)
)
# maxit = 1e3
#   user  system elapsed 
# 11.290   0.323  11.755 
# maxit = 1e4
#    user  system elapsed 
# 145.610  13.119 161.223

# still way too slow, need to write better Rcpp



### Rcpp second implementation

## Part 1

# example
ttt <- playCupsCpp2(example, maxit = 100)
res_ex_cpp <- ttt[[1]]
ind <- which(res_ex_cpp == 1)
res_ex_cpp <- c(1, tail(res_ex_cpp, -ind), head(res_ex_cpp, ind - 1))
paste(res_ex_cpp[-1], collapse = "")


# run
ttt <- playCupsCpp2(input, maxit = 100)
res_run_cpp <- ttt[[1]]
ind <- which(res_run_cpp == 1)
res_run_cpp <- c(1, tail(res_run_cpp, -ind), head(res_run_cpp, ind - 1))
paste(res_run_cpp[-1], collapse = "")


## Part 2


# example
system.time(
  res_ex2_cpp2 <- playCupsCpp2(example2, maxit = 1e5)[[1]]
)
all.equal(res_ex2_cpp, res_ex2_cpp2)
# maxit = 1e3
#  user  system elapsed 
# 0.824   0.004   0.840
# maxit = 1e4
#  user  system elapsed 
# 8.111   0.027   8.239 
all.equal(res_ex2_cpp, res_ex2_cpp2_e4)
# maxit = 1e5
#   user  system elapsed 
# 72.353   0.169  73.526 

# so for 1e7 it should take around 7000s ~ 2h
# way too long, but currently I don't have better ideas so it's worth a shot

# run
input2 <- c(input, seq(from = max(input) + 1, by = 1L, length.out = 1e6 - length(input)))
system.time(
  res_run2_cpp2 <- playCupsCpp2(input2, maxit = 1e7)
)
#     user   system  elapsed 
# 5791.742   12.304 5902.604 
ind <- which(res_run2_cpp2[[1]] == 1)
prod(res_run2_cpp2[[1]][ind + 1:2])
# 11591415792






### Optimization after the following hint from Reddir:
# Store all cups in an array as the index of the next cup, and store the value of
# the current cup separately. Tying in part 2's new cups is a bit finicky,
# but you can verify the correctness by asserting that it takes 1M current = next_list[current]
# to get back to the first cup.

recreateList <- function(x, start = 1) {
  res <- integer(length(x))
  res[1] <- start
  for (i in seq_len(length(x) - 1)) {
    res[i + 1] <- x[res[i]]
  }
  res
}

# this assumes that there are no gaps
playCupsOpt <- function(cups, N = 100) {
  current_cup <- cups[1]
  
  # store index of the next cup under each index
  next_cups <- c(tail(cups, -1), cups[1])
  cups <- next_cups[order(cups)]
  
  max_cup <- length(cups)
  picked_cups <- integer(3)
  
  for (i in 1:N) {
    
    # pick up three cups
    picked_cups[1] <- cups[current_cup]
    picked_cups[2] <- cups[picked_cups[1]]
    picked_cups[3] <- cups[picked_cups[2]]
    
    # remove picked cups
    cups[current_cup] <- cups[picked_cups[3]]
    
    # find destination cup
    destination_cup <- current_cup - 1
    while (TRUE) {
      if (destination_cup == 0) {
        destination_cup <- max_cup
      } else if (destination_cup %in% picked_cups) {
        destination_cup <- destination_cup - 1
      } else {
        break
      }
    }
    
    # insert picked cups
    old_next <- cups[destination_cup]
    cups[destination_cup] <- picked_cups[1]
    cups[picked_cups[1]] <- picked_cups[2]
    cups[picked_cups[2]] <- picked_cups[3]
    cups[picked_cups[3]] <- old_next
    
    # find the position of the new current cup
    current_cup <- cups[current_cup]
  }
  
  
  # recreate cups order starting from 1
  recreateList(cups)
}


## Part 1

# example
res_ex_opt <- playCupsOpt(example)
paste(res_ex_opt[-1], collapse = "")

# run
res_run_opt <- playCupsOpt(input)
paste(res_run_opt[-1], collapse = "")


# speed comparison
bench <- microbenchmark::microbenchmark(
  playCups(input),
  playCupsOpt(input),
  times = 1e4
)
bench
ggplot2::autoplot(bench)
# nice improvement


## Part 2

# example
system.time(
  res_ex2_opt <- playCupsOpt(example2, N = 1e7)
)
#   user  system elapsed 
# 21.656   0.047  21.949 
prod(res_ex2_opt[2:3])



# run
system.time(
  res_run2_opt <- playCupsOpt(input2, N = 1e7)
)
#   user  system elapsed 
# 20.810   0.056  21.107 
# MUCH better (100x time than Rcpp solution)
prod(res_run2_opt[2:3])



# now we could recreate it in Rcpp as well - should be even faster

## Rcpp implementation after optimization

