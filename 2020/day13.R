
# Part 1 ------------------------------------------------------------------

# What is the ID of the earliest bus you can take to the airport multiplied by the number of minutes you'll need to wait for that bus?


input <- readLines("2020/inputs/input13.txt")

my_timestamp <- as.integer(input[1])

buses <- as.integer(Filter(function(x) x != "x", unlist(strsplit(input[[2]], ","))))



waiting_times <- (((my_timestamp %/% buses) + 1) * buses) %% my_timestamp


min(waiting_times * buses[which.min(waiting_times)])

# 3215



# Part 2 ------------------------------------------------------------------

# What is the earliest timestamp such that all of the listed bus IDs depart at offsets matching their positions in the list?


buses <- unlist(strsplit(input[[2]], ","))

ts <- which(buses != "x") - 1
buses <- as.numeric(buses[buses != "x"])

# check whether buses ids are coprime
tmp <- expand.grid(x = buses, y = buses)
tmp <- tmp[tmp$x > tmp$y,]
all(apply(tmp, 1, function(x) numbers::coprime(x[1], x[2])))
# ok, so we could apply the chinese remainder theorem

# reduce remainders
ts <- ts %% buses




# brute force approach first

# example
all <- 19 * (1:1e6) - 3
all <- intersect(all, seq(13, max(all), by = 13) - 2)
all <- intersect(all, seq(17, max(all), by = 17))
min(all)
#



all <- buses[1] * (1:1e7) - ts[1]
for (i in 2:length(buses)) {
  if (length(all) == 0) {
    print("too small")
    break
  }
  all <- intersect(all, seq(buses[i], max(all), by = buses[i]) - ts[i])
}
min(all)

# not feasible, we are expecting number >  1e14!!


# back to chinese

# examples
numbers::chinese(c(0, 13-2, 19-3), c(17,13,19))
numbers::chinese(c(0, 6, 57, 58), c(67,7,59,61))
numbers::chinese(c(0, 5, 56, 57), c(67,7,59,61))
numbers::chinese(c(0, 6, 56, 57), c(67,7,59,61))
numbers::chinese(c(0, 36, 45, 1886), c(1789,37,47,1889))

b <- c(7,13,"x","x",59,"x",31,19)
t <- which(b != "x") - 1
b <- as.integer(b[t+1])

numbers::chinese(b-t, b)
numbers::chinese((b-(t%%b))%%b, b)

# run
tmstmp <- numbers::chinese((buses - ts) %% buses, buses)
format(tmstmp, scientific = FALSE)
# error due to exceeding integer accuracy
# 1001569619313249
# 1001569619313439 correct answer found in an online calculator




## alter functions from the numbers package to use long integer
## functions are trimmed from everything that is not needed in our case
library(bit64)

chinese64 <- function(a, m) {
  M <- prod(m)   # this works fine for integer64
  x <- as.integer64(0)
  for (i in 1:length(m)) {
    Mmi <- prod(m[-i]) # this should work fine as well since it's product of shorter vector
    mmi <- modinv64(Mmi, m[i]) # this is a problem
    x <- x + a[i] * Mmi * mmi # this should work fine as well
  }
  return(x%%M)
}


modinv64 <- function(n, m) {
  v <- extGCD64(n, m) # this is a problem 
  if (v[2] >= 0) {
    v[2]
  } else {
    v[2] + m
  }
}

# creating matrix from integer64 strips class!!
# we need to simulate this as a list with two elements for two rows
extGCD64 <- function (a, b) {
  sign_ab <- sign(c(a, b))
  A <- list(row1 = c(abs(a), 1, 0),
            row2 = c(abs(b), 0, 1))
  while (A$row1[1] * A$row2[1] != 0) {
    if (A$row1[1] > A$row2[1]) {
      m <- A$row1[1] %/% A$row2[1]
      A$row1 <- A$row1 - m * A$row2
    } else {
      m <- A$row2[1] %/% A$row1[1]
      A$row2 <- A$row2 - m * A$row1
    }
  }
  if (A$row1[1] == 0) {
    g <- A$row2
  } else {
    g <- A$row1
  }
  g[2:3] <- sign_ab * g[2:3]
  return(g)
}


# there is actually much simpler solution - create a vector first and append
# a dim attribute to it
extGCD64 <- function(a, b) {
  sign_ab <- sign(c(a, b))
  A <- c(abs(c(a, b)), 1, 0, 0, 1)
  dim(A) <- c(2, 3)
  while (A[1, 1] * A[2, 1] != 0) {
    if (A[1, 1] > A[2, 1]) {
      m <- A[1, 1]%/%A[2, 1]
      A[1, ] <- A[1, ] - m * A[2, ]
    }
    else {
      m <- A[2, 1]%/%A[1, 1]
      A[2, ] <- A[2, ] - m * A[1, ]
    }
  }
  if (A[1, 1] == 0) 
    g <- A[2, ]
  else g <- A[1, ]
  g[2:3] <- sign_ab * g[2:3]
  return(g)
}

chinese64(as.integer64(c(0, 13-2, 19-3)), as.integer64(c(17,13,19)))
chinese64(as.integer64(c(0, 6, 57, 58)), as.integer64(c(67,7,59,61)))
chinese64(as.integer64(c(0, 5, 56, 57)), as.integer64(c(67,7,59,61)))
chinese64(as.integer64(c(0, 6, 56, 57)), as.integer64(c(67,7,59,61)))
chinese64(as.integer64(c(0, 36, 45, 1886)), as.integer64(c(1789,37,47,1889)))


chinese64(as.integer64((buses - ts) %% buses), as.integer64(buses))
# integer64
# [1] 1001569619313439
# success!
