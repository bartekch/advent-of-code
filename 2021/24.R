

read_input <- function(path) {
  l <- readLines(path)
  out <- lapply(strsplit(l, " "), function(l) {
    if (length(l) == 2) {
      data.frame(action = l[1], x = l[2], y = NA)
    } else {
      data.frame(action = l[1], x = l[2], y = l[3])
    }
  })
  out <- do.call(rbind, out)
  out$yl <- out$y %in% c("w", "x", "y", "z")
  out$x <- match(out$x, c("w", "x", "y", "z"))
  out$y <- as.integer(ifelse(out$yl, match(out$y, c("w", "x", "y", "z")), out$y))
  out
}


split_input <- function(input) {
  ind <- which(input$action == "inp")
  l <- diff(c(ind, nrow(input) + 1))
  split(input, rep(seq_along(l), times = l))
}

make_actions_full <- function(actions, inputs, alu = integer(4)) {
  input_counter <- 1

  for (i in seq_len(nrow(actions))) {
    action <- actions[i, ]

    if (action$action == "inp") {
      alu[action$x] <- inputs[input_counter]
      input_counter <- input_counter + 1

    } else if (action$action == "add") {
      if (action$yl) {
        y <- alu[action$y]
      } else {
        y <- action$y
      }
      alu[action$x] <- alu[action$x] + y

    } else if (action$action == "mul") {
      if (action$yl) {
        y <- alu[action$y]
      } else {
        y <- action$y
      }
      alu[action$x] <- alu[action$x] * y

    } else if (action$action == "div") {
      if (action$yl) {
        y <- alu[action$y]
      } else {
        y <- action$y
      }
      alu[action$x] <- alu[action$x] %/% y

    } else if (action$action == "mod") {
      if (action$yl) {
        y <- alu[action$y]
      } else {
        y <- action$y
      }
      alu[action$x] <- alu[action$x] %% y

    } else if (action$action == "eql") {
      if (action$yl) {
        y <- alu[action$y]
      } else {
        y <- action$y
      }
      alu[action$x] <- as.integer(alu[action$x] == y)
    }
  }
  alu
}


test <- read_input("input24_test1.txt")
make_actions_full(test, 10)

test <- read_input("input24_test2.txt")
make_actions_full(test, c(1,3))

test <- read_input("input24_test3.txt")
make_actions_full(test, 1)
make_actions_full(test, 2)
make_actions_full(test, 3)
make_actions_full(test, 4)
make_actions_full(test, 5)
make_actions_full(test, 6)
make_actions_full(test, 7)
make_actions_full(test, 8)
make_actions_full(test, 9)




input <- read_input("input24.txt")
sinput <- split_input(input)



# input analysis ----------------------------------------------------------


# each chunk has the same length - 18
sapply(sinput, nrow)

which(!sapply(1:18, function(i) nrow(unique(input[seq(i, by = 18, length.out = 14),])) == 1))
# marked operations are identical in all chunks - so only three are NOT
#  1 * inp w
#  2 * mul x 0
#  3 * add x z
#  4 * mod x 26
#  5   div z 26  - either 1 or 26, but 1 doesn't change value!
#  6   add x -3  - several values and does affect logic - see further
#  7 * eql x w
#  8 * eql x 0
#  9 * mul y 0
# 10 * add y 25
# 11 * mul y x
# 12 * add y 1
# 13 * mul z y
# 14 * mul y 0
# 15 * add y w
# 16   add y 12 - several values, does not affect the overall logic
# 17 * mul y x
# 18 * add z y

# variable x is set to 0 every time, similarly y and w is input
# so basically only z from the previous section really matters

# variable actions
unique(input[seq(5, by = 18, length.out = 14),])
unique(input[seq(6, by = 18, length.out = 14),])
unique(input[seq(16, by = 18, length.out = 14),])

#
actions56 <- cbind(
  input[seq(5, by = 18, length.out = 14),],
  input[seq(6, by = 18, length.out = 14),]
)

actions56[actions56[, 3] == 1, ]
# every time action 5 is div 1, x is increased by value > 10

actions56[actions56[, 3] == 26, ]
# every time action 5 is div 26, x is decreased, in 5/7 case by at least 9


# working approach --------------------------------------------------------

# brute force in a loop, immediately after every step choose only
# max/min w value for every unique z value
# in div 26 steps this should cause solution space to remain more or less
# the same value
# while in div 1 it grows by the order of 9
# because there are seven div 1 steps, the space should be maximally
# of order 9^7 (9^8 before filtering), which should be manageable

# optimize action function based on input analysis
make_actions <- function(actions, w, z) {
  x <- z %% 26
  z <- z %/% actions$y[5]
  x <- x + actions$y[6]
  x <- x != w
  z <- z * (25 * x + 1)
  z <- z + x * (w + actions$y[16])
  z
}

# first column - total w, second column - last z
m <- matrix(0, ncol = 2)
for (chunk_no in 1:14) {
  nr <- nrow(m)
  message(Sys.time(), ", ", chunk_no, ", ", nr, ", ", sinput[[chunk_no]][5,"y"])
  new_m <- matrix(
    c(
      10 * rep(m[, 1], each = 9) + rep(1:9, times = nr), # new w
      rep(0, 9 * nr) # placeholder for new z
    ),
    ncol = 2
  )
  for (w in 1:9) {
    new_m[seq(w, by = 9, length.out = nr), 2] <- make_actions(sinput[[chunk_no]], w, m[, 2])
  }
  m <- new_m
  message(Sys.time(), ", ", chunk_no, ", ", nrow(m))

  zd <- duplicated(m[, 2], fromLast = TRUE)
  if (any(zd)) {
    # matrix is already sorted due to the way we make it
    m <- m[!zd, ]
    message(Sys.time(), ", ", chunk_no, ", ", nrow(m))
  }

}
format(m[which(m[,2] == 0),1], scientific = FALSE)
# 39924989499969
# control
make_actions_full(input, as.numeric(strsplit("39924989499969", "")[[1]]))

# minimalization
m <- matrix(0, ncol = 2)
for (chunk_no in 1:14) {
  nr <- nrow(m)
  message(Sys.time(), ", ", chunk_no, ", ", nr, ", ", sinput[[chunk_no]][5,"y"])
  new_m <- matrix(
    c(
      10 * rep(m[, 1], each = 9) + rep(1:9, times = nr), # new w
      rep(0, 9 * nr) # placeholder for new z
    ),
    ncol = 2
  )
  for (w in 1:9) {
    new_m[seq(w, by = 9, length.out = nr), 2] <- make_actions(sinput[[chunk_no]], w, m[, 2])
  }
  m <- new_m
  message(Sys.time(), ", ", chunk_no, ", ", nrow(m))

  zd <- duplicated(m[, 2], fromLast = FALSE)
  if (any(zd)) {
    # matrix is already sorted due to the way we make it
    m <- m[!zd, ]
    message(Sys.time(), ", ", chunk_no, ", ", nrow(m))
  }

}
format(m[which(m[,2] == 0),1], scientific = FALSE)
# 16811412161117
# control
make_actions_full(input, as.numeric(strsplit("16811412161117", "")[[1]]))


# parallelization probably could make it even faster, but it's still quite fast


# approach when we check unique inputs first - it doesn't matter
# for different z we get different pair of mod 26 and div 26, and we already
# check for unique zs










# theoretical approach ----------------------------------------------------

# inspired by https://selbydavid.com/2021/12/01/advent-2021/#day23


# key insights:
# - every time action 5 is div 26, x is decreased, in 5/7 case by at least 9, flag A
# - every time action 5 is div 1, x is increased by value > 10, flag B


# last chunk from top
#  1 input w -> could be 1:9
#  2 x' = 0 -> x == 0
#  3 x' = x + z0 -> x == z0
#  4 x' = x mod 26 -> x == z0 mod 26, at this stage x could have 26 different values 0:25
#  5 z' = z div dz -> A) z == z0 div 26
#                     B) z == z0 div 1 == z0

#  6 x' = x + dx -> A) x == x4 + dx, still 26 different values -dx : 22-dx
#                   B) x == x4 + dx, but dx > 10, so x > 10

#  7 x' = (x == w) -> A) x == (x6 == w) could have two values depending on w and z
#                     B) x == (x6 == w), but x6 > 10 and w < 10, so always 0!!
#  8 x' = (x == 0) -> x == ((x6 == w) == 0) == (x6 != w) because of the previous line this is basically swap 0<->1
#                     A) could be 0/1
#                     B) always 1

#  9 y' = 0 -> y == 0
# 10 y' = y + 25 -> y == 25
# 11 y' = y * x -> y == 25 * x8
# 12 y' = y + 1 -> y == 25 * x8 + 1
# 13 z' = z * y -> z == z5 * (25 * x8 + 1)
#                  A) x8 is 0:1, so this is either z5 or 26 * z5
#                  A) x8 is 1, so this is always 26 * z5

# 14 y' = 0 -> y == 0
# 15 y' = y + w ->  y == w -> 9 different values
# 16 y' = y + dy -> y == w + dy -> 9 different values
# 17 y' = y * x -> y == x8 * (w + dy)
#                  A) could be (w + dy) or 0
#                  B) always w + dy

# 18 z' = z + y -> z == z13 + x8 * (w + dy) == z5 * (25 * x8 + 1) + x8 * (w + dy)
#                  A) either 26 * z5 + w + dy OR z5
#                  B) 26 * z5 + w + dy == 26 * z0 + w + dy


# so for B) we ALWAYS have 26 * z0 + w + dy at then end - deterministic
# this could nest like that 26 * (26 * (26 * z0 + w + dy) + w + dy) + w + dy
# in every such action we are increasing z by order of 26
# what is important is that we start with action B, and there is seven such actions
# for B actions dx doesn't even matter!

# for A) the only thing causing nondeterministic behaviour is 6th action
# for A, assuming i layers 26 * ... * (26 * z0 + w_1 + dy_1) + ... + w_i + dy_i,
# we've got
#    x mod 26 = (w_i + dy_i)
#    z div 26 = 26 * ... * (26 * z0 + w_1 + dy_1) + ... + w_(i-1) + dy_(i-1)


# for A) z5 is decrased by the order or 26, so at the end we could have either
#    26 * z5 + w + dy - that means z is still on the same order of magnitude
#    z5 - order is decreased by 26
# there are also seven such actions

# at then end we want to get z = 0!!
# so we need as many decreasing steps as there are increasing, that is seven
# so all actions A) have to give x8 == 0, that means
# x6 == w
# x4 + dx == w
# (w_(i-1) + dy_(i-1)) + dx_i == w_i
# where i-1 is the index of the last B action
# this is the key equation!!!!
# PS it also means, that dy doesn't matter for A actions

# equation
# (w_(i-1) + dy_(i-1)) + dx_i == w_i
# we need to find pair w_(i_1), w_i, that satisfy the equation and
# maximise or minimise [w_(i-1), w_i] -> this is the same as max/min w_(i-1) only
# as w_i could be derived from w_(i-1)

stack <- list()
digits <- integer(14)
for (i in seq_along(sinput)) {
  # get action type
  type <- sinput[[i]][5, "y"]
  if (type == 26) {
    # A
    dx <- sinput[[i]][6, "y"]

    # get last value from the stack
    popped <- stack[[1]]
    stack <- stack[-1]
    dy <- popped$dy
    # (w_(i-1) + dy_(i-1)) + dx_i == w_i
    df <- expand.grid(wi = 1:9, wi1 = 1:9)
    df <- df[df$wi == df$wi1 + dy + dx, , drop = FALSE]
    df <- head(df[order(df$wi1, decreasing = TRUE), ], 1)
    # w_(i-1)
    digits[popped$i] <- df$wi1
    digits[i] <- df$wi

  } else {
    # B
    dy <- sinput[[i]][16, "y"]
    # add to stack
    stack <- c(list(list(dy = dy, i = i)), stack)

  }
}
paste(digits, collapse = "")
# 39924989499969



stack <- list()
digits <- integer(14)
for (i in seq_along(sinput)) {
  # get action type
  type <- sinput[[i]][5, "y"]
  if (type == 26) {
    # A
    dx <- sinput[[i]][6, "y"]

    # get last value from the stack
    popped <- stack[[1]]
    stack <- stack[-1]
    dy <- popped$dy
    # (w_(i-1) + dy_(i-1)) + dx_i == w_i
    df <- expand.grid(wi = 1:9, wi1 = 1:9)
    df <- df[df$wi == df$wi1 + dy + dx, , drop = FALSE]
    df <- head(df[order(df$wi1, decreasing = FALSE), ], 1)
    # w_(i-1)
    digits[popped$i] <- df$wi1
    digits[i] <- df$wi

  } else {
    # B
    dy <- sinput[[i]][16, "y"]
    # add to stack
    stack <- c(list(list(dy = dy, i = i)), stack)

  }
}
paste(digits, collapse = "")
# 16811412161117


# same values!
