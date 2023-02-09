

# 1 -----------------------------------------------------------------------

# relative position of head vs tail
# 21 different positions - 0,0 plus five in each quarter
# but only 12 require tail movement
# we could just list them



get_moves <- function(moves) {
  head <- c(0,0)
  tail <- head
  tail_pos <- list("0_0" = TRUE)

  for (i in seq_along(moves)) {
    move <- moves[[i]]
    move <- strsplit(move, " ")[[1]]
    if (move[1] == "R") {
      direction <- c(1, 0)
    } else if (move[1] == "D") {
      direction <- c(0, -1)
    } else if (move[1] == "L") {
      direction <- c(-1, 0)
    } else if (move[1] == "U") {
      direction <- c(0, 1)
    }

    steps <- as.integer(move[2])

    for (j in seq_len(steps)) {
      head <- head + direction

      if (any(abs(head) == 2)) {
        tail <- tail + sign(head)
        tail_pos[paste(tail, collapse = "_")] <- TRUE
        head <- as.integer(abs(head) == 2) * sign(head)
      }
    }
  }
  tail_pos
}

moves_test <- readLines("input09_test.txt")
length(get_moves(moves_test))



moves <- readLines("input09.txt")
length(get_moves(moves))
# 6212




# 2 -----------------------------------------------------------------------

# now we need to keep track of ten knots - head plus nine knots
# we move head and then all knots one by one depending on the previous one
# we need to keep relative position of every knot in relation to the next one
# as for exact positions - we are interested only in a tail


get_moves2 <- function(moves) {

  knots <- rep(list(c(0,0)), 10)
  tail_pos <- list("0_0" = TRUE)

  for (i in seq_along(moves)) {
    move <- moves[[i]]
    move <- strsplit(move, " ")[[1]]
    if (move[1] == "R") {
      direction <- c(1, 0)
    } else if (move[1] == "D") {
      direction <- c(0, -1)
    } else if (move[1] == "L") {
      direction <- c(-1, 0)
    } else if (move[1] == "U") {
      direction <- c(0, 1)
    }

    steps <- as.integer(move[2])

    for (j in seq_len(steps)) {

      knots[[1]] <- knots[[1]] + direction

      for (k in 1:9) {
        if (any(abs(knots[[k]]) == 2)) {
          knots[[k + 1]] <- knots[[k + 1]] + sign(knots[[k]])
          knots[[k]] <- as.integer(abs(knots[[k]]) == 2) * sign(knots[[k]])
        }
      }
      tail_pos[paste(knots[[10]], collapse = "_")] <- TRUE

    }
  }
  tail_pos
}


moves_test <- readLines("input09_test.txt")
length(get_moves2(moves_test))

moves_test2 <- readLines("input09_test2.txt")
length(get_moves2(moves_test2))

moves <- readLines("input09.txt")
length(get_moves2(moves))
# 2522
