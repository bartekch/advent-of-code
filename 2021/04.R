

parse_input <- function(path) {
  l <- readLines(path)

  # first line indicate numbers
  numbers <- as.integer(strsplit(l[1], ",")[[1]])
  l <- l[-1]

  # boards are 5x5, separated by empty line, so split every 6 lines
  boards <- split(l, rep(seq_len(length(l)/6), each = 6))
  boards <- lapply(boards, function(b) {
    b <- b[-1]
    do.call(rbind, lapply(strsplit(trimws(b), "\\s+"), as.integer))
  })
  list(numbers = numbers, boards = boards)
}



# 1 -----------------------------------------------------------------------



play_bingo <- function(bingo) {
  numbers <- bingo$numbers
  boards <- bingo$boards

  # create logical matrices
  hits <- rep(list(matrix(FALSE, 5, 5)), times = length(boards))
  bingo_board <- 0
  for (i in seq_along(numbers)) {
    number <- numbers[i]

    # mark hits board by board
    for (j in seq_along(boards)) {
      hits[[j]] <- hits[[j]] | (boards[[j]] == number)
    }

    # check for hit
    for (j in seq_along(boards)) {
      if (any(rowSums(hits[[j]]) == 5) || any(colSums(hits[[j]]) == 5)) {
        bingo_board <- j
        break
      }
    }
    if (bingo_board > 0) break
  }

  c(bingo, list(bingo_board = bingo_board, hits = hits, last_number = number))
}

bingo <- parse_input("input04.txt")
result <- play_bingo(bingo)


sum(result$boards[[result$bingo_board]][!result$hits[[result$bingo_board]]]) * result$last_number
# 50008




# 2 -----------------------------------------------------------------------



play_bingo2 <- function(bingo) {
  numbers <- bingo$numbers
  boards <- bingo$boards

  # create logical matrices
  hits <- rep(list(matrix(FALSE, 5, 5)), times = length(boards))
  bingo_board <- 0
  bingo_hit <- logical(length(boards))

  for (i in seq_along(numbers)) {
    number <- numbers[i]

    # mark hits board by board
    for (j in seq_along(boards)[!bingo_hit]) {
      hits[[j]] <- hits[[j]] | (boards[[j]] == number)
    }

    # check for hit
    for (j in seq_along(boards)[!bingo_hit]) {
      if (any(rowSums(hits[[j]]) == 5) || any(colSums(hits[[j]]) == 5)) {
        bingo_hit[j] <- TRUE
        bingo_board <- j
      }
    }
    if (all(bingo_hit)) break
  }

  c(bingo, list(bingo_board = bingo_board, hits = hits, last_number = number))
}

bingo <- parse_input("input04_test.txt")
result <- play_bingo2(bingo)
sum(result$boards[[result$bingo_board]][!result$hits[[result$bingo_board]]]) * result$last_number


bingo <- parse_input("input04.txt")
result <- play_bingo2(bingo)
sum(result$boards[[result$bingo_board]][!result$hits[[result$bingo_board]]]) * result$last_number
# 17408
