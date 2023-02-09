



# 1 -----------------------------------------------------------------------

signal <- function(program) {
  cycle <- 1
  x <- 1
  signal <- 0

  for (i in seq_along(program)) {
    if (grepl("noop", program[[i]])) {
      cycle <- cycle + 1
      if ((cycle - 20) %% 40 == 0) {
        signal <- signal + cycle * x
      }

    } else {
      v <- as.integer(gsub("addx ", "", program[[i]]))
      cycle <- cycle + 2
      if ((cycle - 20) %% 40 == 0) {
        x <- x + v
        signal <- signal + cycle * x
      } else if ((cycle - 20) %% 40 == 1) {
        signal <- signal + (cycle - 1) * x
        x <- x + v
      } else {
        x <- x + v
      }

      # alternative, with cycles made one by one
      # cycle <- cycle + 1
      # if ((cycle - 20) %% 40 == 0) signal <- signal + cycle * x
      # cycle <- cycle + 1
      # x <- x + v
      # if ((cycle - 20) %% 40 == 0) signal <- signal + cycle * x
    }
  }
  signal
}


program_test <- readLines("input10_test.txt")
signal(program_test)


program <- readLines("input10.txt")
signal(program)




# 2 -----------------------------------------------------------------------



pixels <- function(program) {
  cycle <- 1
  x <- 1
  position <- 0
  pixels <- logical(length(program) * 2)

  for (i in seq_along(program)) {
    if (position %in% (x + -1:1)) pixels[cycle] <- TRUE

    if (grepl("noop", program[[i]])) {
      cycle <- cycle + 1

    } else {
      v <- as.integer(gsub("addx ", "", program[[i]]))
      cycle <- cycle + 1
      position <- (cycle - 1) %% 40
      if (position %in% (x + -1:1)) pixels[cycle] <- TRUE
      cycle <- cycle + 1
      x <- x + v
    }
    position <- (cycle - 1) %% 40
  }
  pixels <- head(pixels, cycle - 1)
  pixels <- ifelse(pixels, "#", ".")
  paste(apply(t(matrix(pixels, nrow = 40)), 1, paste, collapse = ""), collapse = "\n")
}


program_test <- readLines("input10_test.txt")
cat(pixels(program_test))


program <- readLines("input10.txt")
cat(pixels(program))
# ####.#..#..##..###..#..#..##..###..#..#.
# ...#.#.#..#..#.#..#.#.#..#..#.#..#.#.#..
# ..#..##...#....#..#.##...#....#..#.##...
# .#...#.#..#.##.###..#.#..#.##.###..#.#..
# #....#.#..#..#.#.#..#.#..#..#.#.#..#.#..
# ####.#..#..###.#..#.#..#..###.#..#.#..#.
# ZKGRKGRK
