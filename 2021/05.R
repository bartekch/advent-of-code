

parse_input <- function(path) {
  ll <- readLines(path)
  lines <- strsplit(ll, " -> ")
  lines <- lapply(lines, function(line) {
    tmp <- strsplit(line, ",")
    data.frame(
      x1 = as.integer(tmp[[1]][1]),
      y1 = as.integer(tmp[[1]][2]),
      x2 = as.integer(tmp[[2]][1]),
      y2 = as.integer(tmp[[2]][2])
    )
  })
  do.call(rbind, lines)
}



# 1 -----------------------------------------------------------------------

count_lines <- function(input) {
  # filter only vertical and horizontal
  input <- input[input$x1 == input$x2 | input$y1 == input$y2, ]

  # for each line generate list of points
  all_lines <- lapply(seq_len(nrow(input)), function(i) {
    line <- input[i, ]
    data.frame(
      x = seq(line$x1, line$x2),
      y = seq(line$y1, line$y2)
    )
  })
  all_lines <- do.call(rbind, all_lines)

  # count overlapping points
  all_lines$x_y <- paste(all_lines$x, all_lines$y)
  tt <- table(all_lines$x_y)
  sum(tt >= 2)
}

count_lines(parse_input("input05_test.txt"))

count_lines(parse_input("input05.txt"))
# 5698



# 2 -----------------------------------------------------------------------

count_lines2 <- function(input) {
  # for each line generate list of points
  all_lines <- lapply(seq_len(nrow(input)), function(i) {
    line <- input[i, ]
    data.frame(
      x = seq(line$x1, line$x2),
      y = seq(line$y1, line$y2)
    )
  })
  all_lines <- do.call(rbind, all_lines)

  # count overlapping points
  all_lines$x_y <- paste(all_lines$x, all_lines$y)
  tt <- table(all_lines$x_y)
  sum(tt >= 2)
}

count_lines2(parse_input("input05_test.txt"))

count_lines2(parse_input("input05.txt"))
# 15463
