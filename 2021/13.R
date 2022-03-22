

read_input <- function(path) {
  l <- readLines(path)
  linebreak <- which(l == "")

  dots <- head(l, linebreak - 1)
  dots <- lapply(strsplit(dots, ","), function(d) data.frame(x = as.integer(d[1]), y = as.integer(d[2])))
  dots <- do.call(rbind, dots)

  folds <- tail(l, -linebreak)
  folds <- gsub("fold along ", "", folds)
  folds <- lapply(strsplit(folds, "="), function(f) data.frame(axis = f[1], line = as.integer(f[2])))
  folds <- do.call(rbind, folds)

  list(dots = dots, folds = folds)
}


plot_dots <- function(dots) {
  dots$x <- dots$x + 1
  dots$y <- dots$y + 1
  out <- matrix(".", nrow = max(dots$x), ncol = max(dots$y))
  out[as.matrix(dots)] <- "#"
  cat(paste(apply(t(out), 1, paste, collapse = ""), collapse = "\n"))
  invisible()
}




# 1 -----------------------------------------------------------------------


fold_once <- function(dots, fold) {

  if (fold$axis == "x") {
    dots_to_fold <- dots[dots$x > fold$line, ]
    dots_rest <- dots[dots$x < fold$line, ]
    dots_to_fold$x <- fold$line - abs(dots_to_fold$x - fold$line)
  } else {
    dots_to_fold <- dots[dots$y > fold$line, ]
    dots_rest <- dots[dots$y < fold$line, ]
    dots_to_fold$y <- fold$line - abs(dots_to_fold$y - fold$line)
  }

  unique(rbind(dots_rest, dots_to_fold))
}


test <- read_input("input13_test.txt")
test_f1 <- fold_once(test$dots, test$folds[1, ])
test_f2 <- fold_once(test_f1, test$folds[2, ])
plot_dots(test_f2)
nrow(test_f2)


input <- read_input("input13.txt")
input_f1 <- fold_once(input$dots, head(input$folds, 1))
nrow(input_f1)




# 2 -----------------------------------------------------------------------

input <- read_input("input13.txt")
dots <- input$dots
for (i in seq_len(nrow(input$folds))) {
  dots <- fold_once(dots, input$folds[i, ])
}
nrow(dots)
# 98
plot_dots(dots)
# ..##.###..####.###..#.....##..#..#.#..#
# ...#.#..#....#.#..#.#....#..#.#.#..#..#
# ...#.#..#...#..###..#....#....##...####
# ...#.###...#...#..#.#....#.##.#.#..#..#
# #..#.#.#..#....#..#.#....#..#.#.#..#..#
# .##..#..#.####.###..####..###.#..#.#..#

# JRZBLGKH
