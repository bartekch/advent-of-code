
# Part 1 ------------------------------------------------------------------

# Starting at the top-left corner of your map and following a slope of right 3 and down 1, how many trees would you encounter?


map <- readLines("inputs/input03.txt")

# create a matrix with trees
map <- do.call(rbind, strsplit(map, ""))

# we need to widen a map so we could reach a bottom
(nrow(map) * 3) %/% ncol(map) + 1
# or we could loop through it!

# find a positions of trees
trees <- which(map == "#", arr.ind = TRUE)
trees <- setNames(as.data.frame(trees), c("row", "col"))


# create our path
our_slope <- data.frame(row = seq_len(nrow(map)),
                        col = seq(1, by = 3, length.out = nrow(map)) %% ncol(map))
# change 0 to 31
our_slope$col[our_slope$col == 0] <- 31


# merge to find tree
nrow(merge(our_slope, trees, by = c("row", "col")))

# 169



# Part 2 ------------------------------------------------------------------

# Right 1, down 1.
# Right 3, down 1. (This is the slope you already checked.)
# Right 5, down 1.
# Right 7, down 1.
# Right 1, down 2.
#
# What do you get if you multiply together the number of trees encountered on each of the listed slopes?

slopes_to_check <- data.frame(down = c(1, 1, 1, 1, 2), right = c(1, 3, 5, 7, 1))

hits <- sapply(seq_len(nrow(slopes_to_check)), function(i) {
  # create our path
  our_slope <- data.frame(
    row = seq(1, by = slopes_to_check$down[i], length.out = nrow(map)),
    col = seq(1, by = slopes_to_check$right[i], length.out = nrow(map)) %% ncol(map)
  )
  # change 0 to 31
  our_slope$col[our_slope$col == 0] <- 31

  # merge to find tree
  nrow(merge(our_slope, trees, by = c("row", "col")))
})
hits
prod(hits)
# 7560370818
