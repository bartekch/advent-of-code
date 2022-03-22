
# 1 -----------------------------------------------------------------------

input <- do.call(rbind, lapply(strsplit(readLines("input03.txt"), ""), as.integer))

gamma_b <- round(colSums(input) / nrow(input))
gamma <- strtoi(paste(gamma_b, collapse = ""), base = 2)
# 284

epsilon <- 2^ncol(input) - gamma - 1
# 3811
# could also be
# strtoi(paste(1 - gamma_b, collapse = ""), base = 2)

gamma * epsilon



# 2 -----------------------------------------------------------------------


input <- do.call(rbind, lapply(strsplit(readLines("input03.txt"), ""), as.integer))

# test input
# input <- strsplit("00100
# 11110
# 10110
# 10111
# 10101
# 01111
# 00111
# 11100
# 10000
# 11001
# 00010
# 01010", "\n")[[1]]
# input <- do.call(rbind, lapply(strsplit(input, ""), as.integer))


# oxygen
oxygen <- input
for (i in 1:ncol(oxygen)) {
  if (nrow(oxygen) <= 1) break
  # 0.5 is rounded to 0!
  leading_bit <- floor(sum(oxygen[, i]) / nrow(oxygen) + 0.5)
  oxygen <- oxygen[oxygen[, i] == leading_bit, , drop = FALSE]
}
oxygen <- strtoi(paste(oxygen[1, ], collapse = ""), base = 2)

# co
co <- input
for (i in 1:ncol(co)) {
  if (nrow(co) <= 1) break
  leading_bit <- 1 - floor(sum(co[, i]) / nrow(co) + 0.5)
  co <- co[co[, i] == leading_bit, , drop = FALSE]
}
co <- strtoi(paste(co[1, ], collapse = ""), base = 2)


#
oxygen * co
