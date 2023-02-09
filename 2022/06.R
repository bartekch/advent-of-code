



# 1 -----------------------------------------------------------------------

s <- readLines("input06.txt")
s <- strsplit(s, "")[[1]]
i <- 4
while (i < length(s)) {
  if (length(unique(s[i:(i-3)])) == 4) break
  i <- i + 1;
}
i
# 1850



# 2 -----------------------------------------------------------------------

i <- 14
while (i < length(s)) {
  if (length(unique(s[i:(i-13)])) == 14) break
  i <- i + 1;
}
i
# 2823
