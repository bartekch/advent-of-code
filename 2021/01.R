


# 1 -----------------------------------------------------------------------

input <- as.integer(readLines("input01.txt"))
sum(diff(input) > 0)
# 1766


# 2 -----------------------------------------------------------------------

triples <- head(input, -2) + tail(head(input, -1), -1) + tail(input, -2)
sum(diff(triples) > 0)
# 1797

# alternatively we could just compare every third number
sum(head(input, -3) < tail(input, - 3))
# 1797
