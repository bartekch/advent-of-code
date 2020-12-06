
# Part 1 ------------------------------------------------------------------

# What is the highest seat ID on a boarding pass?


bps <- readLines("inputs/input05.txt")

bps <- data.frame(row = substr(bps, 1, 7),
                  col = substr(bps, 8, 10))

bps$row <- gsub("F", "0", bps$row)
bps$row <- gsub("B", "1", bps$row)
bps$row_int <- strtoi(bps$row, base = 2)

bps$col <- gsub("L", "0", bps$col)
bps$col <- gsub("R", "1", bps$col)
bps$col_int <- strtoi(bps$col, base = 2)


bps$id <- bps$row_int * 8 + bps$col_int

max(bps$id)



# Part 2 ------------------------------------------------------------------

# Your seat wasn't at the very front or back, though; the seats with IDs +1 and -1 from yours will be in your list.
#
# What is the ID of your seat?

# we need to find those numbers, that are NOT on the list, but +1 and -1 are

all_ids <- 1:max(bps$id)
all_ids <- setdiff(all_ids, bps$id)
all_ids <- all_ids[(all_ids + 1) %in% bps$id]
all_ids <- all_ids[(all_ids - 1) %in% bps$id]
all_ids
# 669
