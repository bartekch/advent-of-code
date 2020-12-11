
# Part 1 ------------------------------------------------------------------


# Simulate your seating area by applying the seating rules repeatedly until no seats change state. How many seats end up occupied? 

# If a seat is empty (L) and there are no occupied seats adjacent to it, the seat becomes occupied.
# If a seat is occupied (#) and four or more seats adjacent to it are also occupied, the seat becomes empty.
#   Otherwise, the seat's state does not change.


seat_layout <- readLines("2020/inputs/input11.txt")

# we could work with matrix and sum logical matrices shifted in every direction
# this code could be optimized to use excusively logicam matrices

# example
# seat_layout <- trimws(unlist(strsplit(
#   "L.LL.LL.LL
#   LLLLLLL.LL
#   L.L.L..L..
#   LLLL.LL.LL
#   L.LL.LL.LL
#   L.LLLLL.LL
#   ..L.L.....
#   LLLLLLLLLL
#   L.LLLLLL.L
#   L.LLLLL.LL"
#   , "\n")))


seat_l_mat <- do.call(rbind, strsplit(seat_layout, ""))

# add floor around it
seat_l_mat <- rbind(".", seat_l_mat, ".")
seat_l_mat <- cbind(".", seat_l_mat, ".")

is_floor <- seat_l_mat == "."

# for each point, countthe number of occupied seata sround it
NR <- nrow(seat_l_mat)
NC <- ncol(seat_l_mat)

i <- 0
while(TRUE) {
  occupied_count <- 
    (seat_l_mat[1:(NR-2), 2:(NC-1)] == "#") + # up
    (seat_l_mat[3:NR, 2:(NC-1)] == "#") + # down
    
    (seat_l_mat[2:(NR-1), 1:(NC-2)] == "#") + # left
    (seat_l_mat[2:(NR-1), 3:NC] == "#") + # right
    
    (seat_l_mat[1:(NR-2), 1:(NC-2)] == "#") + # up left
    (seat_l_mat[1:(NR-2), 3:NC] == "#") + # up right
    (seat_l_mat[3:NR, 1:(NC-2)] == "#") + # down left
    (seat_l_mat[3:NR, 3:NC] == "#") # down right
  
  
  new_seat_l_mat <- seat_l_mat
  new_seat_l_mat[2:(NR-1), 2:(NC-1)][occupied_count == 0] <- "#"
  new_seat_l_mat[2:(NR-1), 2:(NC-1)][occupied_count >= 4] <- "L"
  new_seat_l_mat[is_floor] <- "."
  
  if (all(new_seat_l_mat == seat_l_mat)) break
  i <- i + 1
  seat_l_mat <- new_seat_l_mat
}
i
sum(seat_l_mat == "#")
# 2265



# Part 2 ------------------------------------------------------------------


# Now, instead of considering just the eight immediately adjacent seats, consider the first seat in each of those eight directions.

# Also, people seem to be more tolerant than you expected: it now takes five or more visible occupied seats for an occupied seat to become empty (rather than four or more from the previous rules). The other rules still apply: empty seats that see no occupied seats become occupied, seats matching no rule don't change, and floor never changes.

# Given the new visibility method and the rule change for occupied seats becoming empty, once equilibrium is reached, how many seats end up occupied?

# the previous approach doesn't work, since the adjacent seats are not the same for every seat
# the idea is to calculate once all the visible seats for every seat, even in a loop,
# and then work with this dataset

library("dplyr")

tmp <- gregexpr("L", seat_layout)
seats_pos <- do.call(
  rbind,
  lapply(
    seq_along(tmp),
    function(i) {
      data.frame(row = i,
                 col = tmp[[i]])
    }
  )
)

# now calculate visible seats
seats_vis <- lapply(seq_len(nrow(seats_pos)), function(i) {
  seat <- seats_pos[i,]
  
  up <- seats_pos %>% 
    filter(row < seat$row, col == seat$col) %>% 
    slice_max(row)
  
  down <- seats_pos %>% 
    filter(row > seat$row, col == seat$col) %>% 
    slice_min(row)
  
  left <- seats_pos %>% 
    filter(row == seat$row, col < seat$col) %>% 
    slice_max(col)
  
  right <- seats_pos %>% 
    filter(row == seat$row, col > seat$col) %>% 
    slice_min(col)
  
  
  up_diag_seats <- seats_pos %>% 
    filter((row - seat$row) == -(col - seat$col))
  
  up_right <- up_diag_seats %>% 
    filter(row > seat$row) %>% 
    slice_min(row)
  
  down_left <- up_diag_seats %>% 
    filter(row < seat$row) %>% 
    slice_max(row)
  
  
  down_diag_seats <- seats_pos %>% 
    filter((row - seat$row) == (col - seat$col))
  
  up_left <- down_diag_seats %>% 
    filter(row < seat$row) %>% 
    slice_max(row)
  
  down_right <- down_diag_seats %>% 
    filter(row > seat$row) %>% 
    slice_min(row)
  
  
  
  seat_vis <- rbind(up, down, left, right, up_left, up_right, down_left, down_right) %>% 
    rename(row_vis = row, col_vis = col)
  
  data.frame(seat, seat_vis, row.names = NULL)
})
seats_vis <- dplyr::bind_rows(seats_vis)



seats_pos$occupied <- FALSE
i <- 0

while(TRUE) {
  
  # count occupied vis seats for every seat
  occupied_n <- seats_vis %>% 
    inner_join(seats_pos, by = c("row_vis" = "row", "col_vis" = "col")) %>% 
    group_by(row, col) %>% 
    summarise(occupied_n = sum(occupied), .groups = "drop")
  
  # 
  new_seats_pos <- seats_pos %>% 
    left_join(occupied_n, by = c("row", "col")) %>%
    mutate(occupied = (occupied_n == 0) | (occupied_n < 5 & occupied)) %>% 
    select(-occupied_n)
  
  
  if (isTRUE(all.equal(seats_pos, new_seats_pos))) break
  
  seats_pos <- new_seats_pos
  i <- i + 1
}
i
# 85
sum(seats_pos$occupied)
# 2045
