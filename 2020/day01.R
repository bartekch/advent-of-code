

# Part 1 ------------------------------------------------------------------

# Find the two entries that sum to 2020; what do you get if you multiply them together?


numbers <- as.integer(readLines("inputs/input01.txt"))


all_numbers <- expand.grid(x = numbers, y = numbers)
all_numbers <- all_numbers[all_numbers$x < all_numbers$y,]
res <- all_numbers[all_numbers$x + all_numbers$y == 2020,]
prod(res)

# 1016619



# Part 2 ------------------------------------------------------------------


# In your expense report, what is the product of the three entries that sum to 2020?
all_numbers <- expand.grid(x = numbers, y = numbers, z = numbers)
all_numbers <- all_numbers[all_numbers$x < all_numbers$y,]
all_numbers <- all_numbers[all_numbers$y < all_numbers$z,]
res <- all_numbers[all_numbers$x + all_numbers$y + all_numbers$z == 2020,]
prod(res)

# 218767230
