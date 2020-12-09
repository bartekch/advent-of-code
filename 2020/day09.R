
# Part 1 ------------------------------------------------------------------

# The first step of attacking the weakness in the XMAS data is to find the first number in the list (after the preamble) which is not the sum of two of the 25 numbers before it. 
# What is the first number that does not have this property?


numbers <- as.numeric(readLines("2020/inputs/input09.txt"))

# we could go brute force and calculate all the possibles sums at every step


possible_sums <- outer(numbers[1:25], numbers[1:25], `+`)

for (i in 26:length(numbers)) {
  if (!numbers[i] %in% possible_sums) {
    break
  }
  
  possible_sums <- outer(numbers[(i-24):i], numbers[(i-24):i], `+`)
}
i
# 641
numbers[i]
# 1309761972



# Part 2 ------------------------------------------------------------------

# What is the encryption weakness in your XMAS-encrypted list of numbers?

# do we need to check only the numbers before the invalid number?
# it is not clear, let's check it first

# again, brute force - in a loop starting from the first number we calculate cumulative sums
# and check whether we hit out number

invalid_number_pos <- 641
invalid_number <- numbers[invalid_number_pos]


for (i in 1:(invalid_number_pos-1)) {
  cumsums <- cumsum(numbers[i:(invalid_number_pos-1)])
  tmp <- head(which(cumsums == invalid_number), 1)
  if (length(tmp)) {
    print(i)
    print(i + tmp - 1)
    res <- numbers[i:(i+tmp-1)]
    print(res)
    print(min(res) + max(res))
    break
  }
}
# [1] 525
# [1] 541
# [1]  55940665  55849911  57245695 101821668 116099069  63130797  64021846  68383164  67965922
# [10]  65586219  66869977  67147986  94576426  72844007  95999227  74139472 122139921
# [1] 177989832


