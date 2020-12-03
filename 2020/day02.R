

# Part 1 ------------------------------------------------------------------

# Each line gives the password policy and then the password. The password policy indicates the lowest and highest number of times a given letter must appear for the password to be valid. For example, 1-3 a means that the password must contain a at least 1 time and at most 3 times.
#
# How many passwords are valid according to their policies?


passwords <- read.table("inputs/input02.txt", sep = " ", header = FALSE,
                        col.names = c("count", "letter", "password"), as.is = TRUE)
passwords$letter <- substr(passwords$letter, 1, 1)
cc <- do.call(rbind, strsplit(passwords$count, "-"))
passwords$count_l <- as.integer(cc[, 1])
passwords$count_u <- as.integer(cc[, 2])

passwords$count_true <- stringi::stri_count_fixed(passwords$password, passwords$letter)

passwords$is_valid <- passwords$count_l <= passwords$count_true & passwords$count_true <= passwords$count_u
sum(passwords$is_valid)
# 643



# Part 2 ------------------------------------------------------------------

# Each policy actually describes two positions in the password, where 1 means the first character, 2 means the second character, and so on. (Be careful; Toboggan Corporate Policies have no concept of "index zero"!) Exactly one of these positions must contain the given letter.

sum(xor(
  substr(passwords$password, passwords$count_l, passwords$count_l) == passwords$letter,
  substr(passwords$password, passwords$count_u, passwords$count_u) == passwords$letter
))

# 388

