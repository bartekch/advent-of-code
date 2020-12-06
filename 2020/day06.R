
# Part 1 ------------------------------------------------------------------

# For each group, count the number of questions to which anyone answered "yes". What is the sum of those counts?


answers <- readLines("inputs/input06.txt")

answer_breaks <- which(answers == "")
answer_starts <- c(1, answer_breaks + 1)
answer_ends <- c(answer_breaks - 1, length(answers))

answers_full <- sapply(seq_along(answer_starts), function(i) {
  paste(answers[answer_starts[i]:answer_ends[i]], collapse = "")
})


sum(sapply(strsplit(answers_full, ""), function(x) length(unique(x))))

# 6549



# Part 2 ------------------------------------------------------------------

# For each group, count the number of questions to which everyone answered "yes". What is the sum of those counts?

answers_full <- lapply(seq_along(answer_starts), function(i) {
  answers[answer_starts[i]:answer_ends[i]]
})

sum(sapply(answers_full, function(x) {
  length(Reduce(intersect, strsplit(x, "")))
}))

# 3466

