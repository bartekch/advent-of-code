
parse_input <- function(path) {
  strsplit(readLines(path), "")
}



# 1 -----------------------------------------------------------------------

is_corrupt <- function(chunks) {
  p_chunks <- ""
  broken_chunk_val <- 0

  for (i in seq_along(chunks)) {
    chunk <- chunks[i]
    if (chunk == ")") {
      last_chunk <- p_chunks[1]
      if (last_chunk != "(") {
        broken_chunk_val <- 3
        break
      } else {
        p_chunks <- p_chunks[-1]
      }

    } else if (chunk == "]") {
      last_chunk <- p_chunks[1]
      if (last_chunk != "[") {
        broken_chunk_val <- 57
        break
      } else {
        p_chunks <- p_chunks[-1]
      }

    } else if (chunk == "}") {
      last_chunk <- p_chunks[1]
      if (last_chunk != "{") {
        broken_chunk_val <- 1197
        break
      } else {
        p_chunks <- p_chunks[-1]
      }

    } else if (chunk == ">") {
      last_chunk <- p_chunks[1]
      if (last_chunk != "<") {
        broken_chunk_val <- 25137
        break
      } else {
        p_chunks <- p_chunks[-1]
      }

    } else {
      p_chunks <- append(p_chunks, chunk, 0)
    }
  }
  broken_chunk_val
}


sum(sapply(parse_input("input10_test.txt"), is_corrupt))
sum(sapply(parse_input("input10.txt"), is_corrupt))




# 2 -----------------------------------------------------------------------



complete_lines <- function(chunks) {
  # remove complete chunks () [] {} <> until there are no more left
  # or there are no more characters, then check what is the last one open
  # and assign this as closing bracket
  # we are assuming that lines are NOT corrupted, so we would never have
  # unmatching closing bracket just after opening one
  bracket_dict <- c("(" = ")", "[" = "]", "{" = "}", "<" = ">")
  o_c <- names(bracket_dict)
  closing_b <- character()

  while (TRUE) {
    if (length(chunks) == 0) break

    pairs <- (head(chunks, -1) %in% o_c) & (chunks[-1] %in% bracket_dict)
    if (any(pairs)) {
      # remove pairs
      chunks <- chunks[-c(which(pairs), which(pairs) + 1)]
    } else {
      last_bracket <- tail(chunks, 1)
      closing_b <- append(closing_b, bracket_dict[last_bracket])
      chunks <- head(chunks, -1)
    }
  }
  closing_b
}

count_points <- function(brackets) {
  score <- 0
  for (bracket in brackets) {
    score <- score * 5
    score <- score + c(")" = 1, "]" = 2, "}" = 3, ">" = 4)[bracket]
  }
  score
}

find_score <- function(path) {
  scores <- sapply(parse_input(path), function(chunks) {
    if (is_corrupt(chunks) > 0) {
      0
    } else {
      count_points(complete_lines(chunks))
    }
  })
  scores <- sort(scores[scores > 0])
  median(scores)
}


find_score("input10_test.txt")
find_score("input10.txt")
# 3662008566



# better approach taken from github

find_score2 <- function(input) {
  bracket_dict <- c("(" = ")", "[" = "]", "{" = "}", "<" = ">")
  bracket_point <- c(")" = 1, "]" = 2, "}" = 3, ">" = 4)
  scores <- integer()

  for (chunks in input) {
    correct <- TRUE
    p_chunks <- character(0)

    for (i in seq_along(chunks)) {
      chunk <- chunks[i]

      if (chunk %in% names(bracket_dict)) {
        p_chunks <- append(p_chunks, bracket_dict[chunk], 0)
      } else if (chunk == p_chunks[1]) {
        p_chunks <- p_chunks[-1]
      } else {
        correct <- FALSE
        break
      }
    }

    if (correct) {
      # calculate score
      score <- 0
      for (bracket in p_chunks) {
        score <- score * 5
        score <- score + bracket_point[bracket]
      }
      scores <- append(scores, score)
    }
  }

  median(scores)
}
find_score2(parse_input("input10_test.txt"))
find_score2(parse_input("input10.txt"))
