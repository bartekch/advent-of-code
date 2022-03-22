
read_input <- function(path) {
  l <- readLines(path)
  lapply(l, parse_input)
}

parse_input <- function(string) {
  tree <- jsonlite::fromJSON(string, simplifyVector = FALSE)
  tree <- name_tree(tree)
  tree <- number_levels(tree)
  tree
}

name_tree <- function(tree) {
  if (!is.list(tree)) return(tree)
  names(tree) <- c("left", "right")
  tree$left <- Recall(tree$left)
  tree$right <- Recall(tree$right)
  tree
}

number_levels <- function(tree, level = 0) {
  if (!is.list(tree)) return(tree)
  tree$level <- level
  tree$left <- Recall(tree$left, level + 1)
  tree$right <- Recall(tree$right, level + 1)
  tree
}


collapse_tree <- function(tree) {
  if (is.list(tree)) {
    paste0("[", Recall(tree$left), ",", Recall(tree$right), "]")
  } else {
    as.character(tree)
  }
}

magnitude <- function(tree) {
  if (!is.list(tree)) {
    return(tree)
  }
  left <- magnitude(tree$left)
  right <- magnitude(tree$right)
  3 * left + 2 * right
}


add <- function(tree1, tree2) {
  res <- list(left = tree1, right = tree2)
  res <- number_levels(res)

  while (TRUE) {
    out <- explode(res)
    if (!is.null(out)) {
      res <- out
      next
    }

    out <- split(res)
    if (!is.null(out)) {
      res <- out
      next
    }
    break
  }
  res
}


explode <- function(tree) {
  if (!is.list(tree)) {
    return(NULL)
  } else {
    if (tree$level == 4) {
      # actual explosion
      # left to the left
      # right to the right
      # this pair is replaced by 0
      list(
        is_exploded = TRUE,
        add_to_rightmost = tree$left,
        add_to_leftmost = tree$right
      )

    } else {
      out <- Recall(tree$left)
      if (!is.null(out)) {
        if (isTRUE(out$is_exploded)) {
          # immediate child exploded
          tree$left <- 0
          tree$right <- add_to_leftmost(tree$right, out$add_to_leftmost)
          # pass this higher as there is nothing to add
          if (tree$level > 0) {
            tree$add_to_rightmost <- out$add_to_rightmost
          }

        } else if (!is.null(out$add_to_leftmost)) {
          # info about explosion passed from downstream
          tree$right <- add_to_leftmost(tree$right, out$add_to_leftmost)
          out$add_to_leftmost <- NULL
          tree$left <- out

        } else if (!is.null(out$add_to_rightmost)) {
          if (tree$level > 0) {
            tree$add_to_rightmost <- out$add_to_rightmost
          }
          out$add_to_rightmost <- NULL
          tree$left <- out

        } else {
          tree$left <- out
        }
        return(tree)
      }


      out <- Recall(tree$right)
      if (!is.null(out)) {
        if (isTRUE(out$is_exploded)) {
          # immediate child exploded
          tree$right <- 0
          tree$left <- add_to_rightmost(tree$left, out$add_to_rightmost)
          # pass this higher as there is nothing to add
          if (tree$level > 0) {
            tree$add_to_leftmost <- out$add_to_leftmost
          }

        } else if (!is.null(out$add_to_rightmost)) {
          # info about explosion passed from downstream
          tree$left <- add_to_rightmost(tree$left, out$add_to_rightmost)
          out$add_to_rightmost <- NULL
          tree$right <- out

        } else if (!is.null(out$add_to_leftmost)) {
          if (tree$level > 0) {
            tree$add_to_leftmost <- out$add_to_leftmost
          }
          out$add_to_leftmost <- NULL
          tree$right <- out

        } else {
          tree$right <- out
        }
        return(tree)
      }
    }

  }
}


add_to_leftmost <- function(tree, value) {
  if (!is.list(tree)) {
    return(tree + value)
  } else {
    tree$left <- Recall(tree$left, value)
    tree
  }
}

add_to_rightmost <- function(tree, value) {
  if (!is.list(tree)) {
    return(tree + value)
  } else {
    tree$right <- Recall(tree$right, value)
    tree
  }
}


split <- function(tree) {
  if (!is.list(tree)) {
    if (tree >= 10) {
      return(
        list(
          left = floor(tree / 2),
          right = ceiling(tree / 2)
        )
      )
    }

  } else {
    out <- Recall(tree$left)
    if (!is.null(out)) {
      out$level <- tree$level + 1
      tree$left <- out
      return(tree)
    }

    out <- Recall(tree$right)
    if (!is.null(out)) {
      out$level <- tree$level + 1
      tree$right <- out
      return(tree)
    }
  }
}


# tests
rr <- parse_input("[[1,2],[[3,4],5]]")
test <- read_input("input18_test.txt")
str(test[[1]])

magnitude(parse_input("[9,1]"))
magnitude(parse_input("[1,9]"))
magnitude(parse_input("[[9,1],[1,9]]"))
magnitude(parse_input("[[1,2],[[3,4],5]]"))
magnitude(parse_input("[[[[0,7],4],[[7,8],[6,0]]],[8,1]]"))
magnitude(parse_input("[[[[1,1],[2,2]],[3,3]],[4,4]]"))
magnitude(parse_input("[[[[3,0],[5,3]],[4,4]],[5,5]]"))
magnitude(parse_input("[[[[5,0],[7,4]],[5,5]],[6,6]]"))
magnitude(parse_input("[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]"))

parse_input("[[[[[9,8],1],2],3],4]")

split(parse_input("[11,1]"))
test <- parse_input("[[[[0,7],4],[15,[0,13]]],[1,1]]")
collapse_tree(split(split(test)))

add_to_leftmost(parse_input("[1,2]"), 3)
add_to_leftmost(parse_input("[[1,2],2]"), 3)
add_to_leftmost(parse_input("[[[1,2],3],4]"), 3)
collapse_tree(add_to_rightmost(parse_input("[4,[3,[1,2]]]"), 3))

collapse_tree(explode(parse_input("[1,[2,[3,[4,[5,6]]]]]")))
test <- parse_input("[[[[[9,8],1],2],3],4]")
collapse_tree(explode(test))
collapse_tree(explode(parse_input("[7,[6,[5,[4,[3,2]]]]]")))
collapse_tree(explode(parse_input("[[6,[5,[4,[3,2]]]],1]")))
collapse_tree(explode(parse_input("[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]")))
collapse_tree(explode(parse_input("[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]")))

add(
  parse_input("[[[[4,3],4],4],[7,[[8,4],9]]]"),
  parse_input("[1,1]")
) |> collapse_tree()


test <- lapply(c("[1,1]","[2,2]","[3,3]","[4,4]"), parse_input)
collapse_tree(Reduce(add, test))

test <- lapply(c("[1,1]","[2,2]","[3,3]","[4,4]","[5,5]"), parse_input)
collapse_tree(Reduce(add, test))

test <- lapply(c("[1,1]","[2,2]","[3,3]","[4,4]","[5,5]","[6,6]"), parse_input)
collapse_tree(Reduce(add, test))



test <- read_input("input18_test.txt")
res <- Reduce(add, test)
collapse_tree(res)
magnitude(res)



# 1 -----------------------------------------------------------------------

input <- read_input("input18.txt")
res <- Reduce(add, input)
collapse_tree(res)
magnitude(res)
# 3051



# 2 -----------------------------------------------------------------------


find_highest <- function(numbers) {
  current_max <- 0
  for (i in seq_along(numbers)) {
    for (j in setdiff(seq_along(numbers), i)) {
      out <- magnitude(add(numbers[[i]], numbers[[j]]))
      if (out > current_max) current_max <- out
    }
  }
  current_max
}

test <- read_input("input18_test.txt")
find_highest(test)

input <- read_input("input18.txt")
find_highest(input)
# 4812
