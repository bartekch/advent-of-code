

read_input <- function(path) {
  d <- readLines(path)
  i <- which(d == "")

  stack <- head(d, i - 1)
  stack <- strsplit(stack, "")

  # indices of crates IDs in lines
  s_i <- which(tail(stack, 1)[[1]] != " ")
  stack <- head(stack, -1)


  stack <- lapply(s_i, function(column) {
    cr <- sapply(rev(seq_along(stack)), function(row) stack[[row]][column])
    cr[cr != " "]
  })

  moves <- tail(d, -i)
  moves <- strsplit(moves, "(move )|( from )|( to )") |>
    lapply(function(x) as.integer(x[x != ""]))

  list(moves = moves, stack = stack)
}





# 1 -----------------------------------------------------------------------

cargo <- read_input("input05.txt")

single_move <- function(stack, move) {
  crates <- tail(stack[[move[2]]], move[1])
  stack[[move[2]]] <- head(stack[[move[2]]], -move[1])
  stack[[move[3]]] <- c(stack[[move[3]]], rev(crates))
  stack
}

stack <- cargo$stack
for (i in seq_along(cargo$moves)) {
  stack <- single_move(stack, cargo$moves[[i]])
}
sapply(stack, tail, 1) |> paste(collapse = "")
# SHQWSRBDL



# 2 -----------------------------------------------------------------------

cargo <- read_input("input05.txt")

single_move <- function(stack, move) {
  crates <- tail(stack[[move[2]]], move[1])
  stack[[move[2]]] <- head(stack[[move[2]]], -move[1])
  stack[[move[3]]] <- c(stack[[move[3]]], crates)
  stack
}

stack <- cargo$stack
for (i in seq_along(cargo$moves)) {
  stack <- single_move(stack, cargo$moves[[i]])
}
sapply(stack, tail, 1) |> paste(collapse = "")
# CDTQZHBRS
