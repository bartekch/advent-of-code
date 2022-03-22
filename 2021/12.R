
read_input <- function(path) {
  paths <- path |>
    readLines() |>
    strsplit("-") |>
    lapply(function(x) data.frame(from = x[1], to = x[2]))
  paths <- do.call(rbind, paths)
  paths_rev <- data.frame(from = paths$to, to = paths$from)
  paths <- unique(rbind(paths, paths_rev))
  paths <- paths[paths$from != "end" & paths$to != "start", ]
  all_caves <- unique(paths$from, paths$to)
  small_caves <- sort(setdiff(all_caves[all_caves == tolower(all_caves)], c("start", "end")))

  list(
    paths = paths,
    small_caves = data.frame(cave = small_caves, is_visited = FALSE)
  )
}



# 1 -----------------------------------------------------------------------


find_paths <- function(caves, current_cave = "start") {
  # check stop condition
  if (current_cave == "end") return(list("end"))

  # find all endpoints from the current cave
  # visited small caves are omitted
  endpoints <- caves$paths$to[caves$paths$from == current_cave]
  endpoints <- setdiff(endpoints, caves$small_caves$cave[caves$small_caves$is_visited])

  if (length(endpoints) == 0) return(NULL)

  paths <- lapply(endpoints, function(endpoint) {
    caves_step <- caves
    caves_step$small_caves$is_visited[caves_step$small_caves$cave == endpoint] <- TRUE
    find_paths(caves = caves_step, current_cave = endpoint)
  })
  paths <- do.call(c, paths)

  # remove NULLs
  paths <- Filter(Negate(is.null), paths)

  # add current_cave at the start
  paths <- lapply(paths, function(path) c(current_cave, path))

  paths
}


test1 <- read_input("input12_test1.txt")
length(find_paths(test1))

test2 <- read_input("input12_test2.txt")
length(find_paths(test2))

test3 <- read_input("input12_test3.txt")
length(find_paths(test3))



input <- read_input("input12.txt")
length(find_paths(input))
# 5958



# 2 -----------------------------------------------------------------------

# treat is_visited as integer variable counting number of visits

find_paths2 <- function(caves, current_cave = "start") {
  # check stop condition
  if (current_cave == "end") return(list("end"))

  # find all endpoints from the current cave
  # visited small caves are omitted
  endpoints <- caves$paths$to[caves$paths$from == current_cave]

  if (any(caves$small_caves$is_visited == 2)) {
    used_small <- caves$small_caves$cave[caves$small_caves$is_visited > 0]
    endpoints <- setdiff(endpoints, used_small)
  }

  if (length(endpoints) == 0) return(NULL)

  paths <- lapply(endpoints, function(endpoint) {
    caves_step <- caves
    ind <- caves_step$small_caves$cave == endpoint
    caves_step$small_caves$is_visited[ind] <- caves_step$small_caves$is_visited[ind] + 1
    find_paths2(caves = caves_step, current_cave = endpoint)
  })
  paths <- do.call(c, paths)

  # remove NULLs
  paths <- Filter(Negate(is.null), paths)

  # add current_cave at the start
  paths <- lapply(paths, function(path) c(current_cave, path))

  paths
}



test1 <- read_input("input12_test1.txt")
length(find_paths2(test1))

test2 <- read_input("input12_test2.txt")
length(find_paths2(test2))

test3 <- read_input("input12_test3.txt")
length(find_paths2(test3))



input <- read_input("input12.txt")
length(find_paths2(input))
# 150426
