
create_dirs <- function(cmd) {
  dirs <- list("~" = list())

  current_path <- "~"
  i <- 1
  while (i <= length(cmd)) {
    cm <- cmd[[i]]
    # we proceed only commands in the main loop, other output should be proceeded
    # withing sub loops
    if (substring(cm, 1, 1) != "$") stop("incorrect parsing")

    if (substr(cm, 1, 4) == "$ cd") {
      if (cm == "$ cd /") {
        current_path <- "~"
      } else if (cm == "$ cd .." && length(current_path) > 1) {
        current_path <- head(current_path, -1)
      } else {
        new_dir <- substr(cm, 6, nchar(cm))
        current_path <- c(current_path, new_dir)
        cp <- paste(current_path, collapse = "/")
        if (is.null(dirs[[cp]])) dirs[[cp]] <- list()
      }
      i <- i + 1

    } else {
      # ls
      i <- i + 1
      cp <- paste(current_path, collapse = "/")
      while (i <= length(cmd) && substring(cmd[[i]], 1, 1) != "$") {
        cm <- cmd[[i]]
        # dirs are added to the list while changing directories, ignore now
        if (substr(cm, 1, 3) != "dir") {
          f <- strsplit(cm, " ")[[1]]
          dirs[[cp]]$files <- c(dirs[[cp]]$files, setNames(as.numeric(f[1]), f[2]))
        }
        i <- i + 1
      }
    }

  }
  dirs
}



calculate_sizes <- function(dirs, current = "~", sizes = setNames(rep(NA_real_, length(dirs)), names(dirs))) {
  if (is.na(sizes[[current]])) {
    s_files <- sum(dirs[[current]]$files)
    s_dirs <- 0

    for (subdir in grep(paste0("^", current, "/[a-z]+$"), names(dirs), value = TRUE)) {
      sizes <- calculate_sizes(dirs, subdir, sizes)
      s_dirs <- s_dirs + sizes[[subdir]]
    }

    sizes[[current]] <- s_files + s_dirs
  }
  sizes
}



# 1 -----------------------------------------------------------------------


dirs_test <- create_dirs(readLines("input07_test.txt"))
sizes_test <- calculate_sizes(dirs_test)
sum(sizes_test[sizes_test <= 100000])



dirs <- create_dirs(readLines("input07.txt"))
sizes <- calculate_sizes(dirs)
sum(sizes[sizes <= 100000])
# 1648397



# 2 -----------------------------------------------------------------------

total_space <- 70000000
required_space <- 30000000
current_free_space <- total_space - sizes[["~"]]
to_delete <- required_space - current_free_space
min(sizes[sizes >= to_delete])
# 1815525







# alternative - using nested lists and pluck ------------------------------
# nesting indexing is cumbersome - but we could use purrr::pluck for it,
# as we could pass list of consecutive indices

create_dirs2 <- function(cmd) {
  # one external list so the root is dirs[[1]]
  dirs <- list("~" = list())

  current_path <- "~"
  i <- 1
  while (i <= length(cmd)) {
    cm <- cmd[[i]]
    # we proceed only commands in the main loop, other output should be proceeded
    # withing sub loops
    if (substring(cm, 1, 1) != "$") stop("incorrect parsing")

    if (substr(cm, 1, 4) == "$ cd") {
      if (cm == "$ cd /") {
        current_path <- "~"
      } else if (cm == "$ cd .." && length(current_path) > 1) {
        current_path <- head(current_path, -1)
      } else {
        new_dir <- substr(cm, 6, nchar(cm))

        new_current_path <- c(current_path, new_dir)

        if (is.null(purrr::pluck(dirs, !!!as.list(new_current_path)))) {
          purrr::pluck(dirs, !!!as.list(current_path)) <- c(
            purrr::pluck(dirs, !!!as.list(current_path)),
            setNames(list(list()), new_dir)
          )
        }
        current_path <- new_current_path
      }
      i <- i + 1

    } else {
      # ls
      i <- i + 1
      files <- numeric()
      while (i <= length(cmd) && substring(cmd[[i]], 1, 1) != "$") {
        cm <- cmd[[i]]
        # dirs are added to the list while changing directories, ignore now
        if (substr(cm, 1, 3) != "dir") {
          f <- strsplit(cm, " ")[[1]]
          files <- c(files, setNames(as.numeric(f[1]), f[2]))
        }
        i <- i + 1
      }
      purrr::pluck(dirs, !!!as.list(current_path)) <- c(
        purrr::pluck(dirs, !!!as.list(current_path)),
        as.list(files)
      )
    }

  }
  dirs
}


# appending sizes as attributes
# other option is to have size as an element of the list
# however than we would need to exclude it from directories list every time
# we could have directories in another list, but then indexing would be cumbersome
calculate_sizes2 <- function(dirs) {
  s <- 0
  for (i in seq_along(dirs)) {
    if (is.list(dirs[[i]])) {
      dirs[[i]] <- calculate_sizes2(dirs[[i]])
      s <- s + attr(dirs[[i]], "size")
    } else {
      s <- s + dirs[[i]]
    }
  }
  attr(dirs, "size") <- s
  dirs
}


find_sum <- function(dirs) {
  total <- 0
  if (attr(dirs, "size") <= 100000) total <- total + attr(dirs, "size")

  for (i in seq_along(dirs)) {
    if (is.list(dirs[[i]])) {
      total <- total + find_sum(dirs[[i]])
    }
  }
  total
}


dirs_test <- create_dirs2(readLines("input07_test.txt"))
dirs_test <- calculate_sizes2(dirs_test)
find_sum(dirs_test)


dirs <- create_dirs2(readLines("input07.txt"))
dirs <- calculate_sizes2(dirs)
find_sum(dirs)


total_space <- 70000000
required_space <- 30000000
current_free_space <- total_space - attr(dirs, "size")
to_delete <- required_space - current_free_space


find_min <- function(dirs, mins = attr(dirs, "size")) {
  cs <- attr(dirs, "size")
  if (cs >= to_delete && cs < mins) mins <- cs

  for (i in seq_along(dirs)) {
    if (is.list(dirs[[i]])) {
      mins <- find_min(dirs[[i]], mins)
    }
  }
  mins
}

find_min(dirs)




microbenchmark::microbenchmark(
  times = 10,
  {
    dirs <- create_dirs(readLines("input07.txt"))
    sizes <- calculate_sizes(dirs)
    total_space <- 70000000
    required_space <- 30000000
    current_free_space <- total_space - sizes[["~"]]
    to_delete <- required_space - current_free_space
    min(sizes[sizes >= to_delete])
  },
  {
    dirs <- create_dirs2(readLines("input07.txt"))
    dirs <- calculate_sizes2(dirs)
    total_space <- 70000000
    required_space <- 30000000
    current_free_space <- total_space - attr(dirs, "size")
    to_delete <- required_space - current_free_space
    find_min(dirs)
  }
)

# first method faster anyway
