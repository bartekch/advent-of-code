

read_input <- function(path) {
  e <- readLines(path)
  e <- do.call(rbind, strsplit(e, ""))
  which(e == "#", arr.ind = TRUE)
}


make_matrix <- function(e) {
  e[,1] <- e[,1] - min(e[,1]) + 1
  e[,2] <- e[,2] - min(e[,2]) + 1
  m <- matrix(".", nrow = max(e[,1]), ncol = max(e[,2]))
  m[e] <- "#"
  m
}

plot_elves <- function(e) {
  apply(make_matrix(e), 1, paste, collapse = "") |> paste(collapse = "\n") |> cat()
  invisible(e)
}



move_elves <- function(e, N = 10) {

  all_directions <- list(
    nw = c(-1, -1),
    nn = c(-1, 0),
    ne = c(-1, 1),
    ee = c(0, 1),
    se = c(1, 1),
    ss = c(1, 0),
    sw = c(1, -1),
    ww = c(0, -1)
  )
  directions <- list(
    north = c("nw", "nn", "ne"),
    south = c("sw", "ss", "se"),
    west = c("nw", "ww", "sw"),
    east = c("ne", "ee", "se")
  )


  for (i in seq_len(N)) {
    # find proposed moves
    proposed_moves <- lapply(seq_len(nrow(e)), function(j) {
      elf <- e[j, , drop = FALSE]
      # by default elf is not moving
      new_pos <- elf

      neighbours <- sapply(all_directions, function(dr) {
        pos <- elf + dr
        any(e[,1] == pos[1] & e[,2] == pos[2])
      })

      if (any(neighbours)) {
        for (d in 1:4) {
          dd <- directions[[d]]
          if (!any(neighbours[dd])) {
            # main direction is always in the middle
            new_pos <- elf + all_directions[[dd[2]]]
            break
          }
        }
      }
      new_pos
    })
    proposed_moves <- do.call(rbind, proposed_moves)

    # remove duplicates
    dups <- duplicated(proposed_moves) | duplicated(proposed_moves, fromLast = TRUE)
    for (d in which(dups)) {
      proposed_moves[d, ] <- e[d, ]
    }


    # move elves
    e <- proposed_moves

    # cycle direction counter
    directions <- c(directions[-1], directions[1])
  }

  e
}


get_solution <- function(e) {
  nr <- max(e[,1]) - min(e[,1]) + 1
  nc <- max(e[,2]) - min(e[,2]) + 1
  nr * nc - nrow(e)
}





e_test <- read_input("input23_test.txt")
plot_elves(e_test)
move_elves(e_test, 10) |> plot_elves() |> get_solution()


e <- read_input("input23.txt")
move_elves(e, 10) |> plot_elves() |> get_solution()



# 2 -----------------------------------------------------------------------


move_elves2 <- function(e) {

  round <- 1

  all_directions <- list(
    nw = c(-1, -1),
    nn = c(-1, 0),
    ne = c(-1, 1),
    ee = c(0, 1),
    se = c(1, 1),
    ss = c(1, 0),
    sw = c(1, -1),
    ww = c(0, -1)
  )
  directions <- list(
    north = c("nw", "nn", "ne"),
    south = c("sw", "ss", "se"),
    west = c("nw", "ww", "sw"),
    east = c("ne", "ee", "se")
  )


  while (TRUE) {
    # find proposed moves
    proposed_moves <- lapply(seq_len(nrow(e)), function(j) {
      elf <- e[j, , drop = FALSE]
      # by default elf is not moving
      new_pos <- elf

      neighbours <- sapply(all_directions, function(dr) {
        pos <- elf + dr
        any(e[,1] == pos[1] & e[,2] == pos[2])
      })

      if (any(neighbours)) {
        for (d in 1:4) {
          dd <- directions[[d]]
          if (!any(neighbours[dd])) {
            # main direction is always in the middle
            new_pos <- elf + all_directions[[dd[2]]]
            break
          }
        }
      }
      new_pos
    })
    proposed_moves <- do.call(rbind, proposed_moves)

    # remove duplicates
    dups <- duplicated(proposed_moves) | duplicated(proposed_moves, fromLast = TRUE)
    for (d in which(dups)) {
      proposed_moves[d, ] <- e[d, ]
    }

    if (isTRUE(all.equal(e, proposed_moves))) break
    round <- round + 1
    if (round %% 100 == 0) message(Sys.time(), " round ", round)

    # move elves
    e <- proposed_moves

    # cycle direction counter
    directions <- c(directions[-1], directions[1])
  }

  message("round ", round)
  e
}




move_elves2(e_test) |> plot_elves()

move_elves2(e) |> plot_elves()
# round 950
# rather slow, took ~15 minutes, but working!
