




read_input <- function(path) {
  b <- readLines(path)
  b <- do.call(rbind, strsplit(b, ""))
  # strip borders
  b <- b[-c(1, nrow(b)), -c(1, ncol(b))]

  bl <- which(b != ".", arr.ind = TRUE)

  directions <- list(
    ">" = c(0, 1),
    "<" = c(0, -1),
    "^" = c(-1, 0),
    "v" = c(1, 0)
  )
  drs <- b[bl]

  drs <- do.call(rbind, directions[b[bl]])
  list(
    dim = dim(b),
    blizzards = cbind(bl, drs)
  )
}



generate_blizzard_cycles <- function(b, add = 0) {
  nr <- b$dim[1]
  nc <- b$dim[2]
  bl <- b$blizzards

  n_cycle <- numbers::LCM(nr, nc) + add
  full_cycle <- vector("list", n_cycle)
  full_cycle[[1]] <- bl

  for (i in 2:n_cycle) {
    bl[, 1:2] <- bl[, 1:2] + bl[, 3:4]
    bl[,1] <- (bl[,1] - 1) %% nr + 1
    bl[,2] <- (bl[,2] - 1) %% nc + 1
    full_cycle[[i]] <- bl
  }

  full_cycle
}


make_logical_cycle <- function(b) {
  full_cycle <- generate_blizzard_cycles(b)
  m <- matrix(FALSE, nrow = b$dim[1], ncol = b$dim[2])

  lapply(full_cycle, function(fc) {
    m2 <- m
    m2[fc[,1:2]] <- TRUE
    m2
  })

}


find_path <- function(
    full_cycle,
    dims,
    pos = matrix(c(0, 1), nrow = 1),
    bl_cycle = 1,
    steps = 0,
    current_best = Inf,
    history = list()
) {
  # message("steps ", steps, " best ", current_best, " bc ", bl_cycle, " pos ", pos)
  label <- paste(bl_cycle, pos, collapse = "_")
  if (!is.null(history[[label]])) {
    # message("history at ", label)
    return(list(current_best, history))
  } else {
    history[[label]] <- current_best
  }

  # exit c(dims[1] + 1, dims(2))
  # check if we could reach exit
  if (pos[1] == dims[1] && pos[2] == dims[2]) return(list(min(current_best, steps + 1), history))

  # check if it is impossible to get to exit in less than best number of steps
  fastest <- (dims[1] + 1 - pos[1]) + (dims[2] - pos[2])
  if (steps + fastest >= current_best) return(list(current_best, history))

  # next blizzard state
  bl_cycle <- (bl_cycle %% length(full_cycle)) + 1
  bl <- full_cycle[[bl_cycle]]

  # check every move - promote those towards the exit, than wait, than backwards

  # down
  next_pos <- pos + c(1,0)
  if (next_pos[1] <= dims[1] && !bl[next_pos]) {
    dd <- Recall(
      full_cycle,
      dims,
      next_pos,
      bl_cycle,
      steps + 1,
      current_best,
      history
    )
    current_best <- min(dd[[1]], current_best)
    history <- dd[[2]]
  }

  # right
  next_pos <- pos + c(0, 1)
  # check the starting position as well
  if (pos[1] > 0 && next_pos[2] <= dims[2] && !bl[next_pos]) {
    rr <- Recall(
      full_cycle,
      dims,
      next_pos,
      bl_cycle,
      steps + 1,
      current_best,
      history
    )
    current_best <- min(rr[[1]], current_best)
    history <- rr[[2]]
  }

  # wait
  if (pos[1] == 0 || !bl[pos]) {
    ww <- Recall(
      full_cycle,
      dims,
      pos,
      bl_cycle,
      steps + 1,
      current_best,
      history
    )
    current_best <- min(ww[[1]], current_best)
    history <- ww[[2]]
  }

  # up
  next_pos <- pos + c(-1, 0)
  # check the starting position as well
  if (pos[1] > 0 && next_pos[1] > 0 && !bl[next_pos]) {
    uu <- Recall(
      full_cycle,
      dims,
      next_pos,
      bl_cycle,
      steps + 1,
      current_best,
      history
    )
    current_best <- min(uu[[1]], current_best)
    history <- uu[[2]]
  }

  # left
  next_pos <- pos + c(0, -1)
  # check the starting position as well
  if (pos[1] > 0 && next_pos[2] > 0 && !bl[next_pos]) {
    ll <- Recall(
      full_cycle,
      dims,
      next_pos,
      bl_cycle,
      steps + 1,
      current_best,
      history
    )
    current_best <- min(ll[[1]], current_best)
    history <- ll[[2]]
  }


  list(current_best, history)
}






b_test <- read_input("input24_test.txt")
bc_test <- generate_blizzard_cycles(b_test, 1)
# check correctness
identical(head(bc_test, 1), tail(bc_test, 1))
bcl_test <- make_logical_cycle(b_test)


out_test <- find_path(
  full_cycle = bcl_test,
  dims = b_test$dim,
  pos = matrix(c(0, 1), nrow = 1),
  bl_cycle = 1,
  steps = 0,
  current_best = Inf
)
out_test[[1]]
out_test[[2]]



b <- read_input("input24.txt")
bc <- generate_blizzard_cycles(b, 1)
# check correctness
identical(head(bc, 1), tail(bc, 1))
bcl <- make_logical_cycle(b)


out <- find_path(
  full_cycle = bcl,
  dims = b$dim,
  pos = matrix(c(0, 1), nrow = 1),
  bl_cycle = 1,
  steps = 0,
  current_best = Inf
)
out[[1]]
# 266
length(out[[2]])

# super slow - but working



# 2 -----------------------------------------------------------------------

# first we need to adjust function, so we know what is the final
# blizzard cycle - AT the minute we are at exit
# keep it together with current_best
find_path <- function(
    full_cycle,
    dims,
    pos = matrix(c(0, 1), nrow = 1),
    bl_cycle = 1,
    steps = 0,
    current_best = c(Inf, 0),
    history = list()
) {
  label <- paste(bl_cycle, pos[1], pos[2], collapse = "_")
  if (!is.null(history[[label]])) {
    return(list(current_best, history))
  } else {
    history[[label]] <- current_best
  }

  # exit c(dims[1] + 1, dims(2))
  # check if we could reach exit
  if (pos[1] == dims[1] && pos[2] == dims[2]) return(list(c(min(current_best[1], steps + 1), bl_cycle + 1), history))

  # check if it is impossible to get to exit in less than best number of steps
  fastest <- (dims[1] + 1 - pos[1]) + (dims[2] - pos[2])
  if (steps + fastest >= current_best[1]) return(list(current_best, history))

  # next blizzard state
  bl_cycle <- (bl_cycle %% length(full_cycle)) + 1
  bl <- full_cycle[[bl_cycle]]

  # check every move - promote those towards the exit, than wait, than backwards

  # down
  next_pos <- pos + c(1,0)
  if (next_pos[1] <= dims[1] && !bl[next_pos]) {
    dd <- Recall(
      full_cycle,
      dims,
      next_pos,
      bl_cycle,
      steps + 1,
      current_best,
      history
    )
    if (dd[[1]][1] < current_best[1]) {
      current_best <- dd[[1]]
    }
    history <- dd[[2]]
  }

  # right
  next_pos <- pos + c(0, 1)
  # check the starting position as well
  if (pos[1] > 0 && next_pos[2] <= dims[2] && !bl[next_pos]) {
    rr <- Recall(
      full_cycle,
      dims,
      next_pos,
      bl_cycle,
      steps + 1,
      current_best,
      history
    )
    if (rr[[1]][1] < current_best[1]) {
      current_best <- rr[[1]]
    }
    history <- rr[[2]]
  }

  # wait
  if (pos[1] == 0 || !bl[pos]) {
    ww <- Recall(
      full_cycle,
      dims,
      pos,
      bl_cycle,
      steps + 1,
      current_best,
      history
    )
    if (ww[[1]][1] < current_best[1]) {
      current_best <- ww[[1]]
    }
    history <- ww[[2]]
  }

  # up
  next_pos <- pos + c(-1, 0)
  # check the starting position as well
  if (pos[1] > 0 && next_pos[1] > 0 && !bl[next_pos]) {
    uu <- Recall(
      full_cycle,
      dims,
      next_pos,
      bl_cycle,
      steps + 1,
      current_best,
      history
    )
    if (uu[[1]][1] < current_best[1]) {
      current_best <- uu[[1]]
    }
    history <- uu[[2]]
  }

  # left
  next_pos <- pos + c(0, -1)
  # check the starting position as well
  if (pos[1] > 0 && next_pos[2] > 0 && !bl[next_pos]) {
    ll <- Recall(
      full_cycle,
      dims,
      next_pos,
      bl_cycle,
      steps + 1,
      current_best,
      history
    )
    if (ll[[1]][1] < current_best[1]) {
      current_best <- ll[[1]]
    }
    history <- ll[[2]]
  }


  list(current_best, history)
}

# let's use another function for journey back
find_path_back <- function(
    full_cycle,
    dims,
    pos = matrix(c(dims[1] + 1, dims[2]), nrow = 1),
    bl_cycle = 1,
    steps = 0,
    current_best = c(Inf, 0),
    history = list()
) {
  label <- paste(bl_cycle, pos[1], pos[2], sep = "_")
  if (!is.null(history[[label]])) {
    return(list(current_best, history))
  } else {
    history[[label]] <- current_best
  }

  # exit c(0, 1)
  # check if we could reach exit
  if (pos[1] == 1 && pos[2] == 1) return(list(c(min(current_best[1], steps + 1), bl_cycle + 1), history))

  # check if it is impossible to get to exit in less than best number of steps
  fastest <- pos[1] + (pos[2] - 1)
  if (steps + fastest >= current_best[1]) return(list(current_best, history))

  # next blizzard state
  bl_cycle <- (bl_cycle %% length(full_cycle)) + 1
  bl <- full_cycle[[bl_cycle]]

  # check every move - promote those towards the exit, than wait, than backwards

  # up
  next_pos <- pos + c(-1, 0)
  if (next_pos[1] > 0 && !bl[next_pos]) {
    uu <- Recall(
      full_cycle,
      dims,
      next_pos,
      bl_cycle,
      steps + 1,
      current_best,
      history
    )
    if (uu[[1]][1] < current_best[1]) {
      current_best <- uu[[1]]
    }
    history <- uu[[2]]
  }

  # left
  next_pos <- pos + c(0, -1)
  # check the starting position as well
  if (pos[1] <= dims[1] && next_pos[2] > 0 && !bl[next_pos]) {
    ll <- Recall(
      full_cycle,
      dims,
      next_pos,
      bl_cycle,
      steps + 1,
      current_best,
      history
    )
    if (ll[[1]][1] < current_best[1]) {
      current_best <- ll[[1]]
    }
    history <- ll[[2]]
  }

  # wait
  if (pos[1] == dims[1] + 1 || !bl[pos]) {
    ww <- Recall(
      full_cycle,
      dims,
      pos,
      bl_cycle,
      steps + 1,
      current_best,
      history
    )
    if (ww[[1]][1] < current_best[1]) {
      current_best <- ww[[1]]
    }
    history <- ww[[2]]
  }

  # down
  next_pos <- pos + c(1,0)
  # check the starting position as well
  if (pos[1] < dims[1] && !bl[next_pos]) {
    dd <- Recall(
      full_cycle,
      dims,
      next_pos,
      bl_cycle,
      steps + 1,
      current_best,
      history
    )
    if (dd[[1]][1] < current_best[1]) {
      current_best <- dd[[1]]
    }
    history <- dd[[2]]
  }

  # right
  next_pos <- pos + c(0, 1)
  # check the starting position as well
  if (pos[1] <= dims[1] && next_pos[2] <= dims[2] && !bl[next_pos]) {
    rr <- Recall(
      full_cycle,
      dims,
      next_pos,
      bl_cycle,
      steps + 1,
      current_best,
      history
    )
    if (rr[[1]][1] < current_best[1]) {
      current_best <- rr[[1]]
    }
    history <- rr[[2]]
  }

  list(current_best, history)
}


b_test <- read_input("input24_test.txt")
bcl_test <- make_logical_cycle(b_test)
out_test <- find_path(
  full_cycle = bcl_test,
  dims = b_test$dim
)
out_test[[1]]
out_test[[2]]
out_test2 <- find_path_back(
  full_cycle = bcl_test,
  dims = b_test$dim,
  bl_cycle = out_test[[1]][2]
)
out_test2[[1]]
out_test3 <- find_path(
  full_cycle = bcl_test,
  dims = b_test$dim,
  bl_cycle = out_test2[[1]][2]
)
out_test3[[1]]

out_test[[1]] + out_test2[[1]] + out_test3[[1]]





b <- read_input("input24.txt")
bcl <- make_logical_cycle(b)
out <- find_path(
  full_cycle = bcl,
  dims = b$dim
)
out[[1]]
# [1] 266 267

out2 <- find_path_back(
  full_cycle = bcl,
  dims = b$dim,
  bl_cycle = out[[1]][2]
)
out2[[1]]
# [1] 301 568

out3 <- find_path(
  full_cycle = bcl,
  dims = b$dim,
  bl_cycle = out2[[1]][2]
)
out3[[1]]
# [1] 286 254

out[[1]] + out2[[1]] + out3[[1]]
# [1]  853 1089
