
library(igraph)

#1 2 3 4 5 6 7 8 9 10 11 #
### 12 #14 #16 #18 # # #
### 13 #15 #17 #19 # # #

setup <- function() {
  rooms <- list(12:13, 14:15, 16:17, 18:19)
  pos_room <- c(rep(NA, 11), rep(1:4, each = 2))

  g <- graph(
    c(1,2, 2,3, 3,4, 4,5, 5,6, 6,7, 7,8, 8,9, 9,10, 10,11, 3,12, 12,13, 5,14, 14,15, 7,16, 16,17, 9,18, 18,19),
    directed = FALSE
  )

  # from hallway into the rooms
  paths_h_r <- lapply(setdiff(1:11, c(3,5,7,9)), function(v) {
    paths <- igraph::shortest_paths(g, from = v, to = 12:19)
    # omit starting vertex
    paths <- setNames(lapply(paths$vpath, function(v) tail(as.integer(v), -1)), 12:19)
    paths
  })
  paths_h_r <- setNames(paths_h_r, setdiff(1:11, c(3,5,7,9)))

  paths_r_h <- lapply(12:19, function(v) {
    paths <- igraph::shortest_paths(g, from = v, to = setdiff(1:11, c(3,5,7,9)))
    paths <- setNames(lapply(paths$vpath, function(v) tail(as.integer(v), -1)), setdiff(1:11, c(3,5,7,9)))
    paths
  })
  paths_r_h <- setNames(paths_r_h, 12:19)

  paths_r_r <- lapply(12:19, function(v) {
    targets <- 12:19
    # we cannot move within the same room
    targets <- setdiff(targets, rooms[[pos_room[v]]])
    paths <- igraph::shortest_paths(g, from = v, to = targets)
    paths <- setNames(lapply(paths$vpath, function(v) tail(as.integer(v), -1)), targets)
    paths
  })
  paths_r_r <- setNames(paths_r_r, 12:19)

  list(
    paths_h_r = paths_h_r,
    paths_r_h = paths_r_h,
    paths_r_r = paths_r_r,
    rooms = rooms,
    pos_room = pos_room
  )
}


read_input <- function(path) {
  l <- readLines(path)

  setup <- setup()

  # assign amphs to positions
  pos <- c(
    substr(l[3], 4, 4),
    substr(l[4], 4, 4),

    substr(l[3], 6, 6),
    substr(l[4], 6, 6),

    substr(l[3], 8, 8),
    substr(l[4], 8, 8),

    substr(l[3], 10, 10),
    substr(l[4], 10, 10)
  )

  ids <- rep(NA_integer_, 19)
  ids[12:19] <- match(pos, LETTERS[1:4])

  pos <- c(
    sort(which(pos == "A")),
    sort(which(pos == "B")),
    sort(which(pos == "C")),
    sort(which(pos == "D"))
  ) + 11

  #
  fixed <- logical(8)
  for (i in 1:4) {
    room <- setup$rooms[[i]]
    pos_ind <- (i*2 - 1):(i*2)

    tmp <- which(pos[pos_ind] == room[2])
    if (length(tmp) > 0) {
      fixed[pos_ind][tmp] <- TRUE
      tmp <- which(pos[pos_ind] == room[1])
    }
  }

  c(
    setup,
    list(
      pos = pos,
      ids = ids,
      fixed = fixed,
      key = paste(ids, collapse = ","),
      energy = 0
    )
  )
}



solve <- function(plan, energy, best_energy) {

  # if we surpass the so far best solution, stop solving
  if (energy > best_energy) return()

  # if all amph are in rooms, we are finished
  if (all(plan$fixed)) return(list(energy = energy, paths = list()))

  # if minimal energy left would result in worse solution, stop solving
  min_energy_left <- calculate_minimal_energy_left(plan)
  if (energy + min_energy_left > best_energy) return()

  # check all possibilities, if none of them return better option return NULL
  better_solution <- NULL

  # for each unmatched amph, starting from the most expensive
  for (a_id in rev(which(!plan$fixed))) {
    # find all available paths
    available_paths <- find_available_paths(a_id, plan)

    # for each path available for this amph
    for (path in available_paths) {
      # calculate new total energy
      new_energy <- energy + calculate_cost(a_id, path)

      # if we are already past the best solution, stop
      if (new_energy >= best_energy) next

      # update plan with the new path
      plan_updated <- update_plan(plan, a_id, path)
      plan_out <- solve(plan_updated, new_energy, best_energy)
      if (!is.null(plan_out) && plan_out$energy < best_energy) {
        best_energy <- plan_out$energy
        better_solution <- plan_out
      }
    }
  }

  better_solution
}



find_available_paths <- function(a_id, plan) {
  pos <- plan$pos[a_id]

  # check what is inside target room
  a_room <- plan$ids[pos]
  room <- plan$rooms[[a_room]]
  in_room <- plan$ids[room]

  if (pos <= 11) { # hallway -> room
    if (all(is.na(in_room))) {
      # if both empty - choose deeper path
      all_paths <- plan$paths_h_r[[as.character(pos)]][as.character(room[2])]

    } else if (isTRUE(any(in_room != a_room))) {
      # if there is anything different - no paths available
      return(list())

    } else {
      # if there is already one amph of our type - choose shorter
      all_paths <- plan$paths_h_r[[as.character(pos)]][as.character(room[1])]
    }


  } else {

    if (all(is.na(in_room))) {
      # if both empty - choose deeper path
      all_paths_r <- plan$paths_r_r[[as.character(pos)]][as.character(room[2])]

    } else if (isTRUE(any(in_room != a_room))) {
      # if there is anything different - no paths available
      all_paths_r <- list()

    } else {
      # if there is already one amph of our type - choose shorter
      all_paths_r <- plan$paths_r_r[[as.character(pos)]][as.character(room[1])]
    }

    # if path to the room is not blocked, use only this path
    if (length(all_paths_r) == 1 && length(intersect(all_paths_r[[1]], plan$pos)) == 0) {
      all_paths <- all_paths_r
    } else {
      all_paths_h <- plan$paths_r_h[[as.character(pos)]]
      all_paths <- c(all_paths_r, all_paths_h)
    }
  }

  # discard those paths, that are blocked
  Filter(function(p) length(intersect(p, plan$pos)) == 0, all_paths)
}


calculate_cost <- function(a_id, path) {
  cost <- c(1,1, 10,10, 100,100, 1000,1000)
  length(path) * cost[a_id]
}


update_plan <- function(plan, a_id, path) {
  # update positions
  target_pos <- tail(path, 1)
  current_pos <- plan$pos[a_id]
  plan$ids[target_pos] <- plan$ids[current_pos]
  plan$ids[current_pos] <- NA_integer_
  plan$pos[a_id] <- target_pos

  # check for fix positions
  if (target_pos >= 12) {
    a_room <- plan$ids[target_pos]
    room <- plan$rooms[[a_room]]
    if (isTRUE(target_pos == room[2]) ||
        isTRUE(target_pos == room[1] && plan$ids[room[2]] == a_room)) {
      plan$fixed[a_id] <- TRUE
    }
  }
  plan$key <- paste(plan$ids, collapse = ",")
  plan
}

calculate_minimal_energy_left <- function(plan) {
  energy_left <- 0
  for (a_id in rev(which(!plan$fixed))) {
    # find path to the room, use shorter as it's more pessimistic
    pos <- plan$pos[a_id]
    a_room <- plan$ids[pos]
    room <- plan$rooms[[a_room]]

    if (pos >= 12) { # room to room
      paths <- plan$paths_r_r
    } else { # hall to room
      paths <- plan$paths_h_r
    }
    path <- paths[[as.character(pos)]][[as.character(room[1])]]

    energy_left <- energy_left + calculate_cost(a_id, path)
  }
  energy_left
}



test <- read_input("input23_test.txt")
# limit calculated by hand
system.time(
  res <- solve(test, 0, 13000)
)


input <- read_input("input23.txt")
calculate_minimal_energy_left(input)
# we found this solution by hand and it was too high, so we could surely use it as limit
system.time(
  res <- solve(input, 0, 16212)
)
# user  system elapsed
# 24.33    0.16   24.59
res
# 16157



system.time(
  res <- solve(test, 0, Inf)
)
# user  system elapsed
# 11.17    0.05   11.25
res

system.time(
  res <- solve(input, 0, Inf)
)
# user  system elapsed
# 68.63    0.08   68.81
res



# probably find available paths is too slow, in particular discarding blocked paths
# discarding other paths when there is one to room available helped significantly
# still not enough for the bigger version


# 2 -----------------------------------------------------------------------


#1 2 3 4 5 6 7 8 9 10 11 #
### 12 #16 #20 #24 # # #
### 13 #17 #21 #25 # # #
### 14 #18 #22 #26 # # #
### 15 #19 #23 #27 # # #


setup2 <- function() {
  rooms <- list(12:15, 16:19, 20:23, 24:27)
  pos_room <- c(rep(NA, 11), rep(1:4, each = 4))

  g <- graph(
    c(1,2, 2,3, 3,4, 4,5, 5,6, 6,7, 7,8, 8,9, 9,10, 10,11,
      3,12, 12,13, 13,14, 14,15,
      5,16, 16,17, 17,18, 18,19,
      7,20, 20,21, 21,22, 22,23,
      9,24, 24,25, 25,26, 26,27),
    directed = FALSE
  )

  # from hallway into the rooms

  paths_h_r <- lapply(setdiff(1:11, c(3,5,7,9)), function(v) {
    paths <- igraph::shortest_paths(g, from = v, to = 12:27)
    # omit starting vertex
    paths <- setNames(lapply(paths$vpath, function(v) tail(as.integer(v), -1)), 12:27)
    paths
  })
  paths_h_r <- setNames(paths_h_r, setdiff(1:11, c(3,5,7,9)))

  paths_r_h <- lapply(12:27, function(v) {
    paths <- igraph::shortest_paths(g, from = v, to = setdiff(1:11, c(3,5,7,9)))
    paths <- setNames(lapply(paths$vpath, function(v) tail(as.integer(v), -1)), setdiff(1:11, c(3,5,7,9)))
    paths
  })
  paths_r_h <- setNames(paths_r_h, 12:27)

  paths_r_r <- lapply(12:27, function(v) {
    targets <- 12:27
    # we cannot move within the same room
    targets <- setdiff(targets, rooms[[pos_room[v]]])
    paths <- igraph::shortest_paths(g, from = v, to = targets)
    paths <- setNames(lapply(paths$vpath, function(v) tail(as.integer(v), -1)), targets)
    paths
  })
  paths_r_r <- setNames(paths_r_r, 12:27)

  list(
    paths_h_r = paths_h_r,
    paths_r_h = paths_r_h,
    paths_r_r = paths_r_r,
    rooms = rooms,
    pos_room = pos_room
  )
}

read_input2 <- function(path) {
  l <- readLines(path)

  setup <- setup2()

  # assign amphs to positions
  pos <- c(
    substr(l[3], 4, 4),
    "D",
    "D",
    substr(l[4], 4, 4),

    substr(l[3], 6, 6),
    "C",
    "B",
    substr(l[4], 6, 6),

    substr(l[3], 8, 8),
    "B",
    "A",
    substr(l[4], 8, 8),

    substr(l[3], 10, 10),
    "A",
    "C",
    substr(l[4], 10, 10)
  )

  ids <- rep(NA_integer_, 27)
  ids[12:27] <- match(pos, LETTERS[1:4])

  pos <- c(
    sort(which(pos == "A")),
    sort(which(pos == "B")),
    sort(which(pos == "C")),
    sort(which(pos == "D"))
  ) + 11

  #
  fixed <- logical(16)
  for (i in 1:4) {
    room <- setup$rooms[[i]]
    pos_ind <- (i*4 - 3):(i*4)

    tmp <- which(pos[pos_ind] == room[4])
    if (length(tmp) > 0) {
      fixed[pos_ind][tmp] <- TRUE
      tmp <- which(pos[pos_ind] == room[3])
      if (length(tmp) > 0) {
        fixed[pos_ind][tmp] <- TRUE
        tmp <- which(pos[pos_ind] == room[2])
        if (length(tmp) > 0) {
          fixed[pos_ind][tmp] <- TRUE
          tmp <- which(pos[pos_ind] == room[1])
          if (length(tmp) > 0) {
            fixed[pos_ind][tmp] <- TRUE
          }
        }
      }
    }
  }

  c(
    setup,
    list(
      pos = pos,
      ids = ids,
      fixed = fixed,
      key = paste(ids, collapse = ","),
      energy = 0
    )
  )
}




solve2 <- function(plan, energy, best_energy) {

  # if we surpass the so far best solution, stop solving
  if (energy > best_energy) return()

  # if all amph are in rooms, we are finished
  if (all(plan$fixed)) return(list(energy = energy, paths = list()))

  # if minimal energy left would result in worse solution, stop solving
  min_energy_left <- calculate_minimal_energy_left2(plan)
  if (energy + min_energy_left > best_energy) return()

  # check all possibilities, if none of them return better option return NULL
  better_solution <- NULL

  # for each unmatched amph, starting from the most expensive
  for (a_id in rev(which(!plan$fixed))) {
    # find all available paths
    available_paths <- find_available_paths2(a_id, plan)

    # for each path available for this amph
    for (path in available_paths) {
      # calculate new total energy
      new_energy <- energy + calculate_cost2(a_id, path)

      # if we are already past the best solution, stop
      if (new_energy >= best_energy) next

      # update plan with the new path
      plan_updated <- update_plan2(plan, a_id, path)
      plan_out <- solve2(plan_updated, new_energy, best_energy)
      if (!is.null(plan_out) && plan_out$energy < best_energy) {
        best_energy <- plan_out$energy
        plan_out$paths <- c(list(path), plan_out$paths)
        better_solution <- plan_out
      }
    }
  }

  better_solution
}



find_available_paths2 <- function(a_id, plan) {
  pos <- plan$pos[a_id]

  # check what is inside target room
  a_room <- plan$ids[pos]
  room <- plan$rooms[[a_room]]
  in_room <- plan$ids[room]

  if (pos <= 11) { # hallway -> room
    if (isTRUE(any(in_room != a_room))) {
      # if there is anything different - no paths available
      return(list())
    } else {
      # count the number of amphs in the room to get the deepest path
      s <- sum(in_room == a_room, na.rm = TRUE)
      all_paths <- plan$paths_h_r[[as.character(pos)]][as.character(room[4-s])]
    }


  } else {

    if (isTRUE(any(in_room != a_room))) {
      # if there is anything different - no paths available
      all_paths_r <- list()
    } else {
      # count the number of amphs in the room to get the deepest path
      s <- sum(in_room == a_room, na.rm = TRUE)
      all_paths_r <- plan$paths_r_r[[as.character(pos)]][as.character(room[4-s])]
    }

    # if path to the room is not blocked, use only this path
    if (length(all_paths_r) == 1 && length(intersect(all_paths_r[[1]], plan$pos)) == 0) {
      all_paths <- all_paths_r
    } else {
      all_paths_h <- plan$paths_r_h[[as.character(pos)]]
      all_paths <- c(all_paths_r, all_paths_h)
    }
  }

  # discard those paths, that are blocked
  Filter(function(p) length(intersect(p, plan$pos)) == 0, all_paths)
}


calculate_cost2 <- function(a_id, path) {
  cost <- c(rep(1,4), rep(10,4), rep(100,4), rep(1000,4))
  length(path) * cost[a_id]
}


update_plan2 <- function(plan, a_id, path) {
  # update positions
  target_pos <- tail(path, 1)
  current_pos <- plan$pos[a_id]
  plan$ids[target_pos] <- plan$ids[current_pos]
  plan$ids[current_pos] <- NA_integer_
  plan$pos[a_id] <- target_pos

  # check for fix positions
  if (target_pos >= 12) {
    a_room <- plan$ids[target_pos]
    room <- plan$rooms[[a_room]]
    if (isTRUE(target_pos == room[4]) ||
        isTRUE(target_pos == room[3] && plan$ids[room[4]] == a_room) ||
        isTRUE(target_pos == room[2] && all(plan$ids[room[3:4]] == a_room)) ||
        isTRUE(target_pos == room[1] && all(plan$ids[room[2:4]] == a_room))) {
      plan$fixed[a_id] <- TRUE
    }
  }
  plan$key <- paste(plan$ids, collapse = ",")
  plan
}


calculate_minimal_energy_left2 <- function(plan) {
  energy_left <- 0
  for (a_id in rev(which(!plan$fixed))) {
    # find path to the room, use shorter as it's more pessimistic
    pos <- plan$pos[a_id]
    a_room <- plan$ids[pos]
    room <- plan$rooms[[a_room]]

    if (pos >= 12) { # room to room
      paths <- plan$paths_r_r
    } else { # hall to room
      paths <- plan$paths_h_r
    }
    path <- paths[[as.character(pos)]][[as.character(room[1])]]

    energy_left <- energy_left + calculate_cost2(a_id, path)
  }
  energy_left
}


test <- read_input2("input23_test.txt")
calculate_minimal_energy_left2(test)
system.time(
  res <- solve2(test, 0, 45000)
)
# manually make couple of steps and check whether it could be solved
test2 <- test
test2$pos[16] <- 11
test2$ids[24] <- NA
test2$ids[11] <- 4

test2$pos[3] <- 1
test2$ids[25] <- NA
test2$ids[1] <- 1

test2$pos[7] <- 10
test2$ids[20] <- NA
test2$ids[10] <- 2

test2$pos[8] <- 8
test2$ids[21] <- NA
test2$ids[8] <- 2

test2$pos[2] <- 2
test2$ids[22] <- NA
test2$ids[2] <- 1
system.time(
  res <- solve2(test2, 3000+10+40+30+8, 45000)
)




input <- read_input2("input23.txt")
calculate_minimal_energy_left2(test)
system.time(
  res <- solve2(input, 0, 43881)
)

# manual start
input2 <- input
input2$pos[8] <- 1
input2$ids[24] <- NA
input2$ids[1] <- 2

input2$pos[4] <- 2
input2$ids[25] <- NA
input2$ids[2] <- 1

input2$pos[11] <- 11
input2$ids[26] <- NA
input2$ids[11] <- 3

input2$pos[12] <- 10
input2$ids[27] <- NA
input2$ids[10] <- 3

input2$pos[16] <- 27
input2$ids[16] <- NA
input2$ids[27] <- 4
input2$fixed[16] <- TRUE

input2$pos[13] <- 26
input2$ids[12] <- NA
input2$ids[26] <- 4
input2$fixed[13] <- TRUE

input2$pos[14] <- 25
input2$ids[13] <- NA
input2$ids[25] <- 4
input2$fixed[14] <- TRUE

input2$pos[15] <- 24
input2$ids[14] <- NA
input2$ids[24] <- 4
input2$fixed[15] <- TRUE

system.time(
  res <- solve2(input2, 90+9+1000+39000, 43881)
)
# 43481











# dijkstra ----------------------------------------------------------------


solve_d <- function(initial_plan) {

  sure_plans_score <- numeric()

  # we need to keep track of order in both sets
  unsure_plans <- list(initial_plan)
  unsure_plans_score <- setNames(0, initial_plan$key)

  while (length(unsure_plans) > 0) {
    # sort unsure set
    ord <- order(unsure_plans_score)
    unsure_plans <- unsure_plans[ord]
    unsure_plans_score <- unsure_plans_score[ord]


    # get first plan
    current_plan <- unsure_plans[[1]]

    sure_plans_score[current_plan$key] <- current_plan$energy

    unsure_plans <- unsure_plans[-1]
    unsure_plans_score <- unsure_plans_score[-which(names(unsure_plans_score) == current_plan$key)]

    # get all paths for the current plan
    all_paths <- lapply(rev(which(!current_plan$fixed)), function(a_id) {
      available_paths <- find_available_paths(a_id, current_plan)
      lapply(available_paths, function(p) list(a_id = a_id, path = p))
    })
    all_paths <- unlist(all_paths, recursive = FALSE)

    #
    for (path in all_paths) {
      # calculate new energy
      new_energy <- current_plan$energy + calculate_cost(path$a_id, path$path)

      # update plan with the new path
      plan_updated <- update_plan(current_plan, path$a_id, path$path)
      plan_updated$energy <- new_energy

      # omit already sure plans
      if (plan_updated$key %in% names(sure_plans_score)) {
        next

      } else if (plan_updated$key %in% names(unsure_plans_score)) {
        # plan already in unsure set
        ind <- which(names(unsure_plans_score) == plan_updated$key)

        # compare energy and update if required
        if (plan_updated$energy < unsure_plans_score[ind]) {
          unsure_plans_score[ind] <- plan_updated$energy
          unsure_plans[[ind]] <- plan_updated
        }

      } else {
        # completely new plan - add at then end
        unsure_plans <- c(unsure_plans, list(plan_updated))
        unsure_plans_score <- c(unsure_plans_score,
                                setNames(plan_updated$energy, plan_updated$key))
      }
    }

  }

  sure_plans_score
}



final_state <- paste(c(rep("NA", 11), rep(1:4, each = 2)), collapse = ",")

test <- read_input("input23_test.txt")
system.time(
  res <- solve_d(test)
)
# 8s
res[grepl(final_state, names(res))]
# correct!




input <- read_input("input23.txt")
# we found this solution by hand and it was too high, so we could surely use it as limit
system.time(
  res <- solve_d(input)
)
# ~100s
res[grepl(final_state, names(res))]
# correct


# only names of functions differ

solve_d2 <- function(initial_plan) {

  sure_plans_score <- numeric()

  # we need to keep track of order in both sets
  unsure_plans <- list(initial_plan)
  unsure_plans_score <- setNames(0, initial_plan$key)

  while (length(unsure_plans) > 0) {
    # sort unsure set
    ord <- order(unsure_plans_score)
    unsure_plans <- unsure_plans[ord]
    unsure_plans_score <- unsure_plans_score[ord]


    # get first plan
    current_plan <- unsure_plans[[1]]

    sure_plans_score[current_plan$key] <- current_plan$energy

    unsure_plans <- unsure_plans[-1]
    unsure_plans_score <- unsure_plans_score[-which(names(unsure_plans_score) == current_plan$key)]

    # get all paths for the current plan
    all_paths <- lapply(rev(which(!current_plan$fixed)), function(a_id) {
      available_paths <- find_available_paths2(a_id, current_plan)
      lapply(available_paths, function(p) list(a_id = a_id, path = p))
    })
    all_paths <- unlist(all_paths, recursive = FALSE)

    #
    for (path in all_paths) {
      # calculate new energy
      new_energy <- current_plan$energy + calculate_cost2(path$a_id, path$path)

      # update plan with the new path
      plan_updated <- update_plan2(current_plan, path$a_id, path$path)
      plan_updated$energy <- new_energy

      # omit already sure plans
      if (plan_updated$key %in% names(sure_plans_score)) {
        next

      } else if (plan_updated$key %in% names(unsure_plans_score)) {
        # plan already in unsure set
        ind <- which(names(unsure_plans_score) == plan_updated$key)

        # compare energy and update if required
        if (plan_updated$energy < unsure_plans_score[ind]) {
          unsure_plans_score[ind] <- plan_updated$energy
          unsure_plans[[ind]] <- plan_updated
        }

      } else {
        # completely new plan - add at then end
        unsure_plans <- c(unsure_plans, list(plan_updated))
        unsure_plans_score <- c(unsure_plans_score,
                                setNames(plan_updated$energy, plan_updated$key))
      }
    }

  }

  sure_plans_score
}




final_state2 <- paste(c(rep("NA", 11), rep(1:4, each = 4)), collapse = ",")

test <- read_input2("input23_test.txt")
system.time(
  res <- solve_d2(test)
)
# user  system elapsed
# 340.37    2.77  343.89
res[grepl(final_state2, names(res))]
# should be 44169
# correct!



input <- read_input2("input23.txt")
system.time(
  res <- solve_d2(input)
)
# user  system elapsed
# 350.58    1.22  353.08
res[grepl(final_state2, names(res))]
# should be 43481
# correct!


