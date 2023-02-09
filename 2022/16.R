




read_input <- function(path) {
  v <- readLines(path)
  valve_id <- substr(v, 7, 8)

  rate <- sapply(v, function(x) as.integer(substr(x, 24, regexpr(";", x) - 1)), USE.NAMES = FALSE)
  rate <- setNames(rate, valve_id)

  tunnels <- sapply(v, function(x) substr(x, regexpr("valves? ", x), nchar(x)), USE.NAMES = FALSE)
  tunnels <- gsub("valves? ", "", tunnels)
  tunnels <- strsplit(tunnels, ", ?")
  tunnels <- setNames(tunnels, valve_id)

  paths <- matrix(FALSE, length(v), length(v), dimnames = list(valve_id, valve_id))
  for (i in seq_along(v)) {
    paths[i, tunnels[[i]]] <- TRUE
  }
  graph <- igraph::graph_from_adjacency_matrix(paths)
  paths <- igraph::distances(graph)

  list(
    valves = valve_id,
    rates = rate,
    paths = paths
  )
}


# 1 -----------------------------------------------------------------------


solve <- function(valves) {

  open <- setNames(logical(length(valves$valves)), valves$valves)
  open_time <- setNames(integer(length(valves$valves)), valves$valves)
  time <- 30
  current_valve <- "AA"
  paths <- valves$paths

  while (time > 0) {
    v_d <- paths[current_valve, ][!open]
    v_r <- valves$rates[!open]
    pressure_till_end <- (time - v_d - 1) * v_r
    if (all(pressure_till_end <= 0)) break

    tmp <- which.max(pressure_till_end)
    current_valve <- names(tmp)
    open[current_valve] <- TRUE
    time <- time - v_d[tmp] - 1
    open_time[current_valve] <- time
  }

  # calculate total pressure
  sum(open_time[open] * valves$rates[open])
}


v_test <- read_input("input16_test.txt")
solve(v_test)

v <- read_input("input16.txt")
solve(v)

# does NOT work - this algorithm it too local and short term
# we need to find global - better - solution


# try to develop recursive one

prepare_valves <- function(valves) {
  # we could ignore valves with rate zero - apart from starting valve!
  ind <- setdiff(which(valves$rates == 0), which(valves$valves == "AA"))
  valves$valves <- valves$valves[-ind]
  valves$rates <- valves$rates[-ind]
  valves$paths <- valves$paths[-ind, -ind]

  valves$open <- setNames(logical(length(valves$valves)), valves$valves)
  valves$open_time <- setNames(integer(length(valves$valves)), valves$valves)
  valves
}

solve <- function(valves, time, current_valve, current_best) {

  v_d <- valves$paths[current_valve, ][!valves$open]
  v_r <- valves$rates[!valves$open]
  pressure_till_end <- (time - v_d - 1) * v_r

  # if there are no possible moves, this is the end
  # we could calculate current pressure and compare it with the best value
  current_pressure <- sum(valves$open_time[valves$open] * valves$rates[valves$open])
  if (all(pressure_till_end <= 0)) {
    if (current_pressure > current_best) {
      return(current_pressure)
    } else {
      return(0)
    }
  }


  # check upper bound for the best pressure - if we can't reach it
  # assume we could open valves one by one (every two minutes) in descending order
  tmp <- pressure_till_end - (rank(-pressure_till_end, ties.method = "first") - 1) * 2 * v_r
  possible_best <- current_pressure + sum(pmax(tmp, 0))
  if (possible_best < current_best) return(0)


  # solve further
  for (i in order(pressure_till_end, decreasing = TRUE)) {
    if (pressure_till_end[i] <= 0) break

    new_valves <- valves
    next_current_valve <- names(pressure_till_end)[i]

    new_valves$open[next_current_valve] <- TRUE
    new_time <- time - v_d[i] - 1
    new_valves$open_time[next_current_valve] <- new_time

    out <- solve(new_valves, new_time, next_current_valve, current_best)
    if (out > current_best) current_best <- out
  }

  current_best
}


v_test <- read_input("input16_test.txt") |> prepare_valves()
solve(v_test, 30, "AA", 0)

v <- read_input("input16.txt") |> prepare_valves()
solve(v, 30, "AA", 0)
# 1915


# 2 -----------------------------------------------------------------------

# elephant could open valves in different moments than me
# this means that we cannot update time in batches
# instead we could keep time left to the next valve selection

solve <- function(valves, time, current_valve, current_best, eta) {
  # infinite eta means that agent has no choices to be made

  # get next possible moves for both agents
  # valves would be always the same at this step - only distances could vary!

  v_d_1 <- valves$paths[current_valve[1], ][!valves$open]
  v_r_1 <- valves$rates[!valves$open]
  pressure_till_end_1 <- (time - v_d_1 - 1) * v_r_1
  pressure_till_end_1 <- pressure_till_end_1[pressure_till_end_1 > 0]
  if (length(pressure_till_end_1) == 0) eta[1] <- Inf

  v_d_2 <- valves$paths[current_valve[2], ][!valves$open]
  v_r_2 <- valves$rates[!valves$open]
  pressure_till_end_2 <- (time - v_d_2 - 1) * v_r_2
  pressure_till_end_2 <- pressure_till_end_2[pressure_till_end_2 > 0]
  if (length(pressure_till_end_2) == 0) eta[2] <- Inf

  # if there are no possible moves for both agents, this is the end
  # we could calculate current pressure and compare it with the best value
  current_pressure <- sum(valves$open_time[valves$open] * valves$rates[valves$open])
  if (all(is.infinite(eta))) {
    if (current_pressure > current_best) {
      return(current_pressure)
    } else {
      return(0)
    }
  }

  a1_v <- names(pressure_till_end_1)[order(pressure_till_end_1, decreasing = TRUE)]
  a2_v <- names(pressure_till_end_2)[order(pressure_till_end_2, decreasing = TRUE)]

  # we must take care of a case when both ses are the same singleton
  # we choose one with higher pressure, if equals it doesn't matter
  if (length(a1_v) == 1 && length(a2_v) == 1 && a1_v == a2_v) {
    if (pressure_till_end_1 >= pressure_till_end_2) {
      a2_v <- character()
      eta[2] <- Inf
    } else {
      a1_v <- character()
      eta[1] <- Inf
    }
  }

  # check upper bound for the best pressure - if we can't reach it
  # assume we could open valves one by one (every two minutes) in descending order
  tmp <- intersect(a1_v, a2_v)
  pressure_till_end <- c(
    pressure_till_end_1[setdiff(a1_v, a2_v)],
    pressure_till_end_2[setdiff(a2_v, a1_v)],
    pmax(pressure_till_end_1[tmp], pressure_till_end_2[tmp])
  )
  tmp <- floor(rank(-pressure_till_end, ties.method = "first") / 2 - 0.5)
  tmp <- pressure_till_end - tmp * 2 * valves$rates[names(pressure_till_end)]
  possible_best <- current_pressure + sum(pmax(tmp, 0))
  if (possible_best < current_best) return(0)


  # solve further - depending on who could choose next valve

  # could happen in some cases
  if (all(eta > 0)) {
    move_clock <- min(eta)
    time <- time - move_clock
    eta <- eta - move_clock
  }


  if (eta[1] == 0 && eta[2] > 0) {
    # agent 1 chooses, agent 2 is on the move

    for (a1 in a1_v) {

      new_valves <- valves
      next_current_valve <- current_valve
      new_eta <- eta

      next_current_valve[1] <- a1
      new_valves$open[a1] <- TRUE
      new_eta[1] <- v_d_1[a1] + 1
      new_valves$open_time[a1] <- time - new_eta[1]

      # time has to be corrected maximum by the target time of the other agent
      move_clock <- min(new_eta)
      new_time <- time - move_clock
      new_eta <- new_eta - move_clock

      out <- solve(new_valves, new_time, next_current_valve, current_best, new_eta)
      if (out > current_best) current_best <- out
    }


  } else if (eta[1] > 0 && eta[2] == 0) {
    # agent 1 is on the move, agent 2 chooses

    for (a2 in a2_v) {

      new_valves <- valves
      next_current_valve <- current_valve
      new_eta <- eta

      next_current_valve[2] <- a2
      new_valves$open[a2] <- TRUE
      new_eta[2] <- v_d_2[a2] + 1
      new_valves$open_time[a2] <- time - new_eta[2]

      # time has to be corrected maximum by the target time of the other agent
      move_clock <- min(new_eta)
      new_time <- time - move_clock
      new_eta <- new_eta - move_clock

      out <- solve(new_valves, new_time, next_current_valve, current_best, new_eta)
      if (out > current_best) current_best <- out
    }

  } else if (eta[1] == 0 && eta[2] == 0) {
    # agent 1 chooses, agent 2 chooses
    # how to choose????
    # maybe create all possible pairs, sort by combined pressure
    # or simpler, since we don't have stop condition anyway, simply use nested for loop

    for (a1 in a1_v) {

      for (a2 in setdiff(a2_v, a1)) {

        new_valves <- valves
        next_current_valve <- c(a1, a2)
        new_eta <- eta

        new_valves$open[next_current_valve] <- TRUE
        new_eta <- c(v_d_1[a1], v_d_2[a2]) + 1
        new_valves$open_time[next_current_valve] <- time - new_eta

        # time has to be corrected by smaller eta
        move_clock <- min(new_eta)
        new_time <- time - move_clock
        new_eta <- new_eta - move_clock

        out <- solve(new_valves, new_time, next_current_valve, current_best, new_eta)
        if (out > current_best) current_best <- out
      }
    }

  } else {
    # agent 1 is on the move, agent 2 is on the move
    # we should never end here
    browser()
    stop("something is wrong")
  }

  current_best
}

v_test <- read_input("input16_test.txt") |> prepare_valves()
solve(v_test, 26, current_valve = c("AA", "AA"), 0, integer(2))

v <- read_input("input16.txt") |> prepare_valves()
solve(v, 26, current_valve = c("AA", "AA"), 0, integer(2))
# 2772
