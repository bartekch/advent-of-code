


read_input <- function(path) {
  bl <- readLines(path)
  lapply(seq_along(bl), function(i) {
    l <- bl[[i]]
    tmp <- regexpr("Each ore robot costs", l)
    ore_cost <- as.integer(substr(l, tmp + 21, tmp + 21))

    tmp <- regexpr("Each clay robot costs", l)
    clay_cost <- as.integer(substr(l, tmp + 22, tmp + 22))

    tmp <- regexpr("Each obsidian robot costs", l)
    obsidian_ore_cost <- as.integer(substr(l, tmp + 26, tmp + 26))
    obsidian_clay_cost <- as.integer(trimws(substr(l, tmp + 36, tmp + 37)))

    tmp <- regexpr("Each geode robot costs", l)
    geode_ore_cost <- as.integer(substr(l, tmp + 23, tmp + 23))
    geode_obsidian_cost <- as.integer(trimws(substr(l, tmp + 33, tmp + 34)))


    list(
      id = i,
      ore_robot = ore_cost,
      clay_robot = clay_cost,
      obsidian_robot = c(obsidian_ore_cost, obsidian_clay_cost),
      geode_robot = c(geode_ore_cost, geode_obsidian_cost),
      max_robots = c(
        max(ore_cost, clay_cost, obsidian_ore_cost, geode_ore_cost),
        obsidian_clay_cost,
        geode_obsidian_cost
      )
    )
  })
}



# we could start by collecting 2 ore
mine <- function(
    bl, # blueprint
    robots = c(1, 0, 0, 0), # ore, clay, obsidian, geode
    resources = c(2, 0, 0, 0), # as above
    time = 22,
    best = 0, # best result so far,
    make_sense = rep(TRUE, 4)
) {
  if (time == 1) return(max(best, resources[4] + robots[4]))

  # if we cannot build geode robot at time = 2
  # we are left with robots we already have
  if (time == 2 && (bl$geode_robot[1] > resources[1] || bl$geode_robot[2] > resources[3])) {
    return(max(best, resources[4] + 2 * robots[4]))
  }

  # assume we build new geode robot every minute until the end
  # this is the best possible outcome
  # so we will produce time robots, and each will produce t-1 geodes
  # total is sum 1 ... t-1
  possible_best <- resources[4] + time * robots[4] + (time - 1) * time / 2
  if (possible_best <= best) return(best)


  # select every option to build
  # building nothing is also an option!!
  # start with the most expensive first, than promote waiting,
  # than cheaper ones

  # do not build when it doesn't make sense
  # time 1 - collect geode
  # time 2 - build geode
  # time 3 - collect ore and obs for building geode
  # time 4 - build ore and obs
  # time 5 - collect clay for building obs
  # time 6 - build clay


  # first check whether it makes sense to build a robot
  # in terms of time and maximal value
  # it doesn't make sense to build more than could be used in single turn
  sense_geode <- make_sense[4]
  sense_obs <- make_sense[3] && time >= 4 && robots[3] < bl$max_robots[3]
  sense_clay <- make_sense[2] && time >= 6 && robots[2] < bl$max_robots[2]
  sense_ore <- make_sense[1] && time >= 4 && robots[1] < bl$max_robots[1]

  # then check whether robot could be built in terms of resources
  # if it doesn't make sense to build it's always true
  av_geode <- !sense_geode || (bl$geode_robot[1] <= resources[1] && bl$geode_robot[2] <= resources[3])
  av_obs <- !sense_obs || (bl$obsidian_robot[1] <= resources[1] && bl$obsidian_robot[2] <= resources[2])
  av_clay <- !sense_clay || (bl$clay_robot <= resources[1])
  av_ore <- !sense_ore || (bl$ore_robot <= resources[1])



  if (sense_geode && av_geode) {
    new_resources <- resources + robots
    new_resources[1] <- new_resources[1] - bl$geode_robot[1]
    new_resources[3] <- new_resources[3] - bl$geode_robot[2]
    new_robots <- robots
    new_robots[4] <- new_robots[4] + 1

    build_geode <- Recall(bl, new_robots, new_resources, time - 1, best)
    best <- max(best, build_geode)
  }

  if (sense_obs && av_obs) {
    new_resources <- resources + robots
    new_resources[1] <- new_resources[1] - bl$obsidian_robot[1]
    new_resources[2] <- new_resources[2] - bl$obsidian_robot[2]
    new_robots <- robots
    new_robots[3] <- new_robots[3] + 1

    build_obs <- Recall(bl, new_robots, new_resources, time - 1, best)
    best <- max(best, build_obs)
  }

  # waiting is valid option ONLY if there is some robot that cannot be built now
  # moreover, if we can build a robot now, but decide to wait,
  #  it doesn't make sense to build it in the next round!
  #  so we should pass available robots down the tree
  # this is CRUCIAL pruning!
  cond1 <- !(av_geode && av_obs && av_clay && av_ore)
  # also, it doesn't make sense to wait, if we have only ore robots (clay robots = 0) and
  #   have enough resources to build ore or clay robot
  cond2 <- cond1 && !(robots[2] == 0 && av_ore && av_clay)
  # similarly, if we don't have obsidian robots, but have resources to build all three
  # we shouldn't wait
  cond3 <- cond2 && !(robots[3] == 0 && av_ore && av_clay && av_obs)
  if (cond3) {
    new_resources <- resources + robots
    build_nothing <- Recall(bl, robots, new_resources, time - 1, best, !c(av_ore, av_clay, av_obs, av_geode))
    best <- max(best, build_nothing)
  }


  if (sense_clay && av_clay) {
    new_resources <- resources + robots
    new_resources[1] <- new_resources[1] - bl$clay_robot
    new_robots <- robots
    new_robots[2] <- new_robots[2] + 1

    build_clay <- Recall(bl, new_robots, new_resources, time - 1, best)
    best <- max(best, build_clay)
  }

  if (sense_ore && av_ore) {
    new_resources <- resources + robots
    new_resources[1] <- new_resources[1] - bl$ore_robot
    new_robots <- robots
    new_robots[1] <- new_robots[1] + 1

    build_ore <- Recall(bl, new_robots, new_resources, time - 1, best)
    best <- max(best, build_ore)
  }

  best
}

bl_test <- read_input("input19_test.txt")
mine(bl_test[[1]])
mine(bl_test[[2]])

bl <- read_input("input19.txt")
quality <- lapply(seq_along(bl), function(i) {
  out <- mine(bl[[i]])
  # message(Sys.time(), " id ", bl[[i]]$id, " out ", out)
  out * bl[[i]]$id
})
sum(unlist(quality))
# 1092



# 2 -----------------------------------------------------------------------

mine(bl_test[[1]], time = 30)
mine(bl_test[[2]], time = 30)

out <- sapply(1:3, function(i) {
  mine(bl[[i]], time = 30)
})
out
# 14 11 23
prod(out)
# 3542




# we could try to speed it up by incrementing time one by one and
# using previous best value as a starting point
system.time(mine(bl_test[[1]], time = 30))
# user  system elapsed
# 1.99    0.02    2.06
system.time({
  previous_best <- mine(bl_test[[1]])
  for (time in 23:30) {
    previous_best <- mine(bl_test[[1]], time = time, best = previous_best)
  }
})
# user  system elapsed
# 6.00    0.00    6.14
previous_best


system.time(mine(bl_test[[2]], time = 30))
# user  system elapsed
# 4.69    0.00    4.81
system.time({
  previous_best <- mine(bl_test[[2]])
  for (time in 23:30) {
    previous_best <- mine(bl_test[[2]], time = time, best = previous_best)
  }
})
# user  system elapsed
# 6.83    0.04    6.98
previous_best


system.time({
  sapply(1:3, function(i) {
    mine(bl[[i]], time = 30)
  })
})
# user  system elapsed
# 6.78    0.00    6.92
system.time({
  sapply(1:3, function(i) {
    previous_best <- mine(bl[[i]])
    for (time in 23:30) {
      previous_best <- mine(bl[[i]], time = time, best = previous_best)
    }
    previous_best
  })
})
#  user  system elapsed
# 14.74    0.00   15.14

# slower in all cases!








# choose next robot implementation ----------------------------------------

mine2 <- function(
    bl, # blueprint
    robots = c(1, 0, 0, 0), # ore, clay, obsidian, geode, at the beginning of turn
    resources = c(2, 0, 0, 0), # as above, at the beginning of turn
    time = 22, # time left to 0
    best = 0 # best result so far
) {
  if (time == 1) return(max(best, resources[4] + robots[4]))

  # if we cannot build geode robot at time = 2
  # we are left with robots we already have
  if (time == 2 && (bl$geode_robot[1] > resources[1] || bl$geode_robot[2] > resources[3])) {
    return(max(best, resources[4] + 2 * robots[4]))
  }

  # assume we build new geode robot every minute until the end
  # this is the best possible outcome
  # so we will produce time robots, and each will produce t-1 geodes
  # total is sum 1 ... t-1
  possible_best <- resources[4] + time * robots[4] + (time - 1) * time / 2
  if (possible_best <= best) return(best)

  # default - do nothing until the end
  best <- max(best, resources[4] + time * robots[4])

  # choose what to build next, instead of deciding what to do in this minute
  # start with the most expensive first, than cheaper ones

  # first check whether it makes sense to build a robot in terms of their number
  # then calculate when enough resources will be available
  # then check whether it makes sense to build a robot in terms of time

  # time 1 - collect geode
  # time 2 - build geode
  # time 3 - collect ore and obs for building geode
  # time 4 - build ore and obs
  # time 5 - collect clay for building obs
  # time 6 - build clay

  # for geode robots there is no maximum value
  # if we don't have obsidian robots, we cannot build geode robot
  if (robots[3] > 0) {
    time_left <- max(
      0, # enough resources already
      ceiling((bl$geode_robot[1] - resources[1]) / robots[1]), # ore
      ceiling((bl$geode_robot[2] - resources[3]) / robots[3]) # obsidian
    )
    # check for time
    if (time - time_left >= 2) {
      time_left <- time_left + 1 # one minute to build robot
      new_resources <- resources + robots * time_left
      new_resources[1] <- new_resources[1] - bl$geode_robot[1]
      new_resources[3] <- new_resources[3] - bl$geode_robot[2]
      new_robots <- robots
      new_robots[4] <- new_robots[4] + 1
      build_geode <- Recall(bl, new_robots, new_resources, time - time_left, best)
      best <- max(best, build_geode)
    }
  }


  # if we don't have clay robots, we cannot build obsidian robot
  if (robots[2] > 0 && robots[3] < bl$max_robots[3]) {
    time_left <- max(
      0, # enough resources already
      ceiling((bl$obsidian_robot[1] - resources[1]) / robots[1]), # ore
      ceiling((bl$obsidian_robot[2] - resources[2]) / robots[2]) # clay
    )
    # check for time
    if (time - time_left >= 4) {
      time_left <- time_left + 1 # one minute to build robot
      new_resources <- resources + robots * time_left
      new_resources[1] <- new_resources[1] - bl$obsidian_robot[1]
      new_resources[2] <- new_resources[2] - bl$obsidian_robot[2]
      new_robots <- robots
      new_robots[3] <- new_robots[3] + 1
      build_obs <- Recall(bl, new_robots, new_resources, time - time_left, best)
      best <- max(best, build_obs)
    }
  }


  if (robots[2] < bl$max_robots[2]) {
    time_left <- max(
      0, # enough resources already
      ceiling((bl$clay_robot[1] - resources[1]) / robots[1]) # ore
    )
    # check for time
    if (time - time_left >= 6) {
      time_left <- time_left + 1 # one minute to build robot
      new_resources <- resources + robots * time_left
      new_resources[1] <- new_resources[1] - bl$clay_robot[1]
      new_robots <- robots
      new_robots[2] <- new_robots[2] + 1
      build_clay <- Recall(bl, new_robots, new_resources, time - time_left, best)
      best <- max(best, build_clay)
    }
  }


  if (robots[1] < bl$max_robots[1]) {
    time_left <- max(
      0, # enough resources already
      ceiling((bl$ore_robot[1] - resources[1]) / robots[1]) # ore
    )
    # check for time
    if (time - time_left >= 4) {
      time_left <- time_left + 1 # one minute to build robot
      new_resources <- resources + robots * time_left
      new_resources[1] <- new_resources[1] - bl$ore_robot[1]
      new_robots <- robots
      new_robots[1] <- new_robots[1] + 1
      build_ore <- Recall(bl, new_robots, new_resources, time - time_left, best)
      best <- max(best, build_ore)
    }
  }

  best
}


bl_test <- read_input("input19_test.txt")
mine2(bl_test[[1]])
mine2(bl_test[[2]])

bl <- read_input("input19.txt")
quality <- lapply(seq_along(bl), function(i) {
  out <- mine2(bl[[i]])
  # message(Sys.time(), " id ", bl[[i]]$id, " out ", out)
  out * bl[[i]]$id
})
sum(unlist(quality))
# 1092


microbenchmark::microbenchmark(
  mine(bl_test[[1]]),
  mine2(bl_test[[1]]),
  times = 10
)

microbenchmark::microbenchmark(
  mine(bl_test[[2]]),
  mine2(bl_test[[2]]),
  times = 10
)

microbenchmark::microbenchmark(
  lapply(seq_along(bl), function(i) {
    out <- mine(bl[[i]])
    out * bl[[i]]$id
  }),
  lapply(seq_along(bl), function(i) {
    out <- mine2(bl[[i]])
    out * bl[[i]]$id
  }),
  times = 10
)
# significantly faster!


# part 2

mine2(bl_test[[1]], time = 30)
mine2(bl_test[[2]], time = 30)

out <- sapply(1:3, function(i) {
  mine2(bl[[i]], time = 30)
})
out
# 14 11 23
prod(out)
# 3542


microbenchmark::microbenchmark(
  mine(bl_test[[1]], time = 30),
  mine2(bl_test[[1]], time = 30),
  times = 10
)
# in this case slower - interesting

microbenchmark::microbenchmark(
  mine(bl_test[[2]], time = 30),
  mine2(bl_test[[2]], time = 30),
  times = 10
)
# slightly faster

microbenchmark::microbenchmark(
  sapply(1:3, function(i) {
    mine(bl[[i]], time = 30)
  }),
  sapply(1:3, function(i) {
    mine2(bl[[i]], time = 30)
  }),
  times = 10
)
# slightly faster

# in summary - method is slightly faster, but not always
