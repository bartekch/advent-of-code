
read_input <- function(path) {
  l <- readLines(path)

  action <- substr(l, 1, 2) == "on"
  out <- do.call(
    rbind,
    lapply(strsplit(gsub("on |off |x=|y=|z=", "", l), ",|\\.\\."), function(x) {
      x<- as.integer(x)
      data.frame(x1 = x[1], x2 = x[2], y1 = x[3], y2 = x[4], z1 = x[5], z2 = x[6])
    })
  )
  out$action <- action
  out
}




# 1 -----------------------------------------------------------------------

reboot_step <- function(cubes, step) {
  ind <- cubes$x >= step$x1 & cubes$x <= step$x2 &
    cubes$y >= step$y1 & cubes$y <= step$y2 &
    cubes$z >= step$z1 & cubes$z <= step$z2

  # remove (switch off) all cubes withing range
  cubes <- cubes[!ind, ]

  # if action is on then add all cubes within target
  if (step$action) {
    new_cubes <- expand.grid(
      x = step$x1:step$x2,
      y = step$y1:step$y2,
      z = step$z1:step$z2
    )
    cubes <- rbind(cubes, new_cubes)
  }
  cubes
}


reboot <- function(steps) {
  # remove steps falling outside initial range -50:50
  steps <- steps[steps$x1 <= 50 & steps$x2 >= -50 &
                   steps$y1 <= 50 & steps$y2 >= -50 &
                   steps$z1 <= 50 & steps$z2 >= -50, ]

  cubes <- data.frame(x = integer(), y = integer(), z = integer())

  for (i in seq_len(nrow(steps))) {
    cubes <- reboot_step(cubes, steps[i, ])
  }
  cubes
}


test <- read_input("input22_test2.txt")
res <- reboot(test)
nrow(res)

test <- read_input("input22_test.txt")
res <- reboot(test)
nrow(res)

input <- read_input("input22.txt")
res <- reboot(input)
nrow(res)
# 648681



# 2 -----------------------------------------------------------------------

# the first solution would be infeasible - ranges are too big
# instead we should keep track on switched on cuboids
# when adding/ removing we need to slice them accordingly

split_dim <- function(cuboid, common) {
  org_names <- names(common)
  common <- setNames(common, c("d1", "d2"))
  cuboid <- setNames(cuboid, c("d1", "d2"))

  # common part could cover the whole range, which result in single part
  # from one border to the middle - two parts
  # completely in the middle - three parts

  if (common$d1 > cuboid$d1 & common$d2 < cuboid$d2) {
    split <- data.frame(
      d1 = c(cuboid$d1,     common$d1, common$d2 + 1),
      d2 = c(common$d1 - 1, common$d2, cuboid$d2)
    )
  } else if (common$d1 == cuboid$d1 & common$d2 < cuboid$d2) {
    split <- data.frame(
      d1 = c(cuboid$d1, common$d2 + 1),
      d2 = c(common$d2, cuboid$d2)
    )
  } else if (common$d1 > cuboid$d1 & common$d2 == cuboid$d2) {
    split <- data.frame(
      d1 = c(cuboid$d1,     common$d1),
      d2 = c(common$d1 - 1, cuboid$d2)
    )
  } else {
    split <- data.frame(
      d1 = cuboid$d1,
      d2 = cuboid$d2
    )
  }
  setNames(split, org_names)
}


switchoff <- function(cuboid, target) {
  # find common part
  common <- data.frame(
    x1 = max(cuboid$x1, target$x1),
    x2 = min(cuboid$x2, target$x2),
    y1 = max(cuboid$y1, target$y1),
    y2 = min(cuboid$y2, target$y2),
    z1 = max(cuboid$z1, target$z1),
    z2 = min(cuboid$z2, target$z2)
  )

  if (common$x1 > common$x2 || common$y1 > common$y2 || common$z1 > common$z2) return(cuboid)


  x_split <- split_dim(cuboid[, c("x1", "x2")], common[, c("x1", "x2")])
  y_split <- split_dim(cuboid[, c("y1", "y2")], common[, c("y1", "y2")])
  z_split <- split_dim(cuboid[, c("z1", "z2")], common[, c("z1", "z2")])

  all_splits <- merge(merge(x_split, y_split), z_split)
  # omit common part
  all_splits <- all_splits[all_splits$x1 != common$x1 |
                             all_splits$x2 != common$x2 |
                             all_splits$y1 != common$y1 |
                             all_splits$y2 != common$y2 |
                             all_splits$z1 != common$z1 |
                             all_splits$z2 != common$z2, ]
  all_splits
}


split_dim(data.frame(x1 = 0, x2 = 10), data.frame(x1 = 0, x2 = 10))
split_dim(data.frame(x1 = -10, x2 = 10), data.frame(x1 = 0, x2 = 10))
split_dim(data.frame(x1 = 0, x2 = 20), data.frame(x1 = 0, x2 = 10))
split_dim(data.frame(x1 = -10, x2 = 20), data.frame(x1 = 0, x2 = 10))
split_dim(data.frame(x1 = 0, x2 = 1), data.frame(x1 = 1, x2 = 1))


switchoff(
  data.frame(x1 = 0, x2 = 5, y1 = 0, y2 = 5, z1 = 0, z2 = 5),
  data.frame(x1 = 10, x2 = 15, y1 = 0, y2 = 5, z1 = 0, z2 = 5)
)
switchoff(
  data.frame(x1 = 0, x2 = 5, y1 = 0, y2 = 5, z1 = 0, z2 = 5),
  data.frame(x1 = 0, x2 = 5, y1 = 0, y2 = 5, z1 = 0, z2 = 5)
)
switchoff(
  data.frame(x1 = 0, x2 = 5, y1 = 0, y2 = 5, z1 = 0, z2 = 5),
  data.frame(x1 = 2, x2 = 3, y1 = 2, y2 = 3, z1 = 2, z2 = 3)
)
switchoff(
  data.frame(x1 = 0, x2 = 1, y1 = 0, y2 = 1, z1 = 0, z2 = 1),
  data.frame(x1 = 1, x2 = 2, y1 = 1, y2 = 2, z1 = 1, z2 = 2)
)
switchoff(
  data.frame(x1 = 0, x2 = 2, y1 = 0, y2 = 2, z1 = 0, z2 = 2),
  data.frame(x1 = 1, x2 = 3, y1 = 1, y2 = 3, z1 = 1, z2 = 3)
)


reboot2 <- function(steps) {
  cuboids <- steps[0, -which(colnames(steps) == "action")]

  for (i in seq_len(nrow(steps))) {
    step <- steps[i, ]

    if (step$action && nrow(cuboids) == 0) {
      cuboids <- step[, -which(colnames(step) == "action")]
      next
    }

    # iterate through cuboids
    out <- lapply(seq_len(nrow(cuboids)), function(j) {
      # existing cuboid minus common part with target
      switchoff(cuboids[j, ], step)
    })

    cuboids <- do.call(rbind, out)
    # if action is on we need to add target
    if (step$action) {
      cuboids <- rbind(cuboids, step[, -which(colnames(step) == "action")])
    }
  }
  cuboids
}

calculate_cubes <- function(cuboids) {
  res <- sapply(seq_len(nrow(cuboids)), function(i) {
    cub <- cuboids[i,]
    (cub$x2 - cub$x1 + 1) * (cub$y2 - cub$y1 + 1) * (cub$z2 - cub$z1 + 1)
  })
  sum(res)
}


test <- read_input("input22_test2.txt")
res <- reboot2(test)
calculate_cubes(res)

test <- read_input("input22_test.txt")
res <- reboot2(test)
format(calculate_cubes(res), scientific = FALSE)

test <- read_input("input22_test3.txt")
res <- reboot2(test)
format(calculate_cubes(res), scientific = FALSE)



input <- read_input("input22.txt")
res <- reboot2(input)
format(calculate_cubes(res), scientific = FALSE)
# 1302784472088899
