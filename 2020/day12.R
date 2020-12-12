
# Part 1 ------------------------------------------------------------------


# Action N means to move north by the given value.
# Action S means to move south by the given value.
# Action E means to move east by the given value.
# Action W means to move west by the given value.
# Action L means to turn left the given number of degrees.
# Action R means to turn right the given number of degrees.
# Action F means to move forward by the given value in the direction the ship is currently facing.
# The ship starts by facing east.

# What is the Manhattan distance between that location and the ship's starting position?


route <- readLines("2020/inputs/input12.txt")

route <- data.frame(instruction = substr(route, 1, 1),
                    value = as.integer(substr(route, 2, nchar(route))))

rotations <- list(
  "L" = list(
    "0" = diag(2),
    "90" = matrix(c(0,-1,1,0), byrow = TRUE, ncol = 2),
    "180" = -diag(2),
    "270" = matrix(c(0,1,-1,0), byrow = TRUE, ncol = 2)
  ),
  "R" = list(
    "0" = diag(2),
    "90" = matrix(c(0,1,-1,0), byrow = TRUE, ncol = 2),
    "180" = -diag(2),
    "270" = matrix(c(0,-1,1,0), byrow = TRUE, ncol = 2)
  )
)

pos <- list(en = c(0, 0), facing = as.matrix(c(1, 0)))

for (i in seq_len(nrow(route))) {
  inst <- route$instruction[i]
  val <- route$value[i]
  
  if (inst == "N" ) {
    pos$en <- pos$en + c(0, val)
  } else if (inst == "S") {
    pos$en <- pos$en + c(0, -val)
  } else if (inst == "E") {
    pos$en <- pos$en + c(val, 0)
  } else if (inst == "W") {
    pos$en <- pos$en + c(-val, 0)
  } else if (inst == "F") {
    pos$en <- pos$en + pos$facing * val
  } else {
    pos$facing <- rotations[[inst]][[as.character(val)]] %*% pos$facing
  } 
}
pos
sum(abs(pos$en))



# Part 2 ------------------------------------------------------------------



pos <- list(en = c(0, 0), waypoint = as.matrix(c(10, 1)))

for (i in seq_len(nrow(route))) {
  inst <- route$instruction[i]
  val <- route$value[i]
  
  if (inst == "N" ) {
    pos$waypoint <- pos$waypoint + c(0, val)
  } else if (inst == "S") {
    pos$waypoint <- pos$waypoint + c(0, -val)
  } else if (inst == "E") {
    pos$waypoint <- pos$waypoint + c(val, 0)
  } else if (inst == "W") {
    pos$waypoint <- pos$waypoint + c(-val, 0)
  } else if (inst == "F") {
    pos$en <- pos$en + pos$waypoint * val
  } else {
    pos$waypoint <- rotations[[inst]][[as.character(val)]] %*% pos$waypoint
  } 
}
pos
sum(abs(pos$en))
# 28591
