
# Part 1 ------------------------------------------------------------------


parseInput <- function(x) {
  do.call(
    rbind,
    strsplit( x, "")
  ) == "#"
}


# this is not very optimized but straightforward - and working!

countCubes <- function(initial_state, N = 6) {

  ni <- nrow(initial_state)

  # extend in every dimension - in each cycle the space increases by 1 in every direction
  # so by 2 in every dimension
  # and one extra step so we don't have to bother with edges at the last step
  space <- array(FALSE, dim = c(ni,ni,1) + 2*(N+1))
  space[N+1+1:ni, N+1+1:ni, N+1+1] <- initial_state

  # If a cube is active and exactly 2 or 3 of its neighbors are also active, the cube remains active. Otherwise, the cube becomes inactive.
  # If a cube is inactive but exactly 3 of its neighbors are active, the cube becomes active. Otherwise, the cube remains inactive.

  neighbours <- as.matrix(expand.grid(-1:1, -1:1, -1:1))
  dimnames(neighbours) <- NULL
  neighbours <- neighbours[apply(neighbours, 1, function(x) any(x != 0)),]

  for (i in 1:N) {
    new_space <- space
    for (x in (N+2-i):(N+1+ni+i)) {
      for (y in (N+2-i):(N+1+ni+i)) {
        for (z in (N+2-i):(N+2+i)) {
          neigh <- sum(space[neighbours + rep(c(x,y,z), each = nrow(neighbours))])
          if (space[x,y,z] && !neigh %in% 2:3) {
            new_space[x,y,z] <- FALSE
          } else if (!space[x,y,z] && neigh == 3) {
            new_space[x,y,z] <- TRUE
          }
        }
      }
    }
    space <- new_space
  }

  sum(space)
}


example <- parseInput(strsplit(
  ".#.
..#
###",
  "\n")[[1]])

countCubes(example)


input <- parseInput(readLines("inputs/input17.txt"))

countCubes(input)
# 230



# Part 2 ------------------------------------------------------------------


countCubes4D <- function(initial_state, N = 6) {

  ni <- nrow(initial_state)

  # extend in every dimension - in each cycle the space increases by 1 in every direction
  # so by 2 in every dimension
  # and one extra step so we don't have to bother with edges at the last step
  space <- array(FALSE, dim = c(ni,ni,1,1) + 2*(N+1))
  space[N+1+1:ni, N+1+1:ni, N+1+1, N+1+1] <- initial_state

  # If a cube is active and exactly 2 or 3 of its neighbors are also active, the cube remains active. Otherwise, the cube becomes inactive.
  # If a cube is inactive but exactly 3 of its neighbors are active, the cube becomes active. Otherwise, the cube remains inactive.

  neighbours <- as.matrix(expand.grid(-1:1, -1:1, -1:1, -1:1))
  dimnames(neighbours) <- NULL
  neighbours <- neighbours[apply(neighbours, 1, function(x) any(x != 0)),]

  for (i in 1:N) {
    new_space <- space
    for (x in (N+2-i):(N+1+ni+i)) {
      for (y in (N+2-i):(N+1+ni+i)) {
        for (z in (N+2-i):(N+2+i)) {
          for (w in (N+2-i):(N+2+i)) {
            neigh <- sum(space[neighbours + rep(c(x,y,z,w), each = nrow(neighbours))])
            if (space[x,y,z,w] && !neigh %in% 2:3) {
              new_space[x,y,z,w] <- FALSE
            } else if (!space[x,y,z,w] && neigh == 3) {
              new_space[x,y,z,w] <- TRUE
            }
          }
        }
      }
    }
    space <- new_space
  }

  sum(space)
}

countCubes4D(example)

countCubes4D(input)
# 1600
