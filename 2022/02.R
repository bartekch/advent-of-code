
read_input <- function(path) {
  d <- readLines(path)
  data.frame(op = substr(d, 1, 1), m = substr(d, 3, 3))
}

# A for Rock, B for Paper, and C for Scissors.
# X for Rock, Y for Paper, and Z for Scissors


# 1 -----------------------------------------------------------------------

st <- read_input("input02.txt")

st$gpoints <- ifelse(
  st$op == "A",
  ifelse(
    st$m == "X",
    3,
    ifelse(
      st$m == "Y",
      6,
      0
    )
  ),
  ifelse(
    st$op == "B",
    ifelse(
      st$m == "X",
      0,
      ifelse(
        st$m == "Y",
        3,
        6
      )
    ),
    ifelse(
      st$m == "X",
      6,
      ifelse(
        st$m == "Y",
        0,
        3
      )
    )
  )
)

points <- c("X" = 1, "Y" = 2, "Z" = 3)
st$points <- st$gpoints + points[st$m]
sum(st$points)
# 10941




# 2 -----------------------------------------------------------------------


# second column says how the round needs to end
# X means lose, Y means draw, and Z win

# 1 for Rock, 2 for Paper, and 3 for Scissors

st <- read_input("input02.txt")

points_game <- c("X" = 0, "Y" = 3, "Z" = 6)
points_shape <- c("R" = 1, "P" = 2, "S" = 3)

st$response <- ifelse(
  st$op == "A",
  ifelse(
    st$m == "X",
    "S",
    ifelse(
      st$m == "Y",
      "R",
      "P"
    )
  ),
  ifelse(
    st$op == "B",
    ifelse(
      st$m == "X",
      "R",
      ifelse(
        st$m == "Y",
        "P",
        "S"
      )
    ),
    ifelse(
      st$m == "X",
      "P",
      ifelse(
        st$m == "Y",
        "S",
        "R"
      )
    )
  )
)

st$points <- points_game[st$m] + points_shape[st$response]
sum(st$points)
# 10941
