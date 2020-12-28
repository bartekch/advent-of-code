
library("purrr")
library("dplyr")

# Part 1 ------------------------------------------------------------------

# replace some directions so as all are represented by single letters so it is easier to split
# SW - z
# SE - c
# NW - q
# NE - r
parseInput <- function(input) {
  input <- gsub("sw", "z", input)
  input <- gsub("se", "c", input)
  input <- gsub("nw", "q", input)
  input <- gsub("ne", "r", input)

  strsplit(input, "") %>%
    purrr::map_dfr(~{
      data.frame(direction = .)
    }, .id = "id")
}



# each direction is represented by the change in coordinates
# coordinates indicates the middle of a hex, where east-west neighbours are horizontally
# two points apart, and the rest are one point horizontally and one point vertically
directions <- data.frame(
  direction = c("w", "q", "r", "e", "c", "z"),
  delta_x =   c( -2,  -1,   1,   2,   1,  -1),
  delta_y =   c(   0,  1,   1,   0,  -1,  -1)
)


identifyTiles <- function(paths) {
  paths %>%
    inner_join(directions, by = "direction") %>%
    group_by(id) %>%
    summarise(x = sum(delta_x), y = sum(delta_y), .groups = "drop")
}


calculateBlackTiles <- function(tiles) {
  # those visited odd number of times are black
  tiles %>%
    add_count(x, y) %>%
    filter(n %% 2 == 1) %>%
    nrow()
}


solvePuzzle <- function(input) {
  paths <- parseInput(input)
  tiles <- identifyTiles(paths)
  calculateBlackTiles(tiles)
}



## example

example <- unlist(strsplit("sesenwnenenewseeswwswswwnenewsewsw
neeenesenwnwwswnenewnwwsewnenwseswesw
seswneswswsenwwnwse
nwnwneseeswswnenewneswwnewseswneseene
swweswneswnenwsewnwneneseenw
eesenwseswswnenwswnwnwsewwnwsene
sewnenenenesenwsewnenwwwse
wenwwweseeeweswwwnwwe
wsweesenenewnwwnwsenewsenwwsesesenwne
neeswseenwwswnwswswnw
nenwswwsewswnenenewsenwsenwnesesenew
enewnwewneswsewnwswenweswnenwsenwsw
sweneswneswneneenwnewenewwneswswnese
swwesenesewenwneswnwwneseswwne
enesenwswwswneneswsenwnewswseenwsese
wnwnesenesenenwwnenwsewesewsesesew
nenewswnwewswnenesenwnesewesw
eneswnwswnwsenenwnwnwwseeswneewsenese
neswnwewnwnwseenwseesewsenwsweewe
wseweeenwnesenwwwswnew
", "\n"))


solvePuzzle(example)


## run

input <- readLines("inputs/input24.txt")

solvePuzzle(input)
# 346




# Part 2 ------------------------------------------------------------------

recolorTiles <- function(tiles) {
  tiles$is_black <- TRUE

  # for every (black) tile, get coordinates of its neighbours
  neighbours <- tiles %>%
    full_join(directions, by = character()) %>%
    mutate(x_n = x + delta_x, y_n = y + delta_y) %>%
    select(x, y, x_n, y_n)


  # apply rule to existing black tiles
  black_tiles <- neighbours %>%
    left_join(tiles, by = c("x_n" = "x", "y_n" = "y")) %>%
    group_by(x, y) %>%
    summarise(black_n = sum(is_black, na.rm = TRUE), .groups = "drop") %>%
    filter(black_n %in% 1:2) %>%
    select(x, y)

  # remove existing black tiles from the neighbours set - those are white tiles
  # that need to be checked
  neighbours <- neighbours %>%
    distinct(x = x_n, y = y_n) %>%
    anti_join(tiles, by = c("x", "y"))

  # get neighbours fo those tiles
  neighbours <- neighbours %>%
    full_join(directions, by = character()) %>%
    mutate(x_n = x + delta_x, y_n = y + delta_y) %>%
    select(x, y, x_n, y_n)

  # apply rule to white tiles
  white_tiles <- neighbours %>%
    left_join(tiles, by = c("x_n" = "x", "y_n" = "y")) %>%
    group_by(x, y) %>%
    summarise(black_n = sum(is_black, na.rm = TRUE), .groups = "drop") %>%
    filter(black_n == 2) %>%
    select(x, y)


  #
  rbind(black_tiles, white_tiles)
}


# we keep track only of black tile
solvePuzzle2 <- function(input, times = 100) {
  # initialize
  paths <- parseInput(input)
  tiles <- identifyTiles(paths)
  tiles <- tiles %>%
    add_count(x, y) %>%
    filter(n %% 2 == 1) %>%
    select(x, y)

  #
  for (i in seq_len(times)) {
    tiles <- recolorTiles(tiles)
  }


  #
  nrow(tiles)
}


# example
solvePuzzle2(example, 100)



# run
input <- readLines("inputs/input24.txt")
solvePuzzle2(input, 100)
# 3802
