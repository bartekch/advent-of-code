

read_input <- function(path) {
  l <- readLines(path)
  as.integer(substr(l, nchar(l[1]) - 1, nchar(l[1])))
}


play_game <- function(start) {
  # this is 100k, but we use values 0:99
  dice <- 0
  n <- 0
  # position are 1:10, but we use values 0:9
  pos <- (start - 1) %% 10
  score <- integer(2)
  player <- 1
  while (TRUE) {
    n <- n + 1
    values <- ((dice + 0:2) %% 100) + 1
    dice <- (dice + 3) %% 100
    pos[player] <- (pos[player] + sum(values)) %% 10
    score[player] <- score[player] + pos[player] + 1
    if (score[player] >= 1000) break
    player <- 3 - player
  }

  list(n = n, dice = dice, player = player, score = score, pos = pos)
}


test <- read_input("input21_test.txt")
res <- play_game(test)
res$n * 3 * res$score[3 - res$player]

input <- read_input("input21.txt")
res <- play_game(input)
res$n * 3 * res$score[3 - res$player]
# 1002474





# 2 -----------------------------------------------------------------------


# throws are 0:2
play_game2 <- function(pos, score, player, throw, win = 21) {

  out <- sapply(1:3, function(i) {
    # position is updated immediately
    pos[player] <- (pos[player] + i) %% 10

    # if this is the last throw update score, check for the win and change player
    # if the game continue
    if (throw == 2) {
      score[player] <- score[player] + pos[player] + 1

      if (score[player] >= win) {
        return(as.integer(score >= win))

      } else {
        player <- 3 - player
      }
    }
    throw <- (throw + 1) %% 3
    play_game2(pos, score, player, throw, win)
  })
  return(rowSums(out))
}

play_game2(pos = (test - 1) %% 10, score = integer(2), 1, 0, 8)
# this is infeasible for 21 as final score



play_game2_v2 <- function(pos, score, player, win = 21) {

  # results of three throws could be like the following, with the number of options
  results <- c(0, 0, 1, 3, 6, 7, 6, 3, 1) # 1:9 for easier indexing

  out <- sapply(3:9, function(i) {
    # position is updated immediately
    pos[player] <- (pos[player] + i) %% 10

    # check for the win
    score[player] <- score[player] + pos[player] + 1
    if (score[player] >= win) {
      # player win in every possible combination fo throws
      return(as.integer(score >= win) * results[i])
    }
    # change player and continue
    player <- 3 - player
    play_game2_v2(pos, score, player, win) * results[i]
  })
  return(rowSums(out))
}

system.time(
  print(play_game2_v2(pos = (test - 1) %% 10, score = integer(2), 1, 8))
)
system.time(
  print(play_game2_v2(pos = (test - 1) %% 10, score = integer(2), 1, 15))
)
system.time(
  print(play_game2_v2(pos = (test - 1) %% 10, score = integer(2), 1, 21))
)


res <- play_game2_v2(pos = (input - 1) %% 10, score = integer(2), 1, 21)
format(max(res), scientific = FALSE)
# 919758187195363
