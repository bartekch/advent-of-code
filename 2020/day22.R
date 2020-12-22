
# Part 1 ------------------------------------------------------------------

parseInput <- function(input) {
  input <- input[input != ""]
  pl <- grepl("Player", input)
  cards <- lapply(split(input, cumsum(pl)), tail, -1)
  cards <- setNames(lapply(cards, as.integer), c("pl1", "pl2"))
  cards
}


playCombat <- function(cards, maxit = 1e4) {
  i <- 0
  while (all(sapply(cards, length) > 0) & i <= maxit) {
    pl1_card <- cards$pl1[1]
    pl2_card <- cards$pl2[1]
    
    if (pl1_card > pl2_card) {
      cards$pl1 <- append(tail(cards$pl1, -1), c(pl1_card, pl2_card))
      cards$pl2 <- tail(cards$pl2, -1)
    } else {
      cards$pl1 <- tail(cards$pl1, -1)
      cards$pl2 <- append(tail(cards$pl2, -1), c(pl2_card, pl1_card))
    }
    
    i <- i + 1
  }
  
  if (all(sapply(cards, length) > 0)) {
    print("maxit hit")
  }
  
  cards
}


# example
ex_in <- unlist(strsplit("Player 1:
9
2
6
3
1

Player 2:
5
8
4
7
10", "\n"))


cards_ex <- parseInput(ex_in)

res_ex <- playCombat(cards_ex)
sum(res_ex$pl2 * rev(seq_along(res_ex$pl2)))
# 


# run

input <- readLines("inputs/input22.txt")
cards_run <- parseInput(input)

res <- playCombat(cards_run)
res_run <- res[[which(sapply(res, length) > 0)]]
sum(res_run * rev(seq_along(res_run)))
# 31308



### Rcpp implementation

library("Rcpp")

Rcpp::sourceCpp("day22.cpp")

# ex
res_ex_cpp <- playCombatCpp(1e4, cards_ex$pl1, cards_ex$pl2)
sum(res_ex_cpp * rev(seq_along(res_ex_cpp))) == sum(res_ex$pl2 * rev(seq_along(res_ex$pl2)))


# run
res_run_cpp <- playCombatCpp(1e4, cards_run$pl1, cards_run$pl2)
sum(res_run_cpp * rev(seq_along(res_run_cpp))) == sum(res_run * rev(seq_along(res_run)))


# speed comparison
library("microbenchmark")

bench <- microbenchmark::microbenchmark(
  playCombat(cards_run),
  playCombatCpp(1e4, cards_run$pl1, cards_run$pl2)
)
bench

ggplot2::autoplot(bench)
# massive improvement, as expected




# Part 2 ------------------------------------------------------------------

playRecursiveCombat <- function(cards, maxit = 1e4) {
  i <- 0
  
  previous_games <- list()
  
  while (all(sapply(cards, length) > 0) & i <= maxit) {
    
    # check identical cards condition
    if (any(sapply(previous_games, identical, cards))) {
      return(list(winner = "pl1", cards = cards))
    } else {
      previous_games <- append(previous_games, list(cards))
    }
    
    
    # draw new card
    pl1_card <- head(cards$pl1, 1)
    cards$pl1 <- tail(cards$pl1, -1)
    
    pl2_card <- head(cards$pl2, 1)
    cards$pl2 <- tail(cards$pl2, -1)
    
    
    # if both players have enough cards we play recursive
    # otherwise ordinary comparison
    if (length(cards$pl1) >= pl1_card && length(cards$pl2) >= pl2_card) {
      cards_cp <- cards
      cards_cp$pl1 <- head(cards_cp$pl1, pl1_card)
      cards_cp$pl2 <- head(cards_cp$pl2, pl2_card)
      
      res <- playRecursiveCombat(cards_cp)
      winner <- res$winner
      
    } else {
      if (pl1_card > pl2_card) {
        winner <- "pl1"
      } else {
        winner <- "pl2"
      }
    }

    if (winner == "pl1") {
      cards$pl1 <- append(cards$pl1, c(pl1_card, pl2_card))
    } else {
      cards$pl2 <- append(cards$pl2, c(pl2_card, pl1_card))
    }
    
    i <- i + 1
  }
  
  
  if (all(sapply(cards, length) > 0)) {
    stop("maxit hit")
  }

  
  return(list(winner = ifelse(length(cards$pl1) > 0, "pl1", "pl2"),
              cards = cards,
              it = i))
}


# example
res <- playRecursiveCombat(cards_ex)
res_ex <- res$cards[[which(sapply(res$cards, length) > 0)]]
sum(res_ex * rev(seq_along(res_ex)))
# 291


# run
system.time({
  res <- playRecursiveCombat(cards_run)
})

#    user   system  elapsed 
# 2115.982    2.758 2148.485 

# it takes A LOT of time
res_run <- res$cards[[which(sapply(res$cards, length) > 0)]]
sum(res_run * rev(seq_along(res_run)))
# 33647



### Rcpp implementation

Rcpp::sourceCpp("day22_2.cpp")


# ex
res_ex_cpp <- playRecursiveCombatCpp(1e4, cards_ex$pl1, cards_ex$pl2, 1)
sum(res_ex_cpp * rev(seq_along(res_ex_cpp))) == sum(res_ex * rev(seq_along(res_ex)))


# run
system.time({
  res_cpp <- playRecursiveCombatCpp(1e4, cards_run$pl1, cards_run$pl2, 1)
})
#    user  system elapsed 
# 229.989   0.136 233.205 

sum(res_cpp * rev(seq_along(res_cpp)))
# 33647



# time improvement is around ten times, less then in the first part and less
# then I expected, but my Rcpp code is probably rubbish

# speed comparison on example 
bench <- microbenchmark::microbenchmark(
  playRecursiveCombat(cards_ex),
  playRecursiveCombatCpp(1e4, cards_ex$pl1, cards_ex$pl2, 1),
  times = 1e3
)
bench

ggplot2::autoplot(bench)
# improvement is much better than for the real run, I guess example is much simpler
# especially in terms of recursion stop rule





## Remark
## ALWAYS READ THE INSTRUCTIONS CAREFULLY!!!!!!

