
# Part 1 ------------------------------------------------------------------


library(dplyr)


parseInput <- function(input) {
  tmp <- strsplit(input, " \\(contains ")
  res <- lapply(seq_along(tmp), function(i) {
    x <- tmp[[i]]
    ing <- unlist(strsplit(x[1], " "))
    alg <- unlist(strsplit(gsub("\\)", "", x[2]), ", "))
    expand.grid(id = i, ing = ing, alg = alg, stringsAsFactors = FALSE)
  })
  res <- do.call(rbind, res)
  res
}


matchAlg <- function(df) {
  # create a list of all allergend
  algs <- sort(unique(df$alg))
  
  # for each allergen, create a list of foods that contains it
  algs_ings <- lapply(algs, function(alg) {
    tmp <- sort(unique(df$id[df$alg == alg]))
    lapply(tmp, function(id) unique(df$ing[df$id == id]))
  })
  
  # for each allergen intersect foods
  algs_ings_intersect <- lapply(algs_ings, function(x) Reduce(intersect, x))
  algs_ings_intersect <- setNames(algs_ings_intersect, algs)
  
  # in loop count the number of options - those with one are hits
  algs_matched <- setNames(rep(NA_character_, length(algs)), algs)
  
  while (any(is.na(algs_matched))) {
    algs_ings_c <- sapply(algs_ings_intersect, length)
    tmp <- head(which(algs_ings_c == 1))
    if (length(tmp) == 0) stop("Something went wrong")
    
    alg_m <- names(algs_ings_intersect[tmp])
    ing_m <- algs_ings_intersect[tmp]
    
    # insert new match
    algs_matched[alg_m] <- ing_m
    
    # remove allergen
    algs_ings_intersect[alg_m] <- NULL
    
    # remove ingredient
    algs_ings_intersect <- lapply(algs_ings_intersect, setdiff, ing_m)
  }
  
  algs_matched
}




countNonAlg <- function(df, algs_matched) {
  tmp <- df[! df$ing %in% algs_matched, c("id", "ing")]
  nrow(unique(tmp))
}



solvePuzzle <- function(input) {
  
  df <- parseInput(input)
  
  alg_matched <- matchAlg(df)
  
  countNonAlg(df, alg_matched)
}


# example
example <- unlist(strsplit("mxmxvkd kfcds sqjhc nhms (contains dairy, fish)
trh fvjkl sbzzf mxmxvkd (contains dairy)
sqjhc fvjkl (contains soy)
sqjhc mxmxvkd sbzzf (contains fish)", "\n"))

solvePuzzle(example)


# run

input <- readLines("2020/inputs/input21.txt")
solvePuzzle(input)
# 2493



# Part 2 ------------------------------------------------------------------


# already done :)
df <- parseInput(input)
alg_matched <- matchAlg(df)
paste(unlist(alg_matched), collapse = ",")
# kqv,jxx,zzt,dklgl,pmvfzk,tsnkknk,qdlpbt,tlgrhdh
