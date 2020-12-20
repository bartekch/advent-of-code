
# Part 1 ------------------------------------------------------------------

generateTiles <- function(input) {
  tiles_starts <- grep("Tile", input)
  tiles_ends <- c(which(input == "") - 1, length(input))

  tiles <- lapply(seq_along(tiles_starts), function(i) {
    do.call(rbind, strsplit(input[(tiles_starts[i] + 1):tiles_ends[i]], ""))
  })
  tiles <- setNames(tiles, gsub("Tile |:", "", input[tiles_starts]))
  tiles <- tiles[order(names(tiles))]

  tiles
}



checkTileEdge <- function(edge, tile) {
  n <- length(edge)

  if (all(edge == tile[1, ])) {
    return("top")
  } else if (all(edge == rev(tile[1, ]))) {
    return("top_rev")
  } else if (all(edge == tile[n, ])) {
    return("bottom")
  } else if (all(edge == rev(tile[n, ]))) {
    return("bottom_rev")
  } else if (all(edge == tile[, 1])) {
    return("left")
  } else if (all(edge == rev(tile[, 1]))) {
    return("left_rev")
  } else if (all(edge == tile[, n])) {
    return("right")
  } else if (all(edge == rev(tile[, n]))) {
    return("right_rev")
  }
  return(NULL)
}



checkTilesPair <- function(t1,t2) {
  n <- nrow(t1)

  # check each edge from the first tile with all the combinations of edges
  # from the second tile

  tmp <- checkTileEdge(t1[1, ], t2)
  if (!is.null(tmp)) return(c("top", tmp))

  tmp <- checkTileEdge(t1[n, ], t2)
  if (!is.null(tmp)) return(c("bottom", tmp))

  tmp <- checkTileEdge(t1[, 1], t2)
  if (!is.null(tmp)) return(c("left", tmp))

  tmp <- checkTileEdge(t1[, n], t2)
  if (!is.null(tmp)) return(c("right", tmp))

  return(NULL)
}



checkAllPairs <- function(tiles) {
  res <- data.frame()
  for (i in seq_len(length(tiles) - 1)) {
    for (j in (i + 1):length(tiles)) {
      tmp <- checkTilesPair(tiles[[i]], tiles[[j]])
      if (!is.null(tmp)) {
        res <- rbind(res, data.frame(t1_id = names(tiles)[i],
                                     t1_edge = tmp[1],
                                     t2_id = names(tiles)[j],
                                     t2_edge = tmp[2]))
      }
    }
  }
  res
}




findCorners <- function(pairs) {
  tmp <- table(c(pairs$t1_id, pairs$t2_id))
  as.integer(names(tmp)[which(tmp == 2)])
}




# example
tiles_ex <- generateTiles(readLines("inputs/input20_ex1.txt"))
all_pairs_ex <- checkAllPairs(tiles_ex)

format(prod(findCorners(all_pairs_ex)), scientific = FALSE)




# run
tiles <- generateTiles(readLines("inputs/input20.txt"))
all_pairs <- checkAllPairs(tiles)
corners <- findCorners(all_pairs)
format(prod(corners), scientific = FALSE)
# 8272903687921






# Part 2 ------------------------------------------------------------------

# for each image there are eight possible orientations, identified by
# information which original edge is on the top of the images
# - top (original)
# - top_rev (flipped left-right)
# - bottom (flipped top-bottom)
# - bottom_rev (flipped top-bottom and left-right OR rotated by 180)
# - right (rotated by 270)
# - right_rev (rotated by 270 and flipped left-right afterwards)
# - left (rotated by 90 and flipped left-right afterwards)
# - left_rev (rotated by 90)

# we will keep the matrix of already positioned images and separately
# a list of their orientations
# it doesn't matter how we will orientate the image for finding monsters












# generate ALL the pairs by reversing the relation
reverseRelations <- function(pairs) {
  rev_pairs <- data.frame(t1_id = pairs$t2_id,
                          t1_edge = pairs$t2_edge,
                          t2_id = pairs$t1_id,
                          t2_edge = pairs$t1_edge)
  ind <- grepl("_rev", rev_pairs$t1_edge)
  rev_pairs$t1_edge[ind] <- gsub("_rev", "", rev_pairs$t1_edge[ind])
  rev_pairs$t2_edge[ind] <- paste0(rev_pairs$t2_edge[ind], "_rev")
  rev_pairs
}


reorientateRelation <- function(relation, orientation) {
  rel <- relation$t1_edge
  new_rel <- relation

  if (orientation == "top") {
    # no changes

  } else if (orientation == "top_rev") {
    new_rel$t1_edge <- switch(rel,
                              "top" = "top_rev",
                              "bottom" = "bottom_rev",
                              "left" = "right",
                              "right" = "left")

  } else if (orientation == "bottom") {
    new_rel$t1_edge <- switch(rel,
                              "top" = "bottom",
                              "bottom" = "top",
                              "left" = "left_rev",
                              "right" = "right_rev")

  } else if (orientation == "bottom_rev") {
    new_rel$t1_edge <- switch(rel,
                              "top" = "bottom_rev",
                              "bottom" = "top_rev",
                              "left" = "right_rev",
                              "right" = "left_rev")

  } else if (orientation == "left") {
    new_rel$t1_edge <- switch(rel,
                              "top" = "left",
                              "bottom" = "right",
                              "left" = "top",
                              "right" = "bottom")

  } else if (orientation == "left_rev") {
    new_rel$t1_edge <- switch(rel,
                              "top" = "right",
                              "bottom" = "left",
                              "left" = "top_rev",
                              "right" = "bottom_rev")

  } else if (orientation == "right") {
    new_rel$t1_edge <- switch(rel,
                              "top" = "left_rev",
                              "bottom" = "right_rev",
                              "left" = "bottom",
                              "right" = "top")

  } else if (orientation == "right_rev") {
    new_rel$t1_edge <- switch(rel,
                              "top" = "right_rev",
                              "bottom" = "left_rev",
                              "left" = "bottom_rev",
                              "right" = "top_rev")
  }

  # reverse relation if needed
  if (grepl("_rev", new_rel$t1_edge)) {
    new_rel$t1_edge <- gsub("_rev", "", new_rel$t1_edge)
    new_rel$t2_edge <- ifelse(grepl("rev", new_rel$t2_edge),
                              gsub("_rev", "", new_rel$t2_edge),
                              paste0(new_rel$t2_edge, "_rev"))
  }


  new_rel
}


setNewTile <- function(pos, rel) {
  c_tile <- pos[pos$tile_id == rel$t1_id,]

  if (rel$t1_edge == "top") {
    new_row <- c_tile$row - 1
    new_col <- c_tile$col
    new_ort <- switch(rel$t2_edge,
                      "top" = "bottom",
                      "top_rev" = "bottom_rev",
                      "bottom" = "top",
                      "bottom_rev" = "top_rev",
                      "left" = "right",
                      "left_rev" = "right_rev",
                      "right" = "left",
                      "right_rev" = "left_rev")

  } else if (rel$t1_edge == "bottom") {
    new_row <- c_tile$row + 1
    new_col <- c_tile$col
    new_ort <- switch(rel$t2_edge,
                      "top" = "top",
                      "top_rev" = "top_rev",
                      "bottom" = "bottom",
                      "bottom_rev" = "bottom_rev",
                      "left" = "left",
                      "left_rev" = "left_rev",
                      "right" = "right",
                      "right_rev" = "right_rev")

  } else if (rel$t1_edge == "left") {
    new_row <- c_tile$row
    new_col <- c_tile$col - 1
    new_ort <- switch(rel$t2_edge,
                      "top" = "left_rev",
                      "top_rev" = "right_rev",
                      "bottom" = "left",
                      "bottom_rev" = "right",
                      "left" = "top_rev",
                      "left_rev" = "bottom_rev",
                      "right" = "top",
                      "right_rev" = "bottom")

  } else if (rel$t1_edge == "right") {
    new_row <- c_tile$row
    new_col <- c_tile$col + 1
    new_ort <- switch(rel$t2_edge,
                      "top" = "left",
                      "top_rev" = "right",
                      "bottom" = "left_rev",
                      "bottom_rev" = "right_rev",
                      "left" = "top",
                      "left_rev" = "bottom",
                      "right" = "top_rev",
                      "right_rev" = "bottom_rev")
  }

  new_tile <- data.frame(tile_id = rel$t2_id,
                         orientation = new_ort,
                         row = new_row,
                         col = new_col)
}


removeRelations <- function(rel, pos, tile) {
  row_n <- pos$tile_id[pos$row == tile$row & pos$col %in% (tile$col + (-1:1))]
  col_n <- pos$tile_id[pos$row %in% (tile$row + (-1:1)) & pos$col == tile$col]

  all_n <- unique(c(row_n, col_n))

  ind <- (rel$t1_id == tile$tile_id & rel$t2_id %in% all_n) |
    (rel$t2_id == tile$tile_id & rel$t1_id %in% all_n)
  rel <- rel[!ind, ]
  rel
}

orientTile <- function(tile, orientation) {
  n <- nrow(tile)

  if (orientation == "top") {
    # no changes
    new_tile <- tile

  } else if (orientation == "top_rev") {
    new_tile <- tile[, rev(seq_len(n))]

  } else if (orientation == "bottom") {
    new_tile <- tile[rev(seq_len(n)), ]

  } else if (orientation == "bottom_rev") {
    new_tile <- tile[rev(seq_len(n)), rev(seq_len(n))]

  } else if (orientation == "left") {
    new_tile <- t(tile)

  } else if (orientation == "left_rev") {
    new_tile <- t(tile)[, rev(seq_len(n))]

  } else if (orientation == "right") {
    new_tile <- t(tile)[rev(seq_len(n)), ]

  } else if (orientation == "right_rev") {
    new_tile <- t(tile)[rev(seq_len(n)), rev(seq_len(n))]
  }

  new_tile
}



calculatePositions <- function(tiles, pairs) {

  all_relations <- rbind(pairs, reverseRelations(pairs))

  # start loop with the first tile
  positioned_tiles <- data.frame(tile_id = all_relations$t1_id[1],
                                 orientation = "top",
                                 row = 1,
                                 col = 1)

  while (nrow(positioned_tiles) < length(tiles)) {

    # find the positioned tile with some relations left
    current_tile <- head(intersect(all_relations$t1_id, positioned_tiles$tile_id), 1)
    if (length(current_tile) == 0) {
      stop("Something went wrong")
    }

    # get the first relation left
    current_relation <- head(all_relations[all_relations$t1_id == current_tile, ], 1)

    # reset relation according to the orientation
    current_relation <- reorientateRelation(
      relation = current_relation,
      orientation = positioned_tiles$orientation[positioned_tiles$tile_id == current_relation$t1_id]
    )

    # add tile to the image
    new_tile <- setNewTile(positioned_tiles, current_relation)
    positioned_tiles <- rbind(positioned_tiles, new_tile)

    # remove relations (both sides)
    all_relations <- removeRelations(all_relations, positioned_tiles, new_tile)
  }

  # reorientate tiles
  tiles_oriented <- lapply(names(tiles), function(tt) orientTile(tiles[[tt]], positioned_tiles$orientation[positioned_tiles$tile_id == tt]))
  tiles_oriented <- setNames(tiles_oriented, names(tiles))

  #
  list(positions = positioned_tiles, tiles = tiles_oriented)
}

printPos <- function(pos) {
  n <- sqrt(length(pos$tiles))

  #
  ids <- pos$positions[order(pos$positions$col, pos$positions$row),]
  ids_m <- matrix(ids$tile_id, ncol = n)
  print(ids_m)

  #
  rows <- lapply(seq_len(n), function(i) do.call(cbind, pos$tiles[ids_m[i, ]]))

  tiles_rows <- lapply(pos$tiles, function(tt) apply(tt, 1, paste, collapse = ""))
  res <- do.call(rbind, rows)

  res <- apply(res, 1, paste, collapse = "")
  cat(paste(res, collapse = "\n"))
}


makeImageMatrix <- function(pos) {
  n <- sqrt(length(pos$tiles))

  #
  ids <- pos$positions[order(pos$positions$col, pos$positions$row),]
  ids_m <- matrix(ids$tile_id, ncol = n)

  # crop tiles
  tiles_cropped <- lapply(pos$tiles, function(t) t[2:(nrow(t)-1), 2:(ncol(t)-1)])

  #
  rows <- lapply(seq_len(n), function(i) do.call(cbind, tiles_cropped[ids_m[i, ]]))

  res <- do.call(rbind, rows)
  res
}




findMonstersInImage <- function(img, monster) {
  img_l <- img == "#"
  r_m <- nrow(monster)
  c_m <- ncol(monster)
  res <- data.frame()

  for (i in 1:(nrow(img_l) - r_m + 1)) {
    for (j in 1:(ncol(img_l) - c_m + 1)) {
      if (all(img_l[i + 1:r_m - 1, j + 1:c_m - 1][monster])) {
        res <- rbind(res, data.frame(row = i, col = j))
      }
    }
  }

  res
}




solvePuzzle <- function(tiles, monsters) {
  # as in part 1
  all_pairs <- checkAllPairs(tiles)

  # build image
  pos <- calculatePositions(tiles, all_pairs)
  img <- makeImageMatrix(pos)

  #
  img_c <- img
  for (i in seq_along(monsters)) {
    res <- findMonstersInImage(img, monsters[[i]])
    if (nrow(res) > 0) {
      # set dots where we have monster
      for (j in seq_len(nrow(res))) {
        img_c[res$row[j] + 1:nrow(monsters[[i]]) - 1, res$col[j] + 1:ncol(monsters[[i]]) - 1][monsters[[i]]] <- "."
      }

      break
    }
  }

  return(sum(img_c == "#"))
}



# monsters
monster <- do.call(rbind, strsplit(strsplit("                  #
#    ##    ##    ###
 #  #  #  #  #  #   ", "\n")[[1]], "")) == "#"


all_monster <- list(monster,
                    monster[3:1,],
                    monster[, 20:1],
                    monster[3:1, 20:1],
                    t(monster),
                    t(monster)[20:1,],
                    t(monster)[, 3:1],
                    t(monster)[20:1, 3:1])

# example
solvePuzzle(tiles_ex, all_monster)

# run
tiles <- generateTiles(readLines("inputs/input20.txt"))

solvePuzzle(tiles, all_monster)
# 2304

