
generate_rotations <- function() {
  rotations_x <- list(
    matrix(c(1,0,0, 0,0,1, 0,-1,0), ncol = 3, byrow = TRUE), #90
    matrix(c(1,0,0, 0,-1,0, 0,0,-1), ncol = 3, byrow = TRUE), #180
    matrix(c(1,0,0, 0,0,-1, 0,1,0), ncol = 3, byrow = TRUE) #270
  )

  rotations_y <- list(
    matrix(c(0,0,-1, 0,1,0, 1,0,0), ncol = 3, byrow = TRUE), #90
    matrix(c(-1,0,0, 0,1,0, 0,0,-1), ncol = 3, byrow = TRUE), #180
    matrix(c(0,0,1, 0,1,0, -1,0,0), ncol = 3, byrow = TRUE) #270
  )

  rotations_z <- list(
    matrix(c(0,-1,0, 1,0,0, 0,0,1), ncol = 3, byrow = TRUE), #90
    matrix(c(-1,0,0, 0,-1,0, 0,0,1), ncol = 3, byrow = TRUE), #180
    matrix(c(0,1,0, -1,0,0, 0,0,1), ncol = 3, byrow = TRUE) #270
  )


  all_rotations <- c(
    list(diag(3)),
    rotations_x,
    rotations_y,
    rotations_z
  )

  for (r1 in rotations_x) {
    for (r2 in rotations_y) {
      all_rotations <- append(all_rotations, list(r1 %*% r2))
      for (r3 in rotations_z) {
        all_rotations <- append(all_rotations, list(r1 %*% r2 %*% r3))
      }
    }
    for (r2 in rotations_z) {
      all_rotations <- append(all_rotations, list(r1 %*% r2))
      for (r3 in rotations_y) {
        all_rotations <- append(all_rotations, list(r1 %*% r2 %*% r3))
      }
    }
  }

  for (r1 in rotations_y) {
    for (r2 in rotations_x) {
      all_rotations <- append(all_rotations, list(r1 %*% r2))
      for (r3 in rotations_z) {
        all_rotations <- append(all_rotations, list(r1 %*% r2 %*% r3))
      }
    }
    for (r2 in rotations_z) {
      all_rotations <- append(all_rotations, list(r1 %*% r2))
      for (r3 in rotations_x) {
        all_rotations <- append(all_rotations, list(r1 %*% r2 %*% r3))
      }
    }
  }

  for (r1 in rotations_z) {
    for (r2 in rotations_x) {
      all_rotations <- append(all_rotations, list(r1 %*% r2))
      for (r3 in rotations_y) {
        all_rotations <- append(all_rotations, list(r1 %*% r2 %*% r3))
      }
    }
    for (r2 in rotations_y) {
      all_rotations <- append(all_rotations, list(r1 %*% r2))
      for (r3 in rotations_x) {
        all_rotations <- append(all_rotations, list(r1 %*% r2 %*% r3))
      }
    }
  }


  duplicate <- logical(length(all_rotations))
  for (i in 1:(length(all_rotations) - 1)) {
    if (duplicate[i]) next
    for (j in (i+1):length(all_rotations)) {
      if (duplicate[j]) next
      if (isTRUE(identical(all_rotations[[i]], all_rotations[[j]]))) {
        duplicate[j] <- TRUE
      }
    }
  }
  all_rotations <- all_rotations[!duplicate]

  all_rotations
}



read_input <- function(path) {
  l <- readLines(path)

  start <- grep("scanner", l) + 1
  end <- c(tail(start, -1) - 3, length(l))

  # transpose matrices since it's easier to work with them like that in R
  scanners <- mapply(start, end, FUN = function(s, e) {
    do.call(
      cbind,
      lapply(strsplit(l[s:e], ","), as.integer)
    )
  }, SIMPLIFY = FALSE)
  stopifnot(
    all(sapply(scanners, function(m) ncol(m) == nrow(unique(t(m)))))
  )
  scanners
}



check_match <- function(s1, s2, min_overlap = 12) {
  hit <- FALSE
  for (i in seq_len(ncol(s1))) {
    s1_b <- s1 - s1[,i]
    for (j in seq_len(ncol(s2))) {
      s2_b <- s2 - s2[,j]
      out <- sum(duplicated(t(cbind(s1_b, s2_b))))
      if (out >= min_overlap) {
        return(c(i, j))
      }
    }
  }
}


# this won't be required in part 1
find_match <- function(s1, s1_c, s2, s2_c) {
  s1 <- s1 - s1[, s1_c]
  s2 <- s2 - s2[, s2_c]
  out <- integer(ncol(s1))
  for (i in seq_len(ncol(s1))) {
    dist <- colSums(s2 - s1[, i])
    matched <- which(dist == 0)
    if (length(matched) == 1) out[i] <- matched
  }
  out
}




# 1 -----------------------------------------------------------------------

solve <- function(scanners) {
  rotations <- generate_rotations()

  # scanners - list of matrices
  scanners_org <- scanners

  # list with scanner positions and rotations
  scanners_pos <- vector("list", length(scanners))
  scanners_pos[[1]] <- list(pos = c(0, 0, 0), rotation = diag(3))

  # vector keeping track which scanners positions are already fixed
  scanners_matched <- logical(length(scanners))
  scanners_matched[1] <- TRUE

  # vector keeping track what scanners have been used as base scanners
  scanners_used <- logical(length(scanners))

  while (!all(scanners_matched)) {
    # get first matched and unused scanner
    ind <- head(which(scanners_matched & !scanners_used), 1)
    message(Sys.time(), " using ", ind, " as base scanner")
    scanner_base <- scanners[[ind]]
    scanners_used[ind] <- TRUE

    # loop through all unmatched
    for (i in which(!scanners_matched)) {
      scanner_to_check <- scanners[[i]]

      for (rotation in rotations) {
        scanner_to_check_rotated <- rotation %*% scanner_to_check
        out <- check_match(scanner_base, scanner_to_check_rotated)
        if (!is.null(out)) {
          shift <- scanner_base[, out[1]] - scanner_to_check_rotated[, out[2]]

          scanners_pos[[i]] <- list(
            pos = shift,
            rotation = rotation
          )
          scanners_matched[i] <- TRUE
          message(Sys.time(), " matched ", i, " scanner")
          # overwrite scanner with its absolute position
          scanners[[i]] <- scanner_to_check_rotated + shift

          # omit following rotations
          break
        }
      }
    }

  }

  list(scanners = scanners, scanners_pos = scanners_pos)
}



count_beacons <- function(scanners) {
  nrow(unique(t(do.call(cbind, scanners))))
}




# tests
s1 <- matrix(c(0,2,
               4,1,
               3,3), nrow = 2)


s2 <- matrix(c(-1,-1,
               -5,0,
               -2,1), nrow = 2)

check_match(s1, s2, 3)
find_match(s1, 1, s2, 2)
scanner_pos <- s1[,2] - s2[,1]
scanner_pos + s2


test <- read_input("input19_test.txt")
res <- solve(test)
count_beacons(res$scanners)

input <- read_input("input19.txt")
res <- solve(input)
count_beacons(res$scanners)
# 428



# 2 -----------------------------------------------------------------------

furthest_scanners <- function(positions) {
  max_distance <- 0
  for (i in 1:(length(positions) - 1)) {
    for (j in (i+1):length(positions)) {
      d <- sum(abs(positions[[i]]$pos - positions[[j]]$pos))
      max_distance <- max(max_distance, d)
    }
  }
  max_distance
}

furthest_scanners(res$scanners_pos)
# 12140
