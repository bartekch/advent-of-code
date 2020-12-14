
# Part 1 ------------------------------------------------------------------

# Execute the initialization program. What is the sum of all values left in memory after it completes?

program <- readLines("inputs/input14.txt")

INST_L <- 36

# I wanted to use bitwise operations, but we are hitting R integer size limit
# and there are none bitwise operations in bit64 package
# so we are simulating it by operating on logical vectors

library(bit64)

int2bits <- function(x) {
  as.logical(as.integer(tail(unlist(strsplit(as.bitstring(as.integer64(x)), "")), INST_L)))
}

bits2int <- function(x) {
  as.integer64.bitstring(paste(as.character(as.integer(x)), collapse = ""))
}

parseMask <- function(m) {
  tmp <- unlist(strsplit(gsub("mask = ", "", m), ""))
  list(or = (tmp == "1"), and = (tmp != "0"))
}


parseMem <- function(m) {
  tmp <- unlist(strsplit(m, " = "))
  list(memory = substr(tmp[1], 5, nchar(tmp[1]) - 1),
       value = as.integer64(tmp[2]))
}


applyMask <- function(mask, value) {
  v <- int2bits(value)
  # 
  v <- v | mask$or
  v <- v & mask$and
  
  bits2int(v)
}



initializeProgram <- function(program) {
  res <- list()
  is_mask <- grepl("mask", program)
  
  for (i in seq_along(program)) {
    if (is_mask[i]) {
      mask <- parseMask(program[i])
    } else {
      mem <- parseMem(program[i])
      res[[mem$memory]] <- applyMask(mask, mem$value)
    }
  }
  res
}


mm <- parseMask("mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X")
applyMask(mm, 0)


res <- initializeProgram(program)

Reduce(`+`, res)
# integer64
# [1] 7477696999511





# Part 2 ------------------------------------------------------------------


# If the bitmask bit is 0, the corresponding memory address bit is unchanged.
# If the bitmask bit is 1, the corresponding memory address bit is overwritten with 1.
# If the bitmask bit is X, the corresponding memory address bit is floating.

# we need to rewrite our functions to accomodate for different algorithm

parseMask <- function(m) {
  base_mask <- unlist(strsplit(gsub("mask = ", "", m), ""))
  base_mask
}


applyMask <- function(mask, value) {
  v <- int2bits(value)
  # 
  v <- v | (mask == "1")
  
  # evaluate floating codes
  indX <- which(mask == "X")
  floatingX <- setNames(rep(list(c(FALSE, TRUE)), times = length(indX)), indX)
  floatingX <- do.call(expand.grid, floatingX)
  
  all_values <- sapply(seq_len(nrow(floatingX)), function(i) {
    tmp <- v
    tmp[indX] <- as.logical(floatingX[i,])
    as.character(bits2int(tmp))
  })
  
  all_values
}




initializeProgram2 <- function(program) {
  res <- list()
  is_mask <- grepl("mask", program)
  
  for (i in seq_along(program)) {
    if (is_mask[i]) {
      mask <- parseMask(program[i])
    } else {
      mem <- parseMem(program[i])
      res[as.character(applyMask(mask, mem$memory))] <- list(as.integer64(mem$value))
    }
  }
  res
}


# example
program_ex <- unlist(strsplit(
  "mask = 000000000000000000000000000000X1001X
mem[42] = 100
mask = 00000000000000000000000000000000X0XX
mem[26] = 1"
, "\n"))


initializeProgram2(program_ex)

# 
res <- initializeProgram2(program)

Reduce(`+`, res)
# integer64
# [1] 3687727854171


