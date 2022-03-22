

parse_input <- function(path) {
  input <- readLines(path)
  input <- strsplit(input, " \\| ")
  input <- lapply(input, function(inp) {
    list(
      signal = strsplit(inp[1], " ")[[1]],
      output = strsplit(inp[2], " ")[[1]]
    )
  })
  input
}



# 1 -----------------------------------------------------------------------

count_1478 <- function(input) {
  # 1 - dwo digits
  # 4 - four digits
  # 7 - three digits
  # 8 - seven digits
  tmp <- sapply(input, function(inp) {
    sum(nchar(inp$output) %in% c(2, 4, 3, 7))
  })
  sum(tmp)
}

count_1478(parse_input("input08_test.txt"))
count_1478(parse_input("input08.txt"))
# 543



# 2 -----------------------------------------------------------------------

determine_signal <- function(signals) {
  #  aaaa
  # b    c
  # b    c
  #  dddd
  # e    f
  # e    f
  #  gggg
  # 0 - abc efg # 6
  # 1 -   c  f  # 2
  # 2 - a cde g # 5
  # 3 - a cd fg # 5
  # 4 -  bcd f  # 4
  # 5 - ab d fg # 5
  # 6 - ab defg # 6
  # 7 - a c  f  # 3
  # 8 - abcdefg # 7
  # 9 - abcd fg # 6

  # a - 8: 0,2,3,5,6,7,8,9
  # b - 6: 0,4,5,6,8,9
  # c - 8: 0,1,2,3,4,7,8,9
  # d - 7: 2,3,4,5,6,8,9
  # e - 4: 2,6,8
  # f - 9: 0,1,3,4,5,6,7,8,9
  # g - 7: 0,2,3,5,6,8,9

  # so based solely on frequencies we could identify b,e,f
  # 1 / f => known c
  # 7 / 1 => known a
  # 4 / b / c / f => known d
  # last missing - g
  signal_map <- vector("list", 10) # 10 is 0
  segment_map <- setNames(character(7), letters[1:7])

  signals_c <- strsplit(signals, "")
  nc <- nchar(signals)

  signal_map[1] <- signals_c[nc == 2]
  signal_map[4] <- signals_c[nc == 4]
  signal_map[7] <- signals_c[nc == 3]
  signal_map[8] <- signals_c[nc == 7]

  digits <- table(unlist(signals_c))

  segment_map["b"] <- names(digits)[digits == 6]
  segment_map["e"] <- names(digits)[digits == 4]
  segment_map["f"] <- names(digits)[digits == 9]

  # 1 / f => known c
  segment_map["c"] <- setdiff(signal_map[[1]], segment_map["f"])

  # 7 / 1 => known a
  segment_map["a"] <- setdiff(signal_map[[7]], signal_map[[1]])

  # 4 / b / c / f => known d
  segment_map["d"] <- setdiff(signal_map[[4]], segment_map[c("b", "c", "f")])

  # last missing - g
  segment_map["g"] <- setdiff(letters[1:7], segment_map)

  signal_map[[2]] <- segment_map[c("a", "c", "d", "e", "g")]
  signal_map[[3]] <- segment_map[c("a", "c", "d", "f", "g")]
  signal_map[[5]] <- segment_map[c("a", "b", "d", "f", "g")]
  signal_map[[6]] <- segment_map[c("a", "b", "d", "e", "f", "g")]
  signal_map[[9]] <- segment_map[c("a", "b", "c", "d", "f", "g")]
  signal_map[[10]] <- segment_map[c("a", "b", "c", "e", "f", "g")]

  # we need to sort letters
  setNames(
    c(1:9, 0),
    sapply(lapply(signal_map, sort), paste, collapse = "")
  )
}

calculate_output <- function(entry) {
  number_map <- determine_signal(entry$signal)
  output <- sapply(lapply(strsplit(entry$output, ""), sort), paste, collapse = "")
  as.integer(paste(number_map[output], collapse = ""))
}

sum(sapply(parse_input("input08_test.txt"), calculate_output))

sum(sapply(parse_input("input08.txt"), calculate_output))
# 994266
