
read_input <- function(path) {
  l <- readLines(path)
  dict <- c(
    "0" = "0000",
    "1" = "0001",
    "2" = "0010",
    "3" = "0011",
    "4" = "0100",
    "5" = "0101",
    "6" = "0110",
    "7" = "0111",
    "8" = "1000",
    "9" = "1001",
    "A" = "1010",
    "B" = "1011",
    "C" = "1100",
    "D" = "1101",
    "E" = "1110",
    "F" = "1111"
  )

  lapply(strsplit(l, ""), function(n) as.integer(unlist(strsplit(dict[n], ""))))
}

bin_to_dec <- function(x) {
  sum(x * 2^rev(seq_along(x) - 1))
}



# 1 -----------------------------------------------------------------------

parse_packet <- function(packet) {

  version <- head(packet, 3)
  version <- bin_to_dec(version)
  packet <- tail(packet, -3)

  type <- head(packet, 3)
  type <- bin_to_dec(type)
  packet <- tail(packet, -3)

  if (type == 4) {
    content <- parse_literal(packet)
    length <- content$length
    tail <- content$tail
    content <- content[-which(names(content) %in% c("tail", "length"))]

  } else {
    content <- parse_operator(packet)
    length <- content$length
    tail <- content$tail
    content <- content$subpackets
  }

  list(
    version = version,
    type = type,
    length = length + 6,
    tail = tail,
    content = content
  )
}


parse_literal <- function(packet) {

  value <- integer()
  l <- 0
  while(TRUE) {
    l <- l + 5
    group <- head(packet, 5)
    value <- c(value, group[2:5])
    packet <- tail(packet, -5)
    if (group[1] == 0) break
  }

  value <- bin_to_dec(value)

  list(
    value = value,
    length = l,
    tail = packet
  )
}


parse_operator <- function(packet) {
  length_type <- head(packet, 1)
  packet <- packet[-1]
  packet_l <- 1

  if (length_type == 0) {
    packet_l <- packet_l + 15
    length <- head(packet, 15)
    length <- bin_to_dec(length)
    packet <- tail(packet, -15)

    subpackets <- list()
    while (length > 0) {
      tmp <- parse_packet(packet)
      packet <- tmp$tail
      packet_l <- packet_l + tmp$length
      length <- length - tmp$length
      tmp <- tmp[-which(names(tmp) %in% c("tail", "length"))]
      subpackets <- c(subpackets, list(tmp))
    }


  } else {
    packet_l <- packet_l + 11
    packet_n <- head(packet, 11)
    packet_n <- bin_to_dec(packet_n)
    packet <- tail(packet, -11)

    subpackets <- list()
    for (i in seq_len(packet_n)) {
      tmp <- parse_packet(packet)
      packet <- tmp$tail
      packet_l <- packet_l + tmp$length
      tmp <- tmp[-which(names(tmp) %in% c("tail", "length"))]
      subpackets <- c(subpackets, list(tmp))
    }
  }

  list(
    subpackets = subpackets,
    tail = packet,
    length = packet_l
  )
}


sum_versions <- function(packet_tree) {
  total <- packet_tree$version

  if (packet_tree$type != 4) {
    total <- total + sum(vapply(packet_tree$content, sum_versions, numeric(1)))
  }
  total
}


test <- read_input("input16_test.txt")

for (i in seq_along(test)) {
  message(i, ": ", sum_versions(parse_packet(test[[i]])))
}

input <- read_input("input16.txt")
res <- parse_packet(input[[1]])
sum_versions(res)
# 984



# 2 -----------------------------------------------------------------------


calculate_value <- function(packet_tree) {
  if (packet_tree$type != 4) {
    subvalues <- vapply(packet_tree$content, calculate_value, numeric(1))
  }
  if (packet_tree$type == 0) {
    value <- sum(subvalues)

  } else if (packet_tree$type == 1) {
    value <- prod(subvalues)

  } else if (packet_tree$type == 2) {
    value <- min(subvalues)

  } else if (packet_tree$type == 3) {
    value <- max(subvalues)

  } else if (packet_tree$type == 4) {
    value <- packet_tree$content$value

  } else if (packet_tree$type == 5) {
    value <- as.integer(subvalues[1] > subvalues[2])

  } else if (packet_tree$type == 6) {
    value <- as.integer(subvalues[1] < subvalues[2])

  } else if (packet_tree$type == 7) {
    value <- as.integer(subvalues[1] == subvalues[2])

  } else {
    value <- 0
  }

  value
}



test <- read_input("input16_test2.txt")
for (i in seq_along(test)) {
  message(i, ": ", calculate_value(parse_packet(test[[i]])))
}

input <- read_input("input16.txt")
res <- parse_packet(input[[1]])
format(calculate_value(res), scientific = FALSE)
# 1015320896946
