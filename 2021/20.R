
read_input <- function(path) {
 l <- readLines(path)

 code <- as.integer((strsplit(l[1], "")[[1]] == "#"))

 # image is transposed - it would be easier to read by column
 image <- do.call(cbind, strsplit(tail(l, -2), "")) == "#"
 image <- matrix(as.integer(image), ncol = ncol(image))
 list(code = code, image = image)
}

plot_image <- function(image) {
  image <- t(image)
  out <- matrix(".", nrow = nrow(image), ncol = ncol(image))
  out[image == 1] <- "#"
  cat(paste(apply(out, 1, paste, collapse = ""), collapse = "\n"))
  invisible()
}



enhance_once <- function(image, code, inf_v = 0) {
  # extend image by two layers of 0s
  # , unless there are all zeros on the side
  # if (any(image[1, ]) != 0) image <- image <- rbind(0, image)
  # if (any(image[1, ]) != 0) image <- image <- rbind(0, image)

  image <- rbind(
    inf_v, inf_v,
    cbind(inf_v, inf_v, image, inf_v, inf_v),
    inf_v, inf_v
  )

  new_image <- image

  for (i in 2:(nrow(image) - 1)) {
    for (j in 2:(ncol(image) - 1)) {
      new_value <- c(image[(i - 1):(i + 1), (j - 1):(j + 1)])
      new_image[i, j] <- code[sum(new_value * 2^(8:0)) + 1]
    }
  }

  # replace values for the outer layer
  if (inf_v == 0) {
    new_inf_v <- code[1]
  } else {
    new_inf_v <- code[512]
  }
  new_image[1, ] <- new_inf_v
  new_image[nrow(new_image), ] <- new_inf_v
  new_image[, 1] <- new_inf_v
  new_image[, ncol(new_image)] <- new_inf_v

  new_image
}


enhance_n <- function(image, code, n) {
  for (i in seq_len(n) - 1) {
    # we are processing the whole infinite image at once
    # initially this infinite surface would be all 0s
    # then it would follow the pattern from the previous iterations
    if (i == 0) {
      inf_v <- 0
    } else {
      inf_v <- image[1, 1]
    }
    image <- enhance_once(image, code, inf_v)
  }
  image
}


test <- read_input("input20_test.txt")
plot_image(test$image)
res <- enhance_n(test$image, test$code, 2)
plot_image(res)
sum(res)


input <- read_input("input20.txt")
res <- enhance_n(input$image, input$code, 2)
plot_image(res)
sum(res)
# 5225




# 2 -----------------------------------------------------------------------

test <- read_input("input20_test.txt")
res <- enhance_n(test$image, test$code, 50)
plot_image(res)
sum(res)


input <- read_input("input20.txt")
res <- enhance_n(input$image, input$code, 50)
sum(res)
# 18131
