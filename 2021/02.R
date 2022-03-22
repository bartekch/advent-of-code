
# 1 -----------------------------------------------------------------------

input <- read.table("input02.txt", header = FALSE, sep = " ", col.names = c("direction", "value"))

pos <- sum(input$value[input$direction == "forward"])

input$value[input$direction == "up"] <- -input$value[input$direction == "up"]
depth <- sum(input$value[input$direction != "forward"])

pos * depth
# 2039912



# 2 -----------------------------------------------------------------------

input <- read.table("input02.txt", header = FALSE, sep = " ", col.names = c("direction", "value"))
input$value[input$direction == "up"] <- -input$value[input$direction == "up"]

aim <- 0
depth <- 0
pos <- 0

for (i in seq_len(nrow(input))) {
  if (input$direction[i] == "forward") {
    pos <- pos + input$value[i]
    depth <- depth + aim * input$value[i]
  } else {
    aim <- aim + input$value[i]
  }
}
pos * depth
# 1942068080
