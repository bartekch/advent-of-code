
# Part 1 ------------------------------------------------------------------

# we would define our own operators and replace them in an epression
`%s%` <- `+`
`%m%` <- `*`

1 %s% 2 %m% 3


evalNoPrecedence <- function(expr) {
  expr <- gsub("\\+", "%s%", expr)
  expr <- gsub("\\*", "%m%", expr)
  eval(str2expression(expr))
}

# examples
evalNoPrecedence("1+2*3")
evalNoPrecedence("1 + 2 * 3 + 4 * 5 + 6")
evalNoPrecedence("1 + (2 * 3) + (4 * (5 + 6))")
evalNoPrecedence("2 * 3 + (4 * 5)")
evalNoPrecedence("5 + (8 * 3 + 9 + 3 * 4 * 3)")
evalNoPrecedence("5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))")
evalNoPrecedence("((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2 ")


# run
inputs <- readLines("inputs/input18.txt")
format(sum(sapply(inputs, evalNoPrecedence)), scientific = FALSE)
# 464478013511



# Part 2 ------------------------------------------------------------------

# we could redefine only multiplication operator to lower its precedence
# to do this we need to overwrite some operator, that has lower
# predecence than multiplication, see ?Sytanx
# remember NOT to use that operator in a normal sense!!

`&` <- `*`
3 & 2 + 1

evalRevPrecedence <- function(expr) {
  expr <- gsub("\\*", "&", expr)
  eval(str2expression(expr))
}

# examples
evalRevPrecedence("1+2*3")
evalRevPrecedence("1 + 2 * 3 + 4 * 5 + 6")
evalRevPrecedence("1 + (2 * 3) + (4 * (5 + 6))")
evalRevPrecedence("2 * 3 + (4 * 5)")
evalRevPrecedence("5 + (8 * 3 + 9 + 3 * 4 * 3)")
evalRevPrecedence("5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))")
evalRevPrecedence("((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2 ")

# run
format(sum(sapply(inputs, evalRevPrecedence)), scientific = FALSE)
# 85660197232452
