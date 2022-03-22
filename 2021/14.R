
read_input <- function(path) {
  l <- readLines(path)
  linebreak <- which(l == "")

  template <- head(l, linebreak - 1)
  template <- strsplit(template, "")[[1]]

  rules <- tail(l, -linebreak)
  rules <- lapply(strsplit(rules, " -> "), function(f) data.frame(pair = f[1], el = f[2]))
  rules <- do.call(rbind, rules)

  list(template = template, rules = rules)
}



# 1 -----------------------------------------------------------------------

insert_once <- function(template, rules) {
  template_pair <- paste0(head(template, -1), tail(template, -1))

  next_el <- rules$el[match(template_pair, rules$pair)]

  new_template <- matrix(c(head(template, -1), next_el), nrow = 2, byrow = TRUE)
  c(new_template, tail(template, 1))
}


insert_n <- function(template, rules, n) {
  for (i in seq_len(n)) {
    template <- insert_once(template, rules)
  }
  template
}

calculate_score <- function(template) {
  t <- table(template)
  max(t) - min(t)
}


test <- read_input("input14_test.txt")
insert_n(test$template, test$rules, 1)
insert_n(test$template, test$rules, 4)
calculate_score(insert_n(test$template, test$rules, 10))

input <- read_input("input14.txt")
calculate_score(insert_n(input$template, input$rules, 10))
# 2068



# 2 -----------------------------------------------------------------------

# we need more efficient approach
# just count the number of pairs and modify this table accordingly


insert_n2 <- function(template, rules, n) {
  # prepare table with pairs
  pair_t <- data.frame(
    pair = paste0(head(template, -1), tail(template, -1)),
    n = 1
  )

  # modify rules so for every pair we have two resulting pairs
  rules_pairs <- lapply(seq_len(nrow(rules)), function(i) {
    rule <- rules[i, ]
    r <- strsplit(rule$pair, "")[[1]]
    c(paste0(r[1], rule$el), paste0(rule$el, r[2]))
  })
  names(rules_pairs) <- rules$pair

  for (i in seq_len(n)) {
    pair_t <- insert_once2(pair_t, rules_pairs)
  }


  el_c <- tapply(pair_t$n, substr(pair_t$pair, 1, 1), sum)
  # add last element from template - it will stay the same through whole process
  last_el <- tail(template, 1)
  if (last_el %in% names(el_c)) {
    el_c[last_el] <- el_c[last_el] + 1
  } else {
    el_c <- c(el_c, setNames(1, last_el))
  }

  max(el_c) - min(el_c)
}


insert_once2 <- function(pair_t, rules_pairs) {
  new_pairs <- lapply(seq_len(nrow(pair_t)), function(i) {
    data.frame(pair = rules_pairs[[pair_t$pair[i]]], n = pair_t$n[i])
  })
  new_pairs <- do.call(rbind, new_pairs)
  new_pairs <- aggregate(n ~ pair, new_pairs, sum)
  new_pairs
}


insert_n2(test$template, test$rules, 10)
print(insert_n2(test$template, test$rules, 40), digits = 20)

print(insert_n2(input$template, input$rules, 40), digits = 20)
# 2158894777814
