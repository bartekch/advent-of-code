



read_input <- function(path) {
  m <- readLines(path)
  monkeys <- substr(m, 1, 4)
  out <- lapply(m, function(x) {
    monkey <- substr(x, 1, 4)
    if (nchar(x) == 17) {
      list(
        m1 = substr(x, 7, 10),
        op = substr(x, 12, 12),
        m2 = substr(x, 14, 17)
      )
    } else {
      as.numeric(substr(x, 7, nchar(x)))
    }
  })
  setNames(out, monkeys)
}



find_number <- function(monkeys, monkey_name = "root") {
  monkey <- monkeys[[monkey_name]]
  if (is.numeric(monkey)) return(monkey)

  m1 <- find_number(monkeys, monkey$m1)
  m2 <- find_number(monkeys, monkey$m2)
  if (monkey$op == "+") {
    res <- m1 + m2
  } else if (monkey$op == "-") {
    res <- m1 - m2
  } else if (monkey$op == "*") {
    res <- m1 * m2
  } else if (monkey$op == "/") {
    res <- m1 / m2
  }
  res
}


m_test <- read_input("input21_test.txt")
find_number(m_test, "root")


m <- read_input("input21.txt")
find_number(m, "root") |> print(digits = 20)
# 157714751182692




# 2 -----------------------------------------------------------------------

# brute force - try number by number up to 10000

checkroot <- function(monkeys) {
  rootm1 <- monkeys[["root"]]$m1
  rootm2 <- monkeys[["root"]]$m2

  for (i in 1:10000) {
    monkeys[["humn"]] <- i
    m1 <- find_number(monkeys, rootm1)
    m2 <- find_number(monkeys, rootm2)
    if (m1 == m2) {
      print(i)
      break
    }
  }
}

checkroot(m_test)

checkroot(m)
# no result!




# alternative version - we could keep numbers as couple (x, y) where x is the number of humn-s
# and calculate how many humns are required at the end
# humn will always show up in a single branch, so this is safe option!


find_number2 <- function(monkeys, monkey_name = "root") {
  monkey <- monkeys[[monkey_name]]

  if (is.numeric(monkey)) {
    if (monkey_name == "humn") {
      return(c(1, 0))
    } else {
      return(c(0, monkey))
    }
  }

  m1 <- find_number2(monkeys, monkey$m1)
  m2 <- find_number2(monkeys, monkey$m2)


  if (m1[1] != 0 && m2[1] != 0) stop("both branches with humnn")

  if (monkey$op == "+") {
    res <- m1 + m2

  } else if (monkey$op == "-") {
    res <- m1 - m2

  } else if (monkey$op == "*") {
    if (m1[1] == 0) {
      res <- m1[2] * m2
    } else {
      res <- m1 * m2[2]
    }
  } else if (monkey$op == "/") {
    if (m2[1] == 0) {
      res <- m1 / m2[2]
    } else if (m2[2] == 0) {
      res <- c(m1[2] / m1[1], 0)
    } else {
      res <- m1[2] / m2
    }
  }
  res
}


out <- find_number2(m_test)
m_test[["root"]]
print(out[2] + m_test[["humn"]] * out[1], digits = 20)


out <- find_number2(m)
m[["root"]]
print(out[2] + m[["humn"]] * out[1], digits = 20)
# correct


checkroot <- function(monkeys) {
  rootm1 <- monkeys[["root"]]$m1
  rootm2 <- monkeys[["root"]]$m2

  m1 <- find_number2(monkeys, rootm1)
  m2 <- find_number2(monkeys, rootm2)

  if (m1[1] == 0) {
    (m1[2] - m2[2]) / m2[1]
  } else {
    (m2[2] - m1[2]) / m1[1]
  }
}



checkroot(m_test)
checkroot(m) |> print(digits = 20)
# 3373767893067.0005
# some floating point numbers errors, but 3373767893067 is the correct answer
# loop wouldn't be feasible
