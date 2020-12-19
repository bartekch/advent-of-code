
# Part1 -------------------------------------------------------------------

parseRules <- function(rules) {

  # remove quotation marks
  rules <- gsub("\"", "", rules)

  # watch out! rules are not sorted
  rules_id <- gsub(": .*$", "", rules)

  # remove rules ids
  rules <- gsub("^[[:digit:]]+: ", "", rules)

  # add spaces at the start and the end of the rule so we could safely assume
  # that every rule is surrounded by spaces
  rules <- paste0(" ", rules, " ")

  # create a vector for parsed_rules
  rules_parsed <- rep(NA_character_, length(rules))

  # use ids as names for both vectors
  rules <- setNames(rules, rules_id)
  rules_parsed <- setNames(rules_parsed, rules_id)


  # replace ids wih completed rules until nothign is left
  while (any(is.na(rules_parsed))) {
    # identify those with no digits, that are not parsed yet
    complete_rules <- is.na(rules_parsed) & grepl("^[^[:digit:]]*$", rules)

    if (all(!complete_rules)) stop("Endless loop")

    # move them to parsed_rules and add parentheses, which could be omitted
    # for single characters
    tmp <- gsub(" ", "", rules[complete_rules])
    rules_parsed[complete_rules] <- ifelse(nchar(tmp) == 1, tmp, paste0("(", tmp, ")"))

    # for every new rule, replace its occurences in the rest of the rules
    # add parentheses
    for (rr in names(rules_parsed)[complete_rules]) {
      rules <- gsub(paste0("\\b", rr, "\\b"), rules_parsed[rr], rules)
    }
  }

  # remove whitespaces
  rules_parsed <- gsub(" ", "", rules_parsed)

  # add start/end tags
  rules_parsed <- paste0("^", rules_parsed, "$")

  #
  rules_parsed
}



# example
example <- unlist(strsplit('0: 4 1 5
1: 2 3 | 3 2
2: 4 4 | 5 5
3: 4 5 | 5 4
4: "a"
5: "b"

ababbb
bababa
abbbab
aaabbb
aaaabbb', "\n"))


rules <- head(example, which(example == "") - 1)
messages <- tail(example, -which(example == ""))

parsed_rules <- parseRules(rules)

sum(grepl(parsed_rules[1], messages))


# run

input <- readLines("inputs/input19.txt")

messages <- tail(input, -which(input == ""))
rules <- head(input, which(input == "") - 1)

parsed_rules <- parseRules(rules)
sum(grepl(parsed_rules[grep("^0:", rules)], messages))
# 269




# Part 2 ------------------------------------------------------------------

# 8: 42 | 42 8
# 11: 42 31 | 42 11 31


# take a look at he example manually

example <- unlist(strsplit('42: 9 14 | 10 1
9: 14 27 | 1 26
10: 23 14 | 28 1
1: "a"
11: 42 31
5: 1 14 | 15 1
19: 14 1 | 14 14
12: 24 14 | 19 1
16: 15 1 | 14 14
31: 14 17 | 1 13
6: 14 14 | 1 14
2: 1 24 | 14 4
0: 8 11
13: 14 3 | 1 12
15: 1 | 14
17: 14 2 | 1 7
23: 25 1 | 22 14
28: 16 1
4: 1 1
20: 14 14 | 1 15
3: 5 14 | 16 1
27: 1 6 | 14 18
14: "b"
21: 14 1 | 1 14
25: 1 1 | 1 14
22: 14 14
8: 42
26: 14 22 | 1 20
18: 15 15
7: 14 5 | 1 21
24: 14 1

abbbbbabbbaaaababbaabbbbabababbbabbbbbbabaaaa
bbabbbbaabaabba
babbbbaabbbbbabbbbbbaabaaabaaa
aaabbbbbbaaaabaababaabababbabaaabbababababaaa
bbbbbbbaaaabbbbaaabbabaaa
bbbababbbbaaaaaaaabbababaaababaabab
ababaaaaaabaaab
ababaaaaabbbaba
baabbaaaabbaaaababbaababb
abbbbabbbbaaaababbbbbbaaaababb
aaaaabbaabaaaaababaa
aaaabbaaaabbaaa
aaaabbaabbaaaaaaabbbabbbaaabbaabaaa
babaaabbbaaabaababbaabababaaab
aabbbbbaabbbaaaaaabbbbbababaaaaabbaaabba', "\n"))


rules_ex <- head(example, which(example == "") - 1)
messages_ex <- tail(example, -which(example == ""))

parsed_rules_ex <- parseRules(rules_ex)
sum(grepl(parsed_rules_ex[grep("^0:", rules_ex)], messages_ex))

rules_ex[grep("^8:", rules_ex)] <- "8: 42 | 42 8"
rules_ex[grep("^11:", rules_ex)] <- "11: 42 31 | 42 11 31"

# is it enough to use regex +?
rules_ex[grep("^8:", rules_ex)] <- "8: 42+"
rules_ex[grep("^11:", rules_ex)] <- "11: 42+ 31+"
parsed_rules_ex <- parseRules(rules_ex)
sum(grepl(parsed_rules_ex[grep("^0:", rules_ex)], messages_ex))
# working

# run
rules <- head(input, which(input == "") - 1)
rules[grep("^8:", rules)] <- "8: 42+"
rules[grep("^11:", rules)] <- "11: 42+ 31+"


parsed_rules <- parseRules(rules)
messages <- tail(input, -which(input == ""))

sum(grepl(parsed_rules[grep("^0:", rules)], messages))
# 408
# not working


# NOTE after some remark from reddit
# for rule 11 - we need to have THE SAME number of occurrences of rule 42 and 31!!
# try manually adjusting it
max(nchar(messages))

parseRules2 <- function(rules) {

  # remove quotation marks
  rules <- gsub("\"", "", rules)

  # watch out! rules are not sorted
  rules_id <- gsub(": .*$", "", rules)

  # remove rules ids
  rules <- gsub("^[[:digit:]]+: ", "", rules)

  # add spaces at the start and the end of the rule so we could safely assume
  # that every rule is surrounded by spaces
  rules <- paste0(" ", rules, " ")

  # create a vector for parsed_rules
  rules_parsed <- rep(NA_character_, length(rules))

  # use ids as names for both vectors
  rules <- setNames(rules, rules_id)
  rules_parsed <- setNames(rules_parsed, rules_id)


  # replace ids wih completed rules until nothign is left
  while (any(is.na(rules_parsed))) {
    # identify those with no digits, that are not parsed yet
    complete_rules <- is.na(rules_parsed) & grepl("^[^[:digit:]]*$", rules)
    # special case - rule 11 and rule 0
    complete_rules["11"] <- is.na(rules_parsed["11"]) & !grepl("42|31", rules["11"])
    complete_rules["0"] <- is.na(rules_parsed["0"]) & !grepl("8|11", rules["0"])

    if (all(!complete_rules)) stop("Endless loop")

    # move them to parsed_rules and add parentheses, which could be omitted
    # for single characters
    tmp <- gsub(" ", "", rules[complete_rules])
    rules_parsed[complete_rules] <- ifelse(nchar(tmp) == 1, tmp, paste0("(", tmp, ")"))

    # for every new rule, replace its occurences in the rest of the rules
    # add parentheses
    # separately handle rules 42 and 31, so the values in braces in rule 11
    # are not overwritten
    for (rr in names(rules_parsed)[complete_rules]) {
      if (rr %in% c("42", "31")) {
        rules <- gsub(paste0("\\b", rr, "\\b"), rules_parsed[rr], rules)
      } else {
        ind <- which(names(rules) %in% c("8", "11"))
        rules[-ind] <- gsub(paste0("\\b", rr, "\\b"), rules_parsed[rr], rules[-ind])
      }

    }
  }

  # remove whitespaces
  rules_parsed <- gsub(" ", "", rules_parsed)

  # add start/end tags
  rules_parsed <- paste0("^", rules_parsed, "$")

  #
  rules_parsed
}



rules_ex <- head(example, which(example == "") - 1)
parsed_rules_ex <- parseRules2(rules_ex)
sum(grepl(parsed_rules_ex[grep("^0:", rules_ex)], messages_ex))

rules_ex[grep("^8:", rules_ex)] <- "8: 42+"
rules_ex[grep("^11:", rules_ex)] <- "11: 42+ 31+"
parsed_rules_ex <- parseRules2(rules_ex)
sum(grepl(parsed_rules_ex[grep("^0:", rules_ex)], messages_ex))

rules_ex[grep("^11:", rules_ex)] <- "11: 42{1} 31{1} | 42{2} 31{2} | 42{3} 31{3} | 42{4} 31{4} | 42{5} 31{5}"
parsed_rules_ex <- parseRules2(rules_ex)
sum(grepl(parsed_rules_ex[grep("^0:", rules_ex)], messages_ex))



# run
rules <- head(input, which(input == "") - 1)
rules[grep("^8:", rules)] <- "8: 42+"
rules[grep("^11:", rules)] <- "11: 42{1} 31{1} | 42{2} 31{2} | 42{3} 31{3} | 42{4} 31{4} | 42{5} 31{5}"

parsed_rules <- parseRules2(rules)
messages <- tail(input, -which(input == ""))

sum(grepl(parsed_rules[grep("^0:", rules)], messages))
# 403

# good!

# this approach wouldn't work for longer messages, but it's good enough
