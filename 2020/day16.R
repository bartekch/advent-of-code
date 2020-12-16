
# Part 1 ------------------------------------------------------------------

# What is your ticket scanning error rate?


input <- readLines("inputs/input16.txt")
tmp <- which(input == "")
rules <- input[1:(tmp[1] - 1)]
my_ticket <- input[(tmp[1] + 2):(tmp[2] - 1)]
tickets <- input[(tmp[2] + 2):length(input)]


rules_df <- lapply(seq_along(rules), function(i) {
  tmp <- unlist(strsplit(rules[i], ": "))
  rule_name <- tmp[1]
  ranges <- unlist(strsplit(tmp[2], " or "))


  ranges <- do.call(
    rbind,
    lapply(
      strsplit(ranges, "-"),
      function(s) data.frame(from = as.integer(s[1]), to = as.integer(s[2]))
    )
  )


  ranges$rule_name <- rule_name
  ranges
})
rules_df <- do.call(rbind, rules_df)


tickets_s <- strsplit(tickets, ",")
tickets_df <- do.call(
  rbind,
  lapply(seq_along(tickets_s),
         function(i) data.frame(ticket_id = i, field = as.integer(tickets_s[[i]])))
)


# filter out for every rule
tickets_df_invalid <- tickets_df
for (i in seq_len(nrow(rules_df))) {
  tickets_df_invalid <- tickets_df_invalid[tickets_df_invalid$field < rules_df$from[i] |
                                             tickets_df_invalid$field > rules_df$to[i], ]
}

sum(tickets_df_invalid$field)
# 20091


# Part 2 ------------------------------------------------------------------


# DISCARD invalid tickets
tickets <- tickets[-tickets_df_invalid$ticket_id]

# add YOUR ticket
tickets <- c(my_ticket, tickets)

# parse once again
tickets_s <- strsplit(tickets, ",")
tickets_df <- do.call(
  rbind,
  lapply(seq_along(tickets_s),
         function(i) data.frame(ticket_id = i,
                                field_id = seq_along(tickets_s[[i]]),
                                field = as.integer(tickets_s[[i]])))
)

# ticket_id is not necessary here
tickets_df_w <- tidyr::pivot_wider(tickets_df, names_from = field_id, values_from = field)
tickets_df_w$ticket_id <- NULL

# for every rule get a set of fields that comply with this range
rules_correct <- lapply(unique(rules_df$rule_name), function(rule) {
  rules_ranges <- rules_df[rules_df$rule_name == rule, ]

  # check the fields one by one
  correct_fields <- sapply(tickets_df_w, function(field) {
    # check whether the y fall into any of the ranges
    tmp <- lapply(
      seq_len(nrow(rules_ranges)),
      function(i) {
        field >= rules_ranges$from[i] & field <= rules_ranges$to[i]
      }
    )

    # combina ranges with OR
    all(Reduce('|', tmp))
  })
  data.frame(rule_name = rule,
             correct_field = which(correct_fields))
})
rules_correct <- do.call(rbind, rules_correct)


# iterating find a rule with a single correct field and remove it from the possibilities
rules_correct_tmp <- rules_correct
rules_res <- rules_correct_tmp[0,]
while (TRUE) {
  if (nrow(rules_correct_tmp) == 0) break

  # find the rule with single correct field - if more than one choose the first one
  tt <- table(rules_correct_tmp$rule_name)
  tt <- head(tt[tt == 1], 1)
  if (length(tt) == 0) {
    print("No more rules with single field!")
    break
  }

  new_rule <- rules_correct_tmp[rules_correct_tmp$rule_name == names(tt),]
  rules_res <- rbind(rules_res, new_rule)

  rules_correct_tmp <- rules_correct_tmp[rules_correct_tmp$correct_field != new_rule$correct_field,]
}

rules_res

# Once you work out which field is which, look for the six fields on your ticket that start with the word departure. What do you get if you multiply those six values together?

format(prod(as.numeric(tickets_s[[1]][rules_res$correct_field[grepl("^departure", rules_res$rule_name)]])), scientific = FALSE)
# 2325343130651
