
# Part 1 ------------------------------------------------------------------

# How many bag colors can eventually contain at least one shiny gold bag?

library(purrr)

starting_bag <- "shiny gold"

rules <- readLines("2020/inputs/input07.txt")

# split into puter and inner

rules_df <- rules %>% 
  strsplit(" contain ") %>% 
  map_dfr(~{
    bags <- .x
    
    # extract the outer bag
    outer_bag <- gsub(" bags", "", bags[1])
    
    # extract the inner bags
    inner_bags <- bags[2] %>% 
      strsplit(",") %>%
      unlist() %>%
      trimws() %>% 
      map_df(~{
        bag <- .x
        tmp <- strsplit(bag, " ")[[1]]
        if (tmp[1] == "no") {
          data.frame(inner_bag = "other bags",
                     inner_bag_c = 0L)
        } else {
          data.frame(inner_bag = paste(tmp[2], tmp[3]),
                     inner_bag_c = as.integer(tmp[1]))
        }
        
      })
    
    # join
    df <- inner_bags
    df$outer_bag <- outer_bag
    
    df
  })


# recursively find all parents of the target bag
all_parents <- character()
new_parents <- starting_bag

while (TRUE) {
  # pull outer bags for all the bags from the previous iteration
  new_parents <- rules_df %>% 
    dplyr::filter(inner_bag %in% new_parents) %>% 
    dplyr::pull(outer_bag)
  
  # remove those, that we have already visited - it shouldn't be necessary, because
  # if there is a recursive rule we couldn't solve the problem, but anyway, no extra cost
  new_parents <- setdiff(new_parents, all_parents)
  
  if (length(new_parents) == 0) {
    break
  }
  
  all_parents <- c(all_parents, new_parents)
}

length(all_parents)  
# 274



# Part 2 ------------------------------------------------------------------


# How many individual bags are required inside your single shiny gold bag?


# recursively count all the bags - we have to keep track of the number of bags at each level!
bags_c <- 0
new_bags <- data.frame(inner_bag = starting_bag, bags_c = 1)

while (TRUE) {
  # pull inner bags for all the bags from the previous iteration
  new_bags <- rules_df %>% 
    dplyr::inner_join(new_bags, by = c("outer_bag" = "inner_bag")) %>% 
    dplyr::filter(inner_bag_c > 0) %>% 
    dplyr::mutate(bags_c = bags_c * inner_bag_c) %>% 
    dplyr::select(inner_bag, bags_c)
  
  if (nrow(new_bags) == 0) {
    break
  }
  
  # add the count 
  bags_c <- bags_c + sum(new_bags$bags_c)
}

bags_c  
# 158730

