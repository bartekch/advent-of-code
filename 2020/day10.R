
# Part 1 ------------------------------------------------------------------

# What is the number of 1-jolt differences multiplied by the number of 3-jolt differences?

adapters <- as.integer(readLines("2020/inputs/input10.txt"))

prod(table(diff(sort(c(0, adapters, max(adapters + 3))))))
# 2664



# Part 2 ------------------------------------------------------------------

# What is the total number of distinct ways you can arrange the adapters to connect the charging outlet to your device?
      
# going backwards count the number of different arrangements from the given point

adapters <- data.frame(adapter = sort(c(0, adapters)),
                       arr_c = 0)
adapters$arr_c[nrow(adapters)] <- 1

for (i in (nrow(adapters)-1):1) {
  # sum all possible ways for adapters that are reachable from the given point
  tmp <- adapters$arr_c[adapters$adapter %in% (adapters$adapter[i] + 1:3)]
  adapters$arr_c[i] <- sum(tmp) 
}
head(adapters)
format(adapters$arr_c[1], scientific = FALSE)
# 148098383347712
