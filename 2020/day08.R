
# Part 1 ------------------------------------------------------------------

# Run your copy of the boot code. Immediately before any instruction is executed a second time, what value is in the accumulator?


instructions <- readLines("2020/inputs/input08.txt")
instructions_df <- data.frame(
  instruction = substr(instructions, 1, 3),
  value = as.integer(stringi::stri_extract_first_regex(instructions, " .*$"))
)


instructions_df$visited <- FALSE
accumulator <- 0
current_instruction <- 1

while (TRUE) {
  if (instructions_df$visited[current_instruction]) break
  instructions_df$visited[current_instruction] <- TRUE
  
  inst <- instructions_df$instruction[current_instruction]
  if (inst == "nop") {
    current_instruction <- current_instruction + 1
  } else if (inst == "acc") {
    accumulator <- accumulator + instructions_df$value[current_instruction]
    current_instruction <- current_instruction + 1
  } else {
    current_instruction <- current_instruction + instructions_df$value[current_instruction]
  }
  
}
accumulator
# 1832




# Part 2 ------------------------------------------------------------------


# Fix the program so that it terminates normally by changing exactly one jmp (to nop) or nop (to jmp). What is the value of the accumulator after the program terminates?


# we could either try to change every instruction and try the loop
# or we could go backwards, and when we hit an instruction without predecessor
# we know where the error is

# the latter approach could be faster but seems to be much more complicated
# so we go with the first approach


instructions_df_org <- instructions_df
instructions_df_org$visited <- FALSE



for (i in seq_len(nrow(instructions_df_org))) {
  instructions_df <- instructions_df_org
  
  if (instructions_df$instruction[i] == "nop") {
    instructions_df$instruction[i] <- "jmp"
  } else if (instructions_df$instruction[i] == "jmp") {
    instructions_df$instruction[i] <- "nop"
  } else {
    next
  }
  
  correct_run <- FALSE
  accumulator <- 0
  current_instruction <- 1
  
  while (TRUE) {
    if (current_instruction == nrow(instructions_df) + 1) {
      correct_run <- TRUE
      break
    }
    
    if (instructions_df$visited[current_instruction]) break
    instructions_df$visited[current_instruction] <- TRUE
    
    inst <- instructions_df$instruction[current_instruction]
    if (inst == "nop") {
      current_instruction <- current_instruction + 1
    } else if (inst == "acc") {
      accumulator <- accumulator + instructions_df$value[current_instruction]
      current_instruction <- current_instruction + 1
    } else {
      current_instruction <- current_instruction + instructions_df$value[current_instruction]
    }
    
  }
  
  if (correct_run) {
    print(i)
    print(accumulator)
    break
  }
}

# i
# 360

# accumulator
# 662

