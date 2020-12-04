
# Part 1 ------------------------------------------------------------------

# Count the number of valid passports - those that have all required fields. Treat cid as optional. In your batch file, how many passports are valid?

required_fields <- c("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid")


passports <- readLines("inputs/input04.txt")

passport_breaks <- which(passports == "")
passport_starts <- c(1, passport_breaks + 1)
passport_ends <- c(passport_breaks - 1, length(passports))

passports_full <- sapply(seq_along(passport_starts), function(i) {
  paste(passports[passport_starts[i]:passport_ends[i]], collapse = " ")
})

valid_pass <- Reduce(
  `&`,
  lapply(required_fields, function(field) stringi::stri_detect_fixed(passports_full, field))
)
sum(valid_pass)

# 206




# Part 2 ------------------------------------------------------------------


# byr (Birth Year) - four digits; at least 1920 and at most 2002.
# iyr (Issue Year) - four digits; at least 2010 and at most 2020.
# eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
# hgt (Height) - a number followed by either cm or in:
#   If cm, the number must be at least 150 and at most 193.
#   If in, the number must be at least 59 and at most 76.
# hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
# ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
# pid (Passport ID) - a nine-digit number, including leading zeroes.
# cid (Country ID) - ignored, missing or not.

library("magrittr")

# now we need to create some custom checks
# we need to remember to add space od end-sign to mark the end of a field
byr_valid <- stringi::stri_detect_regex(passports_full, "byr:(19[2-9][0-9]|200[0-2])( |$)")

iyr_valid <- stringi::stri_detect_regex(passports_full, "iyr:20(1[0-9]|20)( |$)")

eyr_valid <- stringi::stri_detect_regex(passports_full, "eyr:20(2[0-9]|30)( |$)")

heights <- stringi::stri_extract_first_regex(passports_full, "hgt:[0-9]{2,3}(cm|in)( |$)")
heights <- gsub("hgt:", "", heights)
heights <- gsub(" ", "", heights)
heights_cm <- grepl("cm$", heights)
heights <- as.integer(gsub("(cm)|(in)", "", heights))
hgt_valid <- ifelse(is.na(heights), FALSE,
                    ifelse(heights_cm, heights >= 150 & heights <= 193, heights >= 59 & heights <= 76))

hcl_valid <- stringi::stri_detect_regex(passports_full, "hcl:#[0-9a-f]{6}( |$)")

ecl_valid <- stringi::stri_detect_regex(passports_full, "ecl:(amb|blu|brn|gry|grn|hzl|oth)( |$)")

pid_valid <- stringi::stri_detect_regex(passports_full, "pid:[[:digit:]]{9}( |$)")


pass_valid <- Reduce(
  `&`,
  list(byr_valid, iyr_valid, eyr_valid, hgt_valid, hcl_valid, ecl_valid, pid_valid)
)
sum(pass_valid)
# 123
