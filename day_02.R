library(tidyverse)

# --- Day 2: Password Philosophy ---
# Your flight departs in a few days from the coastal airport; 
# the easiest way down to the coast from here is via toboggan.
# 
# The shopkeeper at the North Pole Toboggan Rental Shop is having a
# bad day. "Something's wrong with our computers; we can't log in!"
# You ask if you can take a look.
# 
# Their password database seems to be a little corrupted: some of
# the passwords wouldn't have been allowed by the Official Toboggan
# Corporate Policy that was in effect when they were chosen.
# 
# To try to debug the problem, they have created a list (your puzzle
# input) of passwords (according to the corrupted database) and the
# corporate policy when that password was set.
# 
# For example, suppose you have the following list:
# 
# 1-3 a: abcde
# 1-3 b: cdefg
# 2-9 c: ccccccccc
# Each line gives the password policy and then the password. The
# password policy indicates the lowest and highest number of times a
# given letter must appear for the password to be valid. For example,
# 1-3 a means that the password must contain a at least 1 time and at
# most 3 times.
# 
# In the above example, 2 passwords are valid. The middle password,
# cdefg, is not; it contains no instances of b, but needs at least 1.
# The first and third passwords are valid: they contain one a or nine
# c, both within the limits of their respective policies.
# 
# How many passwords are valid according to their policies?

p_raw <- read_file(file="./data/day_2/passwords.txt")

isExpectedFormat = str_count(p_raw,"\\d+\\-\\d+\\s+[a-z]{1}:\\s+[a-z]+")


p <- tibble( num_min = str_extract_all(p_raw, "\\d+(?=-)"), 
             num_max = str_extract_all(p_raw, "(?<=\\-)\\d+"),
             letter  = str_extract_all(p_raw, "[a-z]{1}(?=:)"),
             password = str_extract_all(p_raw, "(?<=:\\s{1,4})\\w+")) %>%
            unnest(cols = c(num_min, num_max, letter, password))


p$num_min <- as.numeric(p$num_min)
p$num_max <- as.numeric(p$num_max)

p <- p %>% 
     mutate(letter_count = str_count(password,letter)) %>%
     mutate(isValid = letter_count>=num_min & letter_count <=num_max) %>%
     filter(isValid) 

num_valid_part1 = nrow(p)
num_valid_part1

# Your puzzle answer was 550
# answer was correct -AT.


# --- Part Two ---
# While it appears you validated the passwords correctly, they don't
# seem to be what the Official Toboggan Corporate Authentication System
# is expecting.
# 
# The shopkeeper suddenly realizes that he just accidentally explained
# the password policy rules from his old job at the sled rental place
# down the street! The Official Toboggan Corporate Policy actually
# works a little differently.
# 
# Each policy actually describes two positions in the password, where
# 1 means the first character, 2 means the second character, and so on.
# (Be careful; Toboggan Corporate Policies have no concept of "index
# zero"!) Exactly one of these positions must contain the given letter.
# Other occurrences of the letter are irrelevant for the purposes of
# policy enforcement.
# 
# Given the same example list from above:
# 
# 1-3 a: abcde is valid: position 1 contains a and position 3 does not.
# 1-3 b: cdefg is invalid: neither position 1 nor position 3 contains b.
# 2-9 c: ccccccccc is invalid: both position 2 and position 9 contain c.
# How many passwords are valid according to the new interpretation of
# the policies?


q <- tibble( pos1 = str_extract_all(p_raw, "\\d+(?=-)"), 
             pos2 = str_extract_all(p_raw, "(?<=\\-)\\d+"),
             letter  = str_extract_all(p_raw, "[a-z]{1}(?=:)"),
             password = str_extract_all(p_raw, "(?<=:\\s{1,4})\\w+")) %>%
  unnest(cols = c(pos1, pos2, letter, password))
q$pos1 <- as.numeric(q$pos1)
q$pos2 <- as.numeric(q$pos2)

q <- q %>% 
     mutate(has_pos1 = str_detect(password,paste("^[a-z]{",pos1-1,"}",letter,sep=""))) %>%
     mutate(has_pos2 = str_detect(password,paste("^[a-z]{",pos2-1,"}",letter,sep=""))) %>%
     mutate(isValid = xor(has_pos1,has_pos2)) %>% filter(isValid)

num_valid_part2 <- nrow(q)
num_valid_part2

# Your puzzle answer was 634.

# Both parts of this puzzle are complete! They provide two gold stars: **
