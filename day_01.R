library(tidyverse)


# --- Day 1: Report Repair ---
# After saving Christmas five years in a row, you've decided to take a vacation at
# a nice resort on a tropical island. Surely, Christmas will go on without you.
# 
# The tropical island has its own currency and is entirely cash-only. The gold coins
# used there have a little picture of a starfish; the locals just call them stars. 
# None of the currency exchanges seem to have heard of them, but somehow, you'll need
# to find fifty of these coins by the time you arrive so you can pay the deposit on 
# your room.
# 
# To save your vacation, you need to get all fifty stars by December 25th.
# 
# Collect stars by solving puzzles. Two puzzles will be made available on each day in
# the Advent calendar; the second puzzle is unlocked when you complete the first. Each 
# puzzle grants one star. Good luck!
# 
# Before you leave, the Elves in accounting just need you to fix your expense report 
# (your puzzle input); apparently, something isn't quite adding up.
# 
# Specifically, they need you to find the two entries that sum to 2020 and then multiply 
# those two numbers together.
# 
# For example, suppose your expense report contained the following:
# 
# 1721
# 979
# 366
# 299
# 675
# 1456
# In this list, the two entries that sum to 2020 are 1721 and 299. Multiplying them 
# together produces 1721 * 299 = 514579, so the correct answer is 514579.
# 
# Of course, your expense report is much larger. Find the two entries that sum to 2020; 
# what do you get if you multiply them together?
# 

d <- read_file(file="data/day_01/day_1.txt")

d <- tibble(e=str_extract_all(d,"\\d+")) %>% unnest(cols = c(e))

d$e <- as.numeric(d$e)




combos_2 <- combn( x=d$e, m=2, simplify=FALSE) 
sum_to_2020 <- map ( combos_2, ~ .[1] + .[2])
f1 <- tibble(combos_2, sum_to_2020) %>% filter(sum_to_2020 == 2020) 
part1 = f1$combos_2[[1]][1] * f1$combos_2[[1]][2]
part1

# Your puzzle answer was 751776.
# 
# The first half of this puzzle is complete! It provides one gold star: *
# 



# --- Part Two ---
# The Elves in accounting are thankful for your help; one of them even offers you a starfish coin 
# they had left over from a past vacation. They offer you a second one if you can find three 
# numbers in your expense report that meet the same criteria.
# 
# Using the above example again, the three entries that sum to 2020 are 979, 366, and 675. 
# Multiplying them together produces the answer, 241861950.
# 
# In your expense report, what is the product of the three entries that sum to 2020?


combos_3 <- combn( x=d$e, m=3, simplify=FALSE) 
sum_to_2020 <- map ( combos_3, ~ .[1] + .[2] + .[3])
f2 <- tibble(combos_3, sum_to_2020) %>% filter(sum_to_2020 == 2020) 
part2 = f2$combos_3[[1]][1] * f2$combos_3[[1]][2] * f2$combos_3[[1]][3]
part2

# Your puzzle answer was 42275090.

# Both parts of this puzzle are complete! They provide two gold stars: **
