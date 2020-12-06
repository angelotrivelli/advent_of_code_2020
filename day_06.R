library(tidyverse)

# read input, split by empty lines (2 or more consecutive newlines)

# For each group, count the number of questions to which anyone 
# answered "yes". What is the sum of those counts?

customs <- tibble( ans = str_split( 
                    read_file(file="./data/day_06/customs_answers.txt"),
                    pattern="\\n{2,}|(\\r\\n){2,}", simplify = FALSE)[[1]] ) %>%
            mutate(group = 1:n()) %>%
            select(ans,group) %>% 
            mutate (group_ans = str_split(ans,pattern="\\s")) %>% 
            unnest(cols=c(group_ans)) %>%
            mutate(person = 1:n()) %>%
            select(ans,group,person,group_ans) %>%
            mutate(person_ans= map( group_ans, ~ str_split(.,"")[[1]])) %>%
            unnest(cols=c(person_ans))

part1 <-    customs %>%
            group_by(group) %>% 
            distinct(person_ans) %>%
            summarise(tally=n()) %>% ungroup() 

part_1_sum_of_yes_answers = sum(part1$tally)
part_1_sum_of_yes_answers

# Your puzzle answer was 6596.
# The first half of this puzzle is complete! It provides one gold star: *

# part 2
# You don't need to identify the questions to which anyone answered "yes"; 
# you need to identify the questions to which everyone answered "yes"!


part2   <- customs %>%
              select(group,person,person_ans)

groups <- part2 %>% group_by(group) %>% distinct(person) %>% summarise(num_people=n()) %>% ungroup()

part2 <- part2 %>% 
            group_by(person_ans,group) %>% distinct(person) %>% summarise(num_ans=n()) %>% ungroup()

part2 <- part2 %>% left_join(groups, by=c("group")) 

part2 <- part2 %>%
            mutate(didAllAnsweryes= if_else(num_ans==num_people,TRUE,FALSE)) %>% 
            filter(didAllAnsweryes) %>%
            group_by(group) %>% summarise(n=n()) %>% ungroup()
part_2_sum_yes_answers_where_everyone_answered = sum(part2$n)
part_2_sum_yes_answers_where_everyone_answered

# Your puzzle answer was 3219.
# Both parts of this puzzle are complete! They provide two gold stars: **
  
  





