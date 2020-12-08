library(tidyverse)


rules <- tibble( raw_rule = str_split( 
  read_file(file="./data/day_07/bag_rules_part2.txt"),
  pattern="\\n{1,}|(\\r\\n){1,}")) %>% unnest(cols=(raw_rule))

rules <- rules %>% 
  mutate(raw_rule = str_replace_all(raw_rule,"bags?","")) %>%
  mutate(raw_rule = str_replace_all(raw_rule," ?\\.$","")) %>%
  separate(raw_rule, "contain\\s+",remove = TRUE,into =c("enclosing_bag","content")) %>%
  mutate(enclosing_bag = str_squish(enclosing_bag)) %>%
  select(enclosing_bag, content) %>%
  mutate(content1=map(content, ~ str_split( . , ","))) %>% 
  unnest(content1) %>% unnest(content1) %>% mutate(content1=str_squish(content1)) %>%
  mutate(qty1= str_extract(content1,"\\d+")) %>%
  mutate(content1= str_extract(content1,"[:alpha:]+\\s[:alpha:]+")) %>%
  select(enclosing_bag,qty1,content1) 

rules[which(is.na(rules$qty1)),]$qty1 <- "0"


find_enclosing_bags <- function(bags, rules, tally) {
  
  x <- rules %>% filter(str_detect(enclosing_bag, paste(bags,collapse="|")))
  xs <- sum(as.numeric(x$qty1))
  if (xs==0)
  {
    return(tally)
  }
  else
  {
    tally <- xs + find_enclosing_bags(x$content1,rules,tally)
  }
}

total <- find_enclosing_bags(c("shiny gold"),rules,0)
total









