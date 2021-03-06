library(tidyverse)


rules <- tibble( raw_rule = str_split( 
  read_file(file="./data/day_07/bag_rules.txt"),
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


find_enclosing_bags <- function(bags, rules, tally) {
  
  x <- (rules %>% filter(str_detect(content1, paste(bags,collapse="|"))))$enclosing_bag
  if (length(x)==0)
    {
      return(tally)
    }
  else
    {
      tally <- unique(c(x, find_enclosing_bags(x,rules,tally)))
    }
}

tally <- find_enclosing_bags(c("shiny gold"),rules,c())
tally
length(tally)

# Your puzzle answer was 242.
# The first half of this puzzle is complete! It provides one gold star: *
  


  




