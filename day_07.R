library(tidyverse)


rules <- tibble( raw_rule = str_split( 
  read_file(file="./data/day_07/sample_data"),
  pattern="\\n{1,}|(\\r\\n){1,}")) %>% unnest(cols=(raw_rule))


# strip out unneded language: "bags", "bags"
# trim whitespace
# split outermost bag and list of contents

outermost_rules <- rules %>% 
  mutate(raw_rule = str_replace_all(raw_rule,"bags?[:punct:]?","")) %>%
  separate(raw_rule, "contain\\s+",remove = TRUE,into =c("enclosing_bag","content")) %>%
  select(enclosing_bag, content) %>%
  mutate(content = if_else(str_detect(content,"no other"),"",str_squish(content)))

# We now that the outermost rule set,
# split the contents of each enclosing bag, so we have level 1 content
rules <- outermost_rules %>% 
          mutate(contents1 = map(content, ~ str_split(., "(?=\\d{1,4})"))) %>% 
          unnest(cols=c(contents1)) %>% unnest(cols=c(contents1)) %>%
          filter(contents1!="") %>% 
          select(enclosing_bag,contents1)

# expand contents1

rules <- rules %>% 
         mutate(contents2=map(contents1, ~ str_split( . , "(?<=\\d{1,4}) "))) %>%
         unnest(contents2) %>%
         mutate(n=as.integer(map(contents2, ~ .[1])), contents = map(contents2, ~ .[2]))









