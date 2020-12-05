library(tidyverse)

# read input, split by empty lines (2 or more consecutive newlines)
binput <- str_split( 
  read_file(file="./data/day_05/boarding_passes.txt"),
  pattern="\\n{1,}|(\\r\\n){1,}", simplify = FALSE)[[1]]
