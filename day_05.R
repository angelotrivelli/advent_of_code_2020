library(tidyverse)
library(binaryLogic)

# read input, split by empty lines (2 or more consecutive newlines)

seats <- tibble( boardingPass= str_split( 
  read_file(file="./data/day_05/boarding_passes.txt"),
  pattern="\\n{1,}|(\\r\\n){1,}", simplify = FALSE)[[1]])

#seats <- tibble( boardingPass= str_split( 
#  read_file(file="./data/day_05/test_data.txt"),
#  pattern="\\n{1,}|(\\r\\n){1,}", simplify = FALSE)[[1]])


# FBBBFFFLRL

getrow <- function(bp) {
  s <- str_split(bp,"")[[1]][1:7]
  seatRow <- 0:127
  
  for(i in 1:7) {
    numRows <- length(seatRow)
    if ( s[i] == "F") {
      seatRow <- seatRow[1:(numRows/2)]
    } else {
      seatRow <- seatRow[(numRows/2+1):numRows]
    }
  }
  return(seatRow)
  }

getSeat <- function(bp) {
  s <- str_split(bp,"")[[1]][8:10]
  seat <- 0:7
  
  for(i in 1:3) {
    numSeats <- length(seat)
    if ( s[i] == "L") {
      seat <- seat[1:(numSeats/2)]
    } else {
      seat <- seat[(numSeats/2+1):numSeats]
    }
  }
  return(seat)
  }

seats <- seats %>% 
  mutate(row = map (boardingPass, ~ getrow(.))) %>% unnest(cols=c(row)) %>%
  mutate(seat = map (boardingPass, ~ getSeat(.))) %>% unnest(cols=c(seat)) %>%
  mutate(seat_id = row*8 + seat) %>%
  arrange(desc(seat_id))

highest_seat_id <- seats$seat_id[1]
highest_seat_id

# Your puzzle answer was 888.
# The first half of this puzzle is complete! It provides one gold star: *
  

seats <- seats %>% mutate (gap = seat_id - lead(seat_id,n=1,default = 0))

unique(seats$gap)
# gaps have values of 1,2,89. We are in a gap of 2...

neighbor_seat_id <- (seats %>% filter(gap==2))$seat_id[1]

area <- seats %>% filter(seat_id>neighbor_seat_id-5 & seat_id < neighbor_seat_id+5)
area

myseat <- neighbor_seat_id-1
myseat
# Your puzzle answer was 522.
# Both parts of this puzzle are complete! They provide two gold stars: **
  
  





