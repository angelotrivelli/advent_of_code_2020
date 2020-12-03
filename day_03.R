library(tidyverse)


slope <- str_split(read_file(file="./data/day_03/slope.txt"), pattern="\\s+", simplify = TRUE)

# here's the dimensions... 
row_count <- length(slope)
col_count <- unique(unlist(map(slope, ~ str_length(.))))

# For each step down, there are 3 steps right.
# So, 

steps_right <- (row_count-1) * 3
cell_repeats <- ceiling(steps_right/col_count)

# each move is 3 right, 1 down.
# there are (row_count - 1) moves to get to the bottom.

row_num_1 <- function(i)
  {
    return(i+1)
  }

col_num_3 <- function(i,col_count) {
    x <- (3*i+1) %% col_count
    if (x==0) {
        x <- col_count
      }
  return(x)
}

# The position sequences for row and col for the path from top-left to bottom:
pos_row_1 <- unlist(map(0:(row_count-1), ~ row_num_1(.)))
pos_col_3 <- unlist(map(0:(row_count-1), ~ col_num_3( ., col_count)))

# display the input slope
slope[1,]

q_31 <- slope
substr(q_31[pos_row_1],pos_col_3,pos_col_3) <- "X"

q_31 <- q_31[1,]
q_31  # display the path down with "X" indicating each step.

# Get the char on each position on the path, it's either a "." or a "#" (tree)
path_31 <- str_sub(slope[pos_row_1],pos_col_3,pos_col_3)
path_31

# Count all the trees, the "#'s"
num_trees_31 <- as.double(sum(str_count(path_31,"#")))
num_trees_31

# Your puzzle answer was 145.

# The first half of this puzzle is complete! It provides one gold star: *



# --- Part Two ---
# Time to check the rest of the slopes - you need to minimize the
# probability of a sudden arboreal stop, after all.
# 
# Determine the number of trees you would encounter if, for each 
# of the following slopes, you start at the top-left corner and 
# traverse the map all the way to the bottom:
# 
# Right 1, down 1.
# Right 3, down 1. (This is the slope you already checked.)
# Right 5, down 1.
# Right 7, down 1.
# Right 1, down 2.
# In the above example, these slopes would find 2, 7, 3, 4, and 
# 2 tree(s) respectively; multiplied together, these produce the
# answer 336.
# 
# What do you get if you multiply together the number of trees
# encountered on each of the listed slopes?

# modify functions for different step sizes

row_num_2 <- function(i)
{
  return(2*i+1)
}

col_num_1 <- function(i,col_count) {
  x <- (i+1) %% col_count
  if (x==0) {
    x <- col_count
  }
  return(x)
}

col_num_5 <- function(i,col_count) {
  x <- (5*i+1) %% col_count
  if (x==0) {
    x <- col_count
  }
  return(x)
}

col_num_7 <- function(i,col_count) {
  x <- (7*i+1) %% col_count
  if (x==0) {
    x <- col_count
  }
  return(x)
}

# Right 1, down 1.
# Right 3, down 1. (This is the slope you already checked.)
# Right 5, down 1.
# Right 7, down 1.
# Right 1, down 2.

# here's the sequences...
pos_col_1 <- unlist(map(0:(row_count-1), ~ col_num_1( ., col_count)))
pos_col_5 <- unlist(map(0:(row_count-1), ~ col_num_5( ., col_count)))
pos_col_7 <- unlist(map(0:(row_count-1), ~ col_num_7( ., col_count)))
pos_row_2 <- unlist(map(0:(row_count-1), ~ row_num_2(.)))[1:162] # because we get to bottom faster


# compute the rest...

# Right 1, down 1.
q_11 <- slope
substr(q_11[pos_row_1],pos_col_1,pos_col_1) <- "X"

q_11 <- q_11[1,]
q_11

path_11 <- str_sub(slope[pos_row_1],pos_col_1,pos_col_1)
path_11

num_trees_11 <- as.double(sum(str_count(path_11,"#")))
num_trees_11



# Right 5, down 1.
q_51 <- slope
substr(q_51[pos_row_1],pos_col_5,pos_col_5) <- "X"

q_51 <- q_51[1,]
q_51

path_51 <- str_sub(slope[pos_row_1],pos_col_5,pos_col_5)
path_51

num_trees_51 <- as.double(sum(str_count(path_51,"#")))
num_trees_51


# Right 7, down 1.
q_71 <- slope
substr(q_71[pos_row_1],pos_col_7,pos_col_7) <- "X"

q_71 <- q_71[1,]
q_71

path_71 <- str_sub(slope[pos_row_1],pos_col_7,pos_col_7)
path_71

num_trees_71 <- as.double(sum(str_count(path_71,"#")))
num_trees_71



# Right 1, down 2.
q_12 <- slope
substr(q_12[pos_row_2],pos_col_1[1:162],pos_col_1[1:162]) <- "X"

q_12 <- q_12[1,]
q_12

path_12 <- str_sub(slope[pos_row_2],pos_col_1[1:162],pos_col_1[1:162])
path_12

num_trees_12 <- as.double(sum(str_count(path_12,"#")))
num_trees_12



tree_product <- num_trees_11 * num_trees_12 * num_trees_31 * num_trees_51 * num_trees_71
tree_product

# Your puzzle answer was 3424528800.

# Both parts of this puzzle are complete! They provide two gold stars: **