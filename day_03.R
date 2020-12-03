library(tidyverse)

# --- Day 3: Toboggan Trajectory ---
# With the toboggan login problems resolved, you set off toward the airport. While travel by toboggan might be easy, it's certainly not safe: there's very minimal steering and the area is covered in trees. You'll need to see which angles will take you near the fewest trees.
# 
# Due to the local geology, trees in this area only grow on exact integer coordinates in a grid. You make a map (your puzzle input) of the open squares (.) and trees (#) you can see. For example:
# 
# ..##.......
# #...#...#..
# .#....#..#.
# ..#.#...#.#
# .#...##..#.
# ..#.##.....
# .#.#.#....#
# .#........#
# #.##...#...
# #...##....#
# .#..#...#.#
#
# These aren't the only trees, though; due to something you read about once 
# involving arboreal genetics and biome stability, the same pattern repeats 
# to the right many times:
# 
# ..##.........##.........##.........##.........##.........##.......  --->
# #...#...#..#...#...#..#...#...#..#...#...#..#...#...#..#...#...#..
# .#....#..#..#....#..#..#....#..#..#....#..#..#....#..#..#....#..#.
# ..#.#...#.#..#.#...#.#..#.#...#.#..#.#...#.#..#.#...#.#..#.#...#.#
# .#...##..#..#...##..#..#...##..#..#...##..#..#...##..#..#...##..#.
# ..#.##.......#.##.......#.##.......#.##.......#.##.......#.##.....  --->
# .#.#.#....#.#.#.#....#.#.#.#....#.#.#.#....#.#.#.#....#.#.#.#....#
# .#........#.#........#.#........#.#........#.#........#.#........#
# #.##...#...#.##...#...#.##...#...#.##...#...#.##...#...#.##...#...
# #...##....##...##....##...##....##...##....##...##....##...##....#
# .#..#...#.#.#..#...#.#.#..#...#.#.#..#...#.#.#..#...#.#.#..#...#.#  --->
#
# You start on the open square (.) in the top-left corner and need to reach
#  the bottom (below the bottom-most row on your map).
# 
# The toboggan can only follow a few specific slopes (you opted for a
# cheaper model that prefers rational numbers); start by counting all the 
# trees you would encounter for the slope right 3, down 1:
# 
# From your starting position at the top-left, check the position that is
# right 3 and down 1. Then, check the position that is right 3 and down 1 
# from there, and so on until you go past the bottom of the map.
# 
# The locations you'd check in the above example are marked here with O 
# where there was an open square and X where there was a tree:
# 
# ..##.........##.........##.........##.........##.........##.......  --->
# #..O#...#..#...#...#..#...#...#..#...#...#..#...#...#..#...#...#..
# .#....X..#..#....#..#..#....#..#..#....#..#..#....#..#..#....#..#.
# ..#.#...#O#..#.#...#.#..#.#...#.#..#.#...#.#..#.#...#.#..#.#...#.#
# .#...##..#..X...##..#..#...##..#..#...##..#..#...##..#..#...##..#.
# ..#.##.......#.X#.......#.##.......#.##.......#.##.......#.##.....  --->
# .#.#.#....#.#.#.#.O..#.#.#.#....#.#.#.#....#.#.#.#....#.#.#.#....#
# .#........#.#........X.#........#.#........#.#........#.#........#
# #.##...#...#.##...#...#.X#...#...#.##...#...#.##...#...#.##...#...
# #...##....##...##....##...#X....##...##....##...##....##...##....#
# .#..#...#.#.#..#...#.#.#..#...X.#.#..#...#.#.#..#...#.#.#..#...#.#  --->
# In this example, traversing the map using this slope would cause you to 
# encounter 7 trees.
# 
# Starting at the top-left corner of your map and following a slope of 
# right 3 and down 1, how many trees would you encounter?


slope <- str_split(read_file(file="./data/day_3/slope.txt"), pattern="\\s+", simplify = TRUE)

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


pos_row_1 <- unlist(map(0:(row_count-1), ~ row_num_1(.)))
pos_col_3 <- unlist(map(0:(row_count-1), ~ col_num_3( ., col_count)))

slope[1,]

q_31 <- slope
substr(q_31[pos_row_1],pos_col_3,pos_col_3) <- "X"

q_31 <- q_31[1,]
q_31

path_31 <- str_sub(slope[pos_row_1],pos_col_3,pos_col_3)
path_31

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


pos_col_1 <- unlist(map(0:(row_count-1), ~ col_num_1( ., col_count)))
pos_col_5 <- unlist(map(0:(row_count-1), ~ col_num_5( ., col_count)))
pos_col_7 <- unlist(map(0:(row_count-1), ~ col_num_7( ., col_count)))
pos_row_2 <- unlist(map(0:(row_count-1), ~ row_num_2(.)))[1:162] # because we get to bottom faster


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