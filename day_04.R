library(tidyverse)

# read input, split by empty lines (2 or more consecutive newlines)
pinput <- str_split( 
            read_file(file="./data/day_04/passport_batch_file.txt"),
            pattern="\\n{2,}|(\\r\\n){2,}", simplify = FALSE)[[1]]

# expected passport fields are...
# byr (Birth Year)
# iyr (Issue Year)
# eyr (Expiration Year)
# hgt (Height)
# hcl (Hair Color)
# ecl (Eye Color)
# pid (Passport ID)
# cid (Country ID)

# cid is optional, to allow citizens of the north pole who have no country code

p <- tibble(pinput) %>% 
      mutate(
        byr=str_extract(pinput,"(?<=byr:)[:graph:]+"),
        iyr=str_extract(pinput,"(?<=iyr:)[:graph:]+"),
        eyr=str_extract(pinput,"(?<=eyr:)[:graph:]+"),
        hgt=str_extract(pinput,"(?<=hgt:)[:graph:]+"),
        hcl=str_extract(pinput,"(?<=hcl:)[:graph:]+"),
        ecl=str_extract(pinput,"(?<=ecl:)[:graph:]+"),
        pid=str_extract(pinput,"(?<=pid:)[:graph:]+"),
        cid=str_extract(pinput,"(?<=cid:)[:graph:]+")
      )

# valid passports have all of the above fields except cid, we don't care about
# that field. 

p <- p %>% drop_na(c(byr,iyr,eyr,hgt,hcl,ecl,pid))

num_valid_passports_part1 <- nrow(p)
num_valid_passports_part1

# Your puzzle answer was 170.
# The first half of this puzzle is complete! It provides one gold star: *
  
  
# now perform further validation...

# byr (Birth Year) - four digits; at least 1920 and at most 2002.
# iyr (Issue Year) - four digits; at least 2010 and at most 2020.
# eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
# hgt (Height) - a number followed by either cm or in:
#   If cm, the number must be at least 150 and at most 193.
#   If in, the number must be at least 59 and at most 76.
# hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
# ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
# pid (Passport ID) - a nine-digit number, including leading zeroes.
# cid (Country ID) - ignored, missing or not.

# next pass of validation, check form of fields...
p <- p %>% 
  mutate(
    byr=str_extract(byr,"^\\d{4}$"),
    iyr=str_extract(iyr,"^\\d{4}$"),
    eyr=str_extract(eyr,"^\\d{4}$"),
    hgt=str_extract(hgt,"^\\d{2,3}(cm|in)$"),
    hcl=str_extract(hcl,"^#[\\da-f]{6}$"),
    ecl=str_extract(ecl,"^(amb|blu|brn|gry|grn|hzl|oth)$"),
    pid=str_extract(pid,"^\\d{9}$")
  )

# drop the ones with bad form
p <- p %>% drop_na(c(byr,iyr,eyr,hgt,hcl,ecl,pid))


# now check ranges, first convert to integers
p$byr <- as.integer(p$byr)
p$iyr <- as.integer(p$iyr)
p$eyr <- as.integer(p$eyr)

# separate height value and unit, convert value to integer
p <- p %>% 
      mutate( hgt_unit = str_extract(hgt,"(cm|in)")) %>%
      mutate( hgt = as.integer(str_extract(hgt,"\\d+")))

# check range
# byr (Birth Year) - four digits; at least 1920 and at most 2002.
# iyr (Issue Year) - four digits; at least 2010 and at most 2020.
# eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
        
p <- p %>% 
      mutate(dates_ok =
        (byr>=1920 & byr<=2002) & 
        (iyr>=2010 & iyr<=2020) & 
        (eyr>=2020 & eyr<=2030))

# check height, be mindful of units
#   If cm, the number must be at least 150 and at most 193.
#   If in, the number must be at least 59 and at most 76.
p <- p %>%
      mutate(hgt_ok = if_else(hgt_unit=="cm" & hgt>=150 & hgt<=193, TRUE , FALSE)) %>%
      mutate(hgt_ok = if_else(hgt_unit=="in" & hgt>=59 & hgt<=76, TRUE , hgt_ok)) 

p <- p %>% filter(hgt_ok & dates_ok)
num_valid_passports_part2 <- nrow(p)
num_valid_passports_part2

# Your puzzle answer was 103.
# Both parts of this puzzle are complete! They provide two gold stars: **
  
  
