library(tidyverse)
library(stringr)

load("../all_programs.RData")

dyn_data <- performance %>% 
  dplyr::select(id, work_id, season, composer, title, movement, conductor) %>%
  
  filter(!is.na(conductor) & conductor != "Not conducted") %>%
  mutate(movement = ifelse(is.na(movement), "", movement)) %>%
  
  mutate(piece_mvmt = str_c(composer, title, movement, sep = ": ")) %>%
  mutate(piece = str_c(composer, title, sep = ": ")) %>%
  distinct %>%
  
  mutate(t = match(season, unique(season)))
  
dyn_data$conductor <- data$conductor %>% 
  map_chr(function(x) {
    y <- str_split_fixed(x, ";", Inf)
    return(ifelse(y[1] == "", y[2], y[1]))
  }) %>%
  str_trim

# pieces
# dyn_piece_counts <- dyn_data %>%
  
  


