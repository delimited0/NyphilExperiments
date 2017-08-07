library(tidyverse)
library(stringr)

load("../all_programs.RData")

data <- performance %>% 
  dplyr::select(id, work_id, composer, title, movement, conductor) %>%
  
  filter(!is.na(conductor) & conductor != "Not conducted") %>%
  mutate(movement = ifelse(is.na(movement), "", movement)) %>%

  mutate(piece_mvmt = str_c(composer, title, movement, sep = ": ")) %>%
  mutate(piece = str_c(composer, title, sep = ": ")) %>%
  distinct

data$conductor <- data$conductor %>% 
  map_chr(function(x) {
    y <- str_split_fixed(x, ";", Inf)
    return(ifelse(y[1] == "", y[2], y[1]))
  }) %>%
  str_trim

# pieces ----
# accounts for multiple movements of the same piece being played
piece_counts <- data %>%   
  mutate(just_work_id = str_split_fixed(work_id, "\\*", Inf)[,1]) %>% 
  distinct(id, just_work_id, .keep_all = TRUE) %>%
  group_by(conductor) %>%
  count(conductor, piece) %>%
  spread(piece, n, fill = 0)

piece_count_mat <- piece_counts %>%
  ungroup() %>%
  dplyr::select(-conductor) %>%
  as.matrix()
rownames(piece_count_mat) <- piece_counts$conductor %>% as_vector
# piece_count_freq <- piece_count_mat / rowSums(piece_count_mat)

# composers ----
composer_counts <- data %>% 
  group_by(conductor) %>%
  count(conductor, composer) %>%
  spread(composer, n, fill = 0)

composer_count_mat <- composer_counts %>%
  ungroup() %>%
  dplyr::select(-conductor) %>%
  as.matrix()
rownames(composer_count_mat) <- composer_counts$conductor %>% as_vector
composer_freq_mat <- composer_count_mat / rowSums(composer_count_mat)

