library(tidyverse)
library(tidytext)
library(stringr)
library(devtools)
library(widyr)
library(magrittr)
library(tm)

setwd("~/Documents/Projects/PerformanceHistory")
load("programs.RData")
set.seed(1)

program_tbl <- programs$works %>% 
  map(function(x) { 
    if ("movement" %in% colnames(x))
      subset(x, select = -movement)
    else
      x
  }) %>%   # because some of the movement columns are lists
  map(as_tibble) %>%
  map2(as.numeric(programs$programID), function(x, y) {
    x %>% mutate(programID = y)
  }) %>%
  keep(~nrow(.) > 0 & "conductorName" %in% colnames(.)) %>%
  map(~select(., composerName, conductorName, workTitle, programID)) %>%
  map(function(x) {
    if (typeof(x$workTitle) == "list")
      # flatten workTitles that are split into lists
      x %>% mutate(workTitle = map_chr(x$workTitle, ~ paste(unlist(.), collapse = " ")))
    else
      x
  }) %>%   # some of the workTitle columns are lists
  map(~filter(., !is.na(workTitle) & workTitle != "")) %>%
  bind_rows

program_tbl %<>% mutate(workTitle = paste(composerName, workTitle, sep = "; "))
composer_dict <- distinct(program_tbl, composerName)
piece_dict <- distinct(program_tbl, workTitle)
program_tbl %<>% mutate(composerID = match(composerName, composer_dict$composerName))
program_tbl %<>% mutate(pieceID = match(workTitle, piece_dict$workTitle))
