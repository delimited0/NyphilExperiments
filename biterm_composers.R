library(tidyverse)
library(tidytext)
library(stringr)
library(devtools)
#install_github("dgrtwo/widyr")
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
  keep(~nrow(.) > 0) %>%
  map(~select(., composerName, workTitle, programID)) %>%
  map(function(x) {
    if (typeof(x$workTitle) == "list")
      # flatten workTitles that are split into lists
      x %>% mutate(workTitle = map_chr(x$workTitle, ~ paste(unlist(.), collapse = " ")))
    else
      x
  }) %>%   # some of the workTitle columns are lists
  map(~filter(., !is.na(workTitle) & workTitle != "")) %>%
  bind_rows

pair_count <- program_tbl %>% pairwise_count(item = composerName, feature = programID,
                                          sort = TRUE)
# program_tbl %>% pairwise_cor(item = composerName, feature = programID, sort = TRUE)

composer_dict <- distinct(program_tbl, composerName)
program_tbl %<>% mutate(composerID = match(composerName, composer_dict$composerName))

btm <- btm_gibbs(program_tbl$composerID[1:16], program_tbl$programID[1:16], 5, .1, .01, 0)


