# looking at the composer data

library(jsonlite)
library(stringr)
library(ggplot2)
library(data.table)

programs <- fromJSON('Programs/json/complete.json')
programs <- programs$programs

# top 10 composers played all time ----
composer_names <- Reduce(c, Map(function(x) x$composerName, programs$works))
composer_names <- composer_names[!is.na(composer_names)]
composer_names <- str_replace_all(composer_names, "\\s+", " ")

composer_table <- table(composer_names)
composer_df <- as.data.frame(sort(composer_table, decreasing=TRUE), 
                             stringsAsFactors = FALSE)

ggplot(data=composer_df[1:20,], aes(x=reorder(composer_names, -Freq), y=Freq)) +
  geom_bar(stat="identity") + 
  xlab("Composer") + ylab("Count") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

# top composers by conductor ----
comp_cond_available <- sapply(FUN = function(x) {
  prod(c("composerName", "conductorName") %in% colnames(x)) == TRUE}, 
       X = programs$works)
comp_cond <- rbindlist(Map(function(x) x[,c("composerName", "conductorName")], 
                        programs$works[comp_cond_available]))
comp_cond <- comp_cond[!is.na(comp_cond$composerName) & !is.na(comp_cond$conductorName), ]
comp_cond <- as.data.frame(sapply(X = comp_cond, 
                                  function(x) str_replace_all(x, "\\s+", " ")), 
                           stringsAsFactors = FALSE)

gilbert_comp <- comp_cond[comp_cond$conductorName == "Gilbert, Alan",]
zweden_comp <- comp_cond[comp_cond$conductorName == "Zweden, Jaap van",]

gilbert_table <- table(gilbert_comp$composerName)
gilbert_df <- as.data.frame(sort(gilbert_table, decreasing=TRUE), 
                             stringsAsFactors = FALSE)
ggplot(data=gilbert_df[1:20,], aes(x=reorder(Var1, -Freq), y=Freq)) +
  geom_bar(stat="identity") + 
  xlab("Composer") + ylab("Count") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

# save composer-conductor data and programs
save(programs, file="programs.RData")
save(comp_cond, file="comp_cond.RData")