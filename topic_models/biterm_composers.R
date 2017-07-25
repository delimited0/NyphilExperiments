source("load_data.R")
source("btm.R")

pair_count <- program_tbl %>% pairwise_count(item = composerName, feature = programID,
                                          sort = TRUE)
# program_tbl %>% pairwise_cor(item = composerName, feature = programID, sort = TRUE)

composer_btm <- btm(program_tbl$composerName, program_tbl$programID, K = 20, iter = 2000)

get_topics(composer_btm, 20)

coherence(composer_btm, 20)

works_btm <- btm(program_tbl$workTitle, program_tbl$programID, K = 20, iter = 2000)
term_freq <- program_tbl %>% count(workTitle)
term_freq <- term_freq[order(match(term_freq$workTitle, works_btm$vocab)), ]
doc_term_freq <- program_tbl %>% count(programID)
works_json <- createJSON(phi = t(works_btm$beta), 
         theta = matrix(rep(works_btm$theta, length(unique(program_tbl$programID))), ncol = 20, byrow = TRUE), 
         doc.length = doc_term_freq$n, 
         vocab = works_btm$vocab, 
         term.frequency = term_freq$n)
serVis(works_json)

get_topics(works_btm, 20)
