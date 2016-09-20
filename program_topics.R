# topic modeling programs!
library(tm)
library(RWeka)

load("programs.RData")

unique_composer_names <- unique(Reduce(c, Map(function(x) x$composerName, programs$works)))
unique_composer_names <- unique_composer_names[!is.na(unique_composer_names)]
unique_composer_names <- str_replace_all(unique_composer_names, "\\s+", " ")

works <- unlist(Reduce(c, Map(function(x) str_replace_all(paste(x$composerName, 
                                                                collapse="@"), 
                                                          "NA", ""),
                              programs$works)))
works <- sapply(X = works, FUN = function(x) {
  str_replace_all(str_replace_all(x, "[:space:]", ""), "@", " ")
})
program_lengths <- sapply(FUN = function(x) length(x$composerName[!is.na(x$composerName)]), X = programs$works)
program_id <- programs$programID[program_lengths > 0]
works = works[program_lengths > 0] 

works_docs <- Corpus(VectorSource(works))
works_dtm <- DocumentTermMatrix(works_docs)

## topic modeling ----
library(topicmodels)

num_topics <- seq(from=5, to=50, by=5)
nyphil_models <- Map(function(n_topics) LDA(x = works_dtm[1:10000,], k = n_topics, method="Gibbs"), num_topics)
perplexities <- sapply(nyphil_models, function(model) perplexity(model, newdata = works_dtm[10001:nrow(works_dtm),]))
get_terms(nyphil_models[[3]], 10)

nyphil_tm <- LDA(x = works_dtm[1:10000,], k = 20, method="Gibbs", control = list(alpha=.1))
perplexity(nyphil_tm, newdata = works_dtm[10001:nrow(works_dtm),])
get_terms(nyphil_tm, 10)

## composer topic over time ----

# sometimes there is no conductor field because its a festival event, etc
# so exclude those concerts
fields_present <- sapply(FUN = function(x) { 
  all(c("composerName", "conductorName") %in% colnames(x))
}, X = programs$works)

comp_cond_time <- Filter(function(z) !is.null(z), 
  Map(function(x, y) {
        if (nrow(x) != 0 & nrow(y) != 0) {
          dat <- cbind(x[,c("composerName", "conductorName")], 
                       rep(as.Date(y[,"Date"]), length(x$composerName)),
                       stringsAsFactors = FALSE)
          colnames(dat) <- c("composer", "conductor", "date")
          return(dat)
        }
      }, programs$works[fields_present], programs$concerts[fields_present])
)

# just ignore missing composers and/or conductors
comp_cond_time <- Map(function(x) {
                        x[!is.na(x$composer) & !is.na(x$conductor),]
                      }, comp_cond_time)

# convert date to 0-1
min_date <- comp_cond_time[[1]]$date[1]
max_date <- comp_cond_time[[length(comp_cond_time)]]$date[1]
comp_cond_time <- Map(function(x) {
    x$date = as.numeric(x$date - min_date) / as.numeric(max_date - min_date)
    return(x)
  }, comp_cond_time)

composers <- unlist(Reduce(c, Map(function(x) x$composer, comp_cond_time)))
composers_unique <- unique(composers)
composers_id <- match(composers, composers_unique)

conductors <- unlist(Reduce(c, Map(function(x) x$conductor, comp_cond_time)))
conductors_unique <- unique(conductors)
conductors_id <- match(conductors, conductors_unique)

# perturb earliest and latest dates to make dealing with 0 and 1 easier
dates <- unlist(Reduce(c, Map(function(x) x$date, comp_cond_time)))
dates[dates == 0] <- 1e-7
dates[dates == 1] <- 1 - 1e-7

# run sampler (straight from the rcpp file)
nyphil_ctot <- ctot_gibbs(composers = composers_id-1, conductors = conductors_id-1, dates = dates,
                    K = 10, alpha = 1, eta = .01, nu = .5, iter = 10)
nyphil_ctot$vocab <- composers_unique
source('~/Documents/Projects/TopicModel/topicmodel/posterior_stats.R')

nyphil_ctot <- ctot(composers = composers, conductors = conductors, dates = dates,
                    K = c(40, 50, 60), iter = 200)
nyphil_ctot <- ctot(composers = composers, conductors = conductors, dates = dates,
                    K = c(20), iter = 200)
nyphil_coherences <- lapply(nyphil_ctot, function(x) coherence(x, 20))

get_topics(nyphil_ctot[[1]], 15)

apply(nyphil_ctot[[2]]$psi, 2, function(psi) {
  curve(dbeta(x, shape1 = psi[1], shape2 = psi[2]), add = TRUE)
})

foo <- mapply(FUN = function(x, y) {
    dbeta(seq(from = 0, to = 1, by =.01), x, y)    
  }, nyphil_ctot[[2]]$psi[1,], nyphil_ctot[[2]]$psi[2,])


curve(dbeta(x, shape1 = nyphil_ctot[[2]]$psi[1, 5], shape2 = nyphil_ctot[[2]]$psi[2, 5]), add = FALSE)

plot(nyphil_ctot[[1]]$theta[conductors_unique == "Bernstein, Leonard"])

## piece topic over time ----
# same model, but use distributions over pieces

piece_cond_time <- Filter(function(z) !is.null(z), 
    Map(function(x, y) {
      if (nrow(x) != 0 & nrow(y) != 0) {
        dat <- cbind(x[, c("composerName", "conductorName")], 
                     rep(as.Date(y[,"Date"]), length(x$composerName)),
                     stringsAsFactors = FALSE)
        colnames(dat) <- c("piece", "conductor", "date")
        dat$piece <- paste(x$composerName, x$workTitle, sep = ": ")
        return(dat)
      }
    }, programs$works[fields_present], programs$concerts[fields_present])
)

piece_cond_time <- Map(function(x) {
  x[!is.na(x$piece) & !is.na(x$conductor),]
}, piece_cond_time)

# convert date to 0-1
min_date <- piece_cond_time[[1]]$date[1]
max_date <- piece_cond_time[[length(piece_cond_time)]]$date[1]
piece_cond_time <- Map(function(x) {
  x$date = as.numeric(x$date - min_date) / as.numeric(max_date - min_date)
  return(x)
}, piece_cond_time)

pieces <- unlist(Reduce(c, Map(function(x) x$piece, piece_cond_time)))
pieces_unique <- unique(pieces)
pieces_id <- match(pieces, pieces_unique)

conductors <- unlist(Reduce(c, Map(function(x) x$conductor, piece_cond_time)))
conductors_unique <- unique(conductors)
conductors_id <- match(conductors, conductors_unique)

dates <- unlist(Reduce(c, Map(function(x) x$date, piece_cond_time)))
dates[dates == 0] <- 1e-7
dates[dates == 1] <- 1 - 1e-7

nyphil_ctot <- ctot_gibbs(composers = pieces_id-1, conductors = conductors_id-1, dates = dates,
                          K = 10, alpha = 1, eta = .01, nu = .5, iter = 1)
nyphil_ctot$vocab <- pieces_unique

get_topics(nyphil_ctot, 15)
curve(dbeta(x, shape1 = nyphil_ctot$psi[1, 13], shape2 = nyphil_ctot$psi[2, 13]), add = FALSE)

plot(nyphil_ctot$theta[conductors_unique == "Bernstein, Leonard"])

