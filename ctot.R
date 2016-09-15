Rcpp::sourceCpp("ctot.cpp")
ctot <- function(composers, conductors, dates, K, alpha = .1, eta = .01, 
                 nu = .5, iter = 200) {
# composers - character
# conductors - character
# dates - numeric, normalized date between 0 and 1
# K - number of topics, can be vector
# nu - beta parameter psi initialization
  
  if (any(K %% 1 != 0)) stop("K must be an integer")
  if (any(K <= 0)) stop("K must be positive")

  composers_unique <- unique(composers)
  composers_id <- match(composers, composers_unique)
  conductors_unique <- unique(conductors)
  conductors_id <- match(conductors, conductors_unique)
  dates[dates == 0] <- 1e-7
  dates[dates == 1] <- 1 - 1e-7
  
  fits <- lapply(X = K, FUN = function(k) {
      fit <- ctot_gibbs(composers_id-1, conductors_id-1, dates, k, alpha, 
               eta, nu, iter)
      fit$vocab <- composers_unique
      fit$K <- k
      fit$perplexity <- perplexity(fit, composers_id, conductors_id, dates)
      return(fit)
    })
  
  return(fits)
}

# this is all wrong don't use
perplexity <- function(fit, new_composers_id, new_conductors_id, new_dates) {
  betas <- log(fit$beta[new_composers_id,])
  thetas <- log(fit$theta[new_conductors_id,])
  # psis <- t(replicate(length(new_dates), fit$psi[1,] - 1)) * 
  #           replicate(fit$K, log(new_dates)) + 
  #         t(replicate(length(new_dates), fit$psi[2,] - 1)) * 
  #           replicate(fit$K, log(1 - new_dates)) - 
  #         log(beta(fit$psi[1,], fit$psi[2,]))
  # sum(betas + thetas + psis)
  sum(betas + thetas)
}

# coherence scores as in Mimno et al 2011
coherence <- function(fit , M) {
  term_score <- term_score(fit)
  vocab_mat <- matrix(data=fit$composer_id, nrow=length(fit$composer_id), M)
  topic_mat <- apply(term_score, 2, 
                     function(x) { vocab_mat[order(x)[1:M]] })
  calc_coherence(fit, topic_mat)
}

get_topics <- function(fit, K) {
  term_score <- term_score(fit)
  vocab_mat <- matrix(data=fit$vocab, nrow=length(fit$vocab), K)
  apply(term_score, 2, function(x) {
    vocab_mat[order(x)[1:K]]
  })
}

# get time distribution of topic k
time_dist <- function(fit, k) {
  curve(dbeta(x, shape1 = fit$psi[1, k], 
              shape2 = fit$psi[2, k]), add = FALSE)
}