Rcpp::sourceCpp("btm.cpp")
Rcpp::sourceCpp("ctot_stats.cpp")

btm <- function(tokens, doc_ids, K, alpha = .1, eta = .01, iter = 200) {
  # tokens - character
  # doc_id - document id for each token
  
  tokens_unique <- unique(tokens)
  token_ids <- match(tokens, tokens_unique)
  fit <- btm_gibbs(token_ids-1, doc_ids, K, alpha, eta, iter)
  fit$vocab <- tokens_unique
  fit$K <- K
  
  return(fit)
}