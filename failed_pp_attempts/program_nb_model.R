library(nimble)
library(Rcpp)
source("load_data.R")
sourceCpp("biterm_vectors.cpp")

# from https://github.com/paciorek/nimble-ncar-2016/blob/master/user_dist.Rmd
ddirchmulti <- nimbleFunction(
  run = function(x = double(1), alpha = double(1), size = double(0),
                 log = integer(0, default = 0)) {
    
    returnType(double(0))
    logProb <- lgamma(size) - sum(lgamma(x)) + 
      lgamma(sum(alpha)) -
      sum(lgamma(alpha)) + sum(lgamma(alpha + x)) - 
      lgamma(sum(alpha) + size)
    if(log) return(logProb)
    else return(exp(logProb))
  })

biterm_nb_model <- nimbleCode({
  for (b in 1:B) {
    z[b, 1:K] ~ ddirchmulti(alpha, num_topic[1:K])
    
    for (n in 1:N) {
      x[n, 1:K] ~ ddirchmulti(eta[n, 1:V[n]], word_topic[])
    }
    
  }
  for (k in 1:K) {
    num_topic[k] <- sum(z[, k]) 
    for ()
  }
  
})