library(nimble)

code <- nimbleCode({
  for(i in 1:n) {
    # likelihood - word counts
    y[i, 1:K] ~ dmulti(p[i,1:K], N[i])
    # latent process (random effects) - topic model
    p[i, 1:K] ~ ddirch(alpha[topic[i], 1:K])
  }
  # prior for hyperparameters
  for(tp in 1:M)
    for(k in 1:K)
      alpha[tp, k] ~ dunif(0, 100)
})

const <- list(M = 2, K = 4, n = 5, N = rep(1000, 5),
              topic = c(1, 1, 1, 2, 2))
alphaInits <- rbind(c(10, 30, 100, 3), c(12, 15, 15, 8))
m <- nimbleModel(code, constants = const, 
                 inits = list(alpha = alphaInits))

set.seed(0)
m$simulate(c('p', 'y'))
m$p
m$y

# marginalize out
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

rdirchmulti <- nimbleFunction(
  run = function(n = integer(0), alpha = double(1), 
                 size = double(0)) {
    
    returnType(double(1))
    if(n != 1) print("rdirchmulti only allows n = 1; using n = 1.")
    p <- rdirch(1, alpha)
    return(rmulti(1, size = size, prob = p))
  })

registerDistributions(list(
  ddirchmulti = list(
    BUGSdist = "ddirchmulti(alpha, size)",
    types = c('value = double(1)', 'alpha = double(1)'))
))

code2 <- nimbleCode({
  for(i in 1:n)
    # likelihood 
    y[i,1:K] ~ ddirchmulti(alpha[topic[i], 1:K], N[i])
  # priors for hyperparameters
  for(tp in 1:M)
    for(k in 1:K)
      alpha[tp, k] ~ dunif(0, 100)
})
m2 <- nimbleModel(code2, constants = const,
                  inits = list(alpha = alphaInits))
set.seed(0)
m2$simulate('y')
m2$y

Cm2 <- compileNimble(m2)
m2Conf <- configureMCMC(m2, print = TRUE)
m2MCMC <- buildMCMC(m2Conf)
Cm2MCMC <- compileNimble(m2MCMC, project = m2)
Cm2MCMC$run(100)

