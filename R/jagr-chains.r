
jagr_chains <- function (mcmc, jags) {
  
  stopifnot(all(unlist(lapply(mcmc, is.mcarray))))
  
  object <- list(mcmc = mcmc, jags = jags)
  class(object) <- "jagr_chains"
  
  object <- revise(object)
  return (object)
}
