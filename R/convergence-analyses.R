#' Combine Models
#'
#' @inheritParams convergence
#' @param object2 A JAGS object. 
#' @export
convergence_analyses <- function(object, object2, parm = "all", combine = TRUE) {
  if (!is.jags_analysis(object)) stop("object must be a jags_analysis object")
  if (!is.jags_analysis(object2)) stop("object2 must be a jags_analysis object")
  check_scalar(parm, c("all", "fixed", "random"))
  check_flag(combine)

  stopifnot(is_one_model(object))
  stopifnot(is_one_model(object2))
  
  analysis1 <- analysis(object)
  analysis2 <- analysis(object2)
  
  parm1 <- expand_parm(analysis1, parm)
  parm2 <- expand_parm(analysis1, parm)
  
  if (!identical(parm1, parm2)) stop("objects have different parameters")
  if (!identical(dataset(object), dataset(object2))) stop("objects have different datasets")
  
  mcmc1 <- as.mcmc.list(as.jagr_chains(analysis1))
  mcmc2 <- as.mcmc.list(as.jagr_chains(analysis2))
  
  mcmc1 <- runjags::combine.mcmc(list(mcmc1))
  mcmc2 <- runjags::combine.mcmc(list(mcmc2))
  niter <- min(niter(mcmc1), niter(mcmc2))
  mcmc1 <- as.mcmc(mcmc1[seq_len(niter),])
  mcmc2 <- as.mcmc(mcmc2[seq_len(niter),])
  mcmc <- list(mcmc1, mcmc2)
  class(mcmc) <- "mcmc.list"
  rhat <- convergence(mcmc, vars = parm1)
  if (combine)
    return(max(rhat, na.rm = TRUE))
  rhat
}
