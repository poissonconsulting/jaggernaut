
#' @export
update_jags <- function (object, ...) {
  UseMethod("update_jags", object)
}

#' @method updated jags_mcmc
update_jags.jags_mcmc <- function (object, ...)
{

}

#' @method update_jags jagr_analysis
#' @export 
update_jags.jagr_analysis <- function (object, quiet = FALSE, ...)
{  
  fun2 <- function (jags, monitor, n.sim, n.thin, quiet, recompile)
  {
    n.sim <- as.integer(n.sim)
    n.thin <- as.integer(n.thin)
    
    if (quiet) {
      options(jags.pb = "none")
    } else {
      options(jags.pb = "text")
    }
    
    if (recompile)
      jags$recompile()
    
    mcmc <- jags.samples(
      model = jags, variable.names = monitor, n.iter = n.sim, thin = n.thin
    )
    
    mcmc <- jags_mcmc(mcmc=mcmc,jags=list(jags))
    return (mcmc)
  }
  
  n.chain <- nchain(object)
  n.sim <- object$iterations
  n.thin <- max(1, floor(n.chain * n.sim / nsim(object)))
  
  monitor <- monitor(object)  
  jags <- object$mcmc$jags
  
  parallel <- length(jags) > 1
  
  ptm <- proc.time()
  
  
  if (parallel) {
    
    doMC::registerDoMC(cores=n.chain)   
    i <- 1 # hack to prevent warning on package check
    mcmc <- foreach::foreach(i = 1:n.chain, .combine = add_jags_jags_mcmc) %dopar% {
      fun2(
        jags = jags[[i]], monitor = monitor, n.sim = n.sim, n.thin = n.thin, 
        quiet = quiet, recompile = T
      )
    }
  } else {    
    mcmc <- fun2 (
      jags = jags[[1]], monitor = monitor, n.sim = n.sim, n.thin = n.thin, 
      quiet = quiet, recompile = F
    )
  }
  object$mcmc <- mcmc
  object$iterations <- object$iterations * 2
  object$time <- object$time + ((proc.time () - ptm)[3]) / (60 * 60)
  
  return (object)
}

#' @method update_jags jags_analysis
#' @export 
update_jags.jags_analysis <- function (object, quiet = F, ...)
{
  
}

#' @method update_jags jags_simulation
#' @export 
update_jags.jags_simulation <- function (object, nrep = 1, values = NULL, mode = "current", ...)
{
  if(!is.jags_simulation(object))
    stop("object should be of class jags_simulation")
  
  if(!is.null(values)) {
    if(!is.data.frame(values))
      stop ("values must be NULL or a data frame")
    
    if(nrow(values) == 0)
      stop ("values must have at least one row of data")
    
    if(ncol(values) == 0)
      stop ("values must have at least one column of data")
  }
  
  if(!is.numeric(nrep))
    stop("nrep must be class integer")
  
  if(!length(nrep) == 1)
    stop("nrep must be a single value")
  
  if(nrep < 0)
    stop("nrep must not be positive")
  
  nrep <- as.integer(nrep)
  
  if(!"basemod" %in% list.modules())
    load.module("basemod")  
  
  if(!"bugs" %in% list.modules())
    load.module("bugs")
  
  old_opts <- opts_jagr(mode = mode)
  on.exit(opts_jagr(old_opts))
  
  if(!is.null(values)) {
    
    if(any(!colnames(object$values) %in% colnames(values)))
      stop("colnames missing from values")
    
    if(any(!colnames(values) %in% colnames(object$values)))
      warning("unrecognised colnames in values")  
    
    values <- subset(values, select = colnames(object$values))
    
    newObject <- jags_simulation(data_model = object$data_model, 
                                 nrep = object$nrep + nrep, 
                                 values = values, 
                                 mode = "current")
  }
  
  if (nrep > 0) {
    nvalues <- nrow(object$values)
    for (value in 1:nvalues) {
      for (rep in (object$nrep + 1):(object$nrep + nrep)) {
        if (!opts_jagr("quiet"))
          print(paste0("Value: ",value," of ",nvalues,"  Rep: ", rep," of ",(object$nrep + nrep)))
        
        x <- data_jags(object$data_model,
                       value = object$values[value,,drop = FALSE])
        
        object$simulated[[value]][[rep]] <- data_jags(object$data_model,
                                                      value = object$values[value,,drop = FALSE])
        
      }
    }
    object$nrep <- object$nrep + nrep
  }
  
  #  object <- object + newObject
  
  return (object)
}

#' @method update_jags jags_power_analysis
#' @export 
update_jags.jags_power_analysis <- function (object, quiet = F, ...)
{
  
}
