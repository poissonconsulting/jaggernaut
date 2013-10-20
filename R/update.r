
update.jagr_power_analysis <- function (object, ...) {  
  
  update_jg <- function (jags, monitor, n.sim, n.thin, recompile) {
    
    stopifnot(is.jags(jags))
    
    n.sim <- as.integer(n.sim)
    n.thin <- as.integer(n.thin)
    
    quiet <- opts_jagr("quiet")
    
    if (recompile) {
      if (opts_jagr("mode") != "debug") {
        capture.output(jags$recompile())
      } else
        jags$recompile()
    }
    if (opts_jagr("mode") != "debug") {
      capture.output(samples <- rjags::jags.samples(
        model = jags, variable.names = monitor, n.iter = n.sim, thin = n.thin
      ))
    } else {
      samples <- rjags::jags.samples(
        model = jags, variable.names = monitor, n.iter = n.sim, thin = n.thin
      )      
    }
    object <- list()
    class(object) <- "jagr_chains"
    samples(object) <- samples
    jags(object) <- list(jags)
    return (object)
  }
  
  n.chain <- nchains(object)
  n.sim <- niters(object)
  n.thin <- max(1, floor(n.chain * n.sim / nsims(object)))
  
  monitor <- monitor(object)  
  jags <- jags(chains(object))
    
  ptm <- proc.time()
  
  if (length(jags) > 1) {
    chains_list <- llply_jg(.data = jags, .fun = update_jg, 
                            monitor = monitor, n.sim = n.sim, n.thin = n.thin, 
                            recompile = TRUE)
    
    chains <- chains_list[[1]]
    for (i in 2:length(chains_list)) {
      chains <- add_jags(chains, chains_list[[i]])
    } 
  } else {
    chains <- update_jg (jags = jags[[1]], monitor = monitor, n.sim = n.sim, 
                         n.thin = n.thin, quiet = quiet, recompile = FALSE)
  }
  
  chains(object) <- chains
  niters(object) <- niters(object) * 2
  time_interval(object) <- object$time + ((proc.time () - ptm)[3]) / (60 * 60)
  
  return (object)
}

update_jagr_power_analysis <- function (object, ...) {
  stopifnot(is.jagr_power_analysis(object))
  return (update(object, ...))
}

#' @method update jags_analysis
#' @export 
update.jags_analysis <- function (object, mode = "current", ...) {

  check_modules()
  
  if (mode != "current") {
    old_opts <- opts_jagr(mode = mode)
    on.exit(opts_jagr(old_opts))
  }
  
  quiet <- opts_jagr("quiet")
  
  if (quiet && options()$jags.pb != "none") {
    jags.pb <- options()$jags.pb
    options(jags.pb = "none")
    on.exit(options("jags.pb" = jags.pb), add = TRUE)
  }
  
  rhat_threshold <- opts_jagr("rhat")
  
  analyses <- llply_jg(analyses(object), update_jagr_power_analysis)
  
  if(!quiet) {
    for (i in 1:nmodels(object)) {
      cat(paste("\n\nModel",i,"of",nmodels(object),"\n\n"))
      
      if (is_converged (analyses[[i]], rhat_threshold = rhat_threshold)) {
        cat ("Analysis converged")
      } else 
        cat ("Analysis failed to converge")
      cat_convergence (analyses[[i]])
    }
  }
  
  analyses(object) <- analyses
  rhat_threshold(object) <- rhat_threshold
    
  return (object)
}

#' @method update jags_simulation
#' @export 
update.jags_simulation <- function (object, nreps, values = NULL, mode = "current", ...) {
  check_modules()
  
  nreps <- as.integer(nreps)
  
  if(!is.numeric(nreps))
    stop("nreps must be class integer")
  
  if(!length(nreps) == 1)
    stop("nreps must be a single value")
  
  if(nreps < 0)
    stop("nreps must not be negative")

  if (mode != "current") {
    old_opts <- opts_jagr(mode = mode)
    on.exit(opts_jagr(old_opts))
  }
  
  quiet <- opts_jagr("quiet")
      
  data_model <- data_model(object)
  
  if (!is.null(values)) {
    if(!quiet)
      cat("\nUpdating Values...\n")
    
    simulation <- jags_simulation(data_model, values, nrep = nreps(object))

    if(!quiet)
      cat("\nValues Updated\n")

    object <- add_jags(object, simulation)
  }
    
  if (nreps > 0) {
    if(!quiet)
      cat("\nUpdating Replicates...\n")   
    
    simulation <- jags_simulation(data_model, values = values(object), nreps = nreps)

    if(!quiet)
      cat("\nReplicates Updated\n")   

    data_jags(object) <- clist(data_jags(object), data_jags(simulation), 
                               recursive = 2)
  }

  return (object)
}

#' @method update jags_power_analysis
#' @export 
update.jags_power_analysis <- function (object, nreps = 0, values = NULL, mode = "current", ...) {
  
  check_modules()
  
  if (mode != "current") {
    old_opts <- opts_jagr(mode = mode)
    on.exit(opts_jagr(old_opts))
  }
  
  quiet <- opts_jagr("quiet")
  rhat_threshold <- opts_jagr("rhat")
  
  if(opts_jagr("mode") == "debug")
    nreps <- min(2,nreps)
  
  data_model <- data_model(object)
  model <- model(object)
  niters <- min(niters(object))
  
  if (!is.null(values)) {
    if(!quiet)
      cat("\nUpdating Values...\n")
      
    power <- jags_power_analysis(model, data_model, values, 
                                      nreps = nreps(object), niters = niters)
    
    if(!quiet)
      cat("\nValues Updated\n")
    
    object <- add_jags(object, power)
  }
  
  if (nreps > 0) {
    if(!quiet)
      cat("\nUpdating Replicates...\n")   
    
    power <- jags_power_analysis(model, data_model, values(object), 
                                    nreps = nreps, niters = niters)
    
    if(!quiet)
      cat("\nReplicates Updated\n")   

    data_jags(object) <- clist(data_jags(object), data_jags(power), 
                               recursive = 2)
    analyses(object) <- clist(analyses(object), analyses(power), 
                              recursive = 2)
  }

  
  if(nreps == 0 && is.null(values)) {
      
    fun <- function (object, rhat_threshold) {
      stopifnot(is.jagr_power_analysis(object))
      
      if (!is_converged(object, rhat_threshold = rhat_threshold))
        object <- update(object)
  
      return (object)
    }
    
    cat("\nUpdating Analyses...\n")   
    
    analyses <- llply_jg(analyses(object), fun, rhat_threshold = rhat_threshold,
                         .recursive = 2)

    cat("\nAnalyses Updated\n")   
    
    analyses(object) <- analyses 
                              
    rhat_threshold(object) <- rhat_threshold
  }
  return (object)  
}
