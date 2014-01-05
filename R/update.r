update_jags <- function (jags, monitor, n.sim, n.thin, recompile) {
  
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
    capture.output(samples <- jags.samples(
      model = jags, variable.names = monitor, n.iter = n.sim, thin = n.thin
    ))
  } else {
    samples <- jags.samples(
      model = jags, variable.names = monitor, n.iter = n.sim, thin = n.thin
    )      
  }
  object <- list()
  class(object) <- "jagr_chains"
  samples(object) <- samples
  jags(object) <- list(jags)
  return (object)
}

update.jagr_chains <- function (object, niters, nworkers, ...) {
  
  nchains <- nchains(object)
  n.thin <- max(1, floor(nchains * niters / nsims(object)))
  
  monitor <- monitor(object)

  jags <- jags(object)
  
  if(nworkers == 1) {
    chains <- update_jags (jags = jags[[1]], monitor = monitor, 
                           n.sim = niters, 
                           n.thin = n.thin, 
                           recompile = FALSE)
  } else {
    i <- NULL
    chains <- foreach(i = isplitIndices(n = nchains, chunks = nworkers),
                      .combine = combine_jagr_chains, 
                      .export = "update_jags") %dopar% {
                        update_jags(jags = jags[[i]], monitor = monitor, 
                                    n.sim = niters, 
                                    n.thin = n.thin, 
                                    recompile = TRUE)
                      } 
  }
  
  random(chains) <- random(object)
  return (chains)
}

update.jagr_power_analysis <- function (object, nworkers, ...) {  
    
  niters <- niters(object)
  ptm <- proc.time()
  
  chains(object) <- update(chains(object), niters = niters, nworkers = nworkers)
  niters(object) <- niters * 2
  time_interval(object) <- object$time + ((proc.time () - ptm)[3]) / (60 * 60)
  
  return (object)
}

update_jagr_power_analysis <- function (object, nworkers, ...) {
  stopifnot(is.jagr_power_analysis(object))
  return (update(object, nworkers = nworkers, ...))
}

#' @method update jags_analysis
#' @export 
update.jags_analysis <- function (object, mode = "current", ...) {
  
  if (mode != "current") {
    old_opts <- opts_jagr(mode = mode)
    on.exit(opts_jagr(old_opts))
  }
    
  if (options()$jags.pb != "none") {
    jags.pb <- options()$jags.pb
    options(jags.pb = "none")
    on.exit(options("jags.pb" = jags.pb), add = TRUE)
  }
  
  check_modules()
  
  nworkers <- getDoParWorkers()
  
  nchains <- nchains(object)[[1]]
  nmodels <- nmodels(object)
  
  quiet <- opts_jagr("quiet")
  
  rhat_threshold <- opts_jagr("rhat")
  
  chunks <- floor(nworkers / nchains)
  chunks <- min(nmodels, chunks)
  if (chunks <= 1) {
    analyses <- lapply(analyses(object), update_jagr_power_analysis, 
                      nworkers = nworkers)
  } else { 
    i <- NULL
    
    fun <- function (x1, x2) {
      n1 <- length(x1)
      n2 <- length(x2)
      x <- list()
      for (i in 1:n1)
        x[[i]] <- x1[[i]]
      for (i in 1:n2)
        x[[i + n1]] <- x2[[i]]
      return (x)
    }
    
    analyses <- foreach(i = isplitIndices(n = nmodels, 
                                          chunks = chunks),
                        .combine = fun, 
                        .export = "update_jagr_power_analysis") %dopar% {
                          update_jagr_power_analysis(analyses[i], 
                                                     nworkers = nchains)
                        }
  }
  
  if(!quiet) {
    for (i in 1:nmodels) {
      cat(paste("\n\nModel",i,"of",nmodels,"\n\n"))
      
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
    
    simulation <- jags_simulation(data_model, values, nreps = nreps(object))

    if(!quiet)
      cat("\nValues Updated\n")

    object <- combine(object, simulation)
  }
    
  if (nreps > 0) {
    if(!quiet)
      cat("\nUpdating Replicates...\n")   
    
    simulation <- jags_simulation(data_model, values = values(object), nreps = nreps)

    if(!quiet)
      cat("\nReplicates Updated\n")   

    dataset(object) <- clist(dataset(object), dataset(simulation), 
                               recursive = 2)
  }

  return (object)
}

#' @method update jags_power_analysis
#' @export 
update.jags_power_analysis <- function (object, nreps = 0, values = NULL, mode = "current", ...) {
    
  if (mode != "current") {
    old_opts <- opts_jagr(mode = mode)
    on.exit(opts_jagr(old_opts))
  }
  
  check_modules()
  
  nworkers <- getDoParWorkers()
  
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
    
    object <- combine(object, power)
  }
  
  if (nreps > 0) {
    if(!quiet)
      cat("\nUpdating Replicates...\n")   
    
    power <- jags_power_analysis(model, data_model, values(object), 
                                    nreps = nreps, niters = niters)
    
    if(!quiet)
      cat("\nReplicates Updated\n")   

    dataset(object) <- clist(dataset(object), dataset(power), 
                               recursive = 2)
    analyses(object) <- clist(analyses(object), analyses(power), 
                              recursive = 2)
  }

  
  if(nreps == 0 && is.null(values)) {
      
    fun <- function (object, rhat_threshold) {
      stopifnot(is.jagr_power_analysis(object))
      
      if (!is_converged(object, rhat_threshold = rhat_threshold))
        object <- update(object, nworkers = nworkers)
  
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
