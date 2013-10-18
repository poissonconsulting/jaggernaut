
#' @export
update_jags <- function (object, ...) {
  UseMethod("update_jags", object)
}

update_jags.jagr_power_analysis <- function (object, ...) {  
  
  fun2 <- function (jags, monitor, n.sim, n.thin, recompile) {
    n.sim <- as.integer(n.sim)
    n.thin <- as.integer(n.thin)

    if (recompile)
      jags$recompile()
    
    samples <- jags.samples(
      model = jags, variable.names = monitor, n.iter = n.sim, thin = n.thin
    )
    object <- list()
    class(object) <- "jagr_chains"
    samples(object) <- samples
    jags(object) <- list(jags)
    return (object)
  }
  
  if(!"basemod" %in% list.modules())
    load.module("basemod")  
  
  if(!"bugs" %in% list.modules())
    load.module("bugs")
  
  if(!"dic" %in% list.modules())
    load.module("dic")
  
  n.chain <- nchains(object)
  n.sim <- niters(object)
  n.thin <- max(1, floor(n.chain * n.sim / nsims(object)))
  
  monitor <- monitor(object)  
  jags <- jags(chains(object))
  
  parallel <- length(jags) > 1
  
  ptm <- proc.time()
  
  if (parallel) {
    
    doMC::registerDoMC(cores=n.chain)   
    i <- 1 # hack to prevent warning on package check
    chains <- foreach::foreach(i = 1:n.chain, .combine = add_jags_jagr_chains) %dopar% {
      fun2(
        jags = jags[[i]], monitor = monitor, n.sim = n.sim, n.thin = n.thin, 
        recompile = T
      )
    }
  } else {    
    chains <- fun2 (
      jags = jags[[1]], monitor = monitor, n.sim = n.sim, n.thin = n.thin, 
      recompile = F
    )
  }
  chains(object) <- chains
  niters(object) <- niters(object) * 2
  time_interval(object) <- object$time + ((proc.time () - ptm)[3]) / (60 * 60)
  
  return (object)
}

#' @method update_jags jags_analysis
#' @export 
update_jags.jags_analysis <- function (object, mode = "current", ...) {
  old_opts <- opts_jagr(mode = mode)
  on.exit(opts_jagr(old_opts))
  
  rhat_threshold <- opts_jagr("rhat")
  quiet <- opts_jagr("quiet")
  parallelModels <- opts_jagr("parallel_models")
  
  if (quiet) {
    options(jags.pb = "none")
  } else {
    options(jags.pb = "text")
  }   
    
  nmodel <- nmodels(object)
  
  if(nmodel == 1) {
    parallelModels <- FALSE
  }
  
  analyses <- list()
  if(parallelModels) {
    
    doMC::registerDoMC(cores=nmodel)
    
    analyses <- foreach::foreach(i = 1:nmodel) %dopar% { 
      update_jags(object$analyses[[i]],...)
    }
    if(!quiet) {
      for (i in 1:nmodel) {
        
        cat(paste("\n\nModel",i,"of",nmodel,"\n\n"))
        
        if (is_converged (analyses[[i]], rhat_threshold = rhat_threshold)) {
          cat ("Analysis converged")
        } else 
          cat ("Analysis failed to converge")
        cat_convergence (analyses[[i]])
      }
    }
  } else {
    for (i in 1:nmodel) {
      if (!quiet)
        cat(paste("\n\nModel",i,"of",nmodel,"\n\n"))
      analyses[[i]] <- update_jags(object$analyses[[i]], ...)
      
      if(!quiet) {
        if (is_converged (analyses[[i]], rhat_threshold = rhat_threshold)) {
          cat ("Analysis converged")
        } else 
          cat ("Analysis failed to converge")
        cat_convergence (analyses[[i]])
      }
    }
  }
  
  analyses(object) <- analyses
  rhat_threshold(object) <- rhat_threshold
  
  object <- revise(object)
  
  return (object)
}

#' @method update_jags jags_simulation
#' @export 
update_jags.jags_simulation <- function (object, nreps, values = NULL, mode = "current", ...) {
    
  if(!is.numeric(nreps))
    stop("nreps must be class integer")
  
  if(!length(nreps) == 1)
    stop("nreps must be a single value")
  
  if(nreps < 0)
    stop("nreps must not be negative")

  old_opts <- opts_jagr(mode = mode)
  on.exit(opts_jagr(old_opts))
  
  quiet <- opts_jagr("quiet")
  
  nreps <- as.integer(nreps)
  
  old_nvalues <- nvalues(object)
  old_nreps <- nreps(object)
  new_nreps <- old_nreps + nreps
  
  data <- data_jags(object)
  
  values(object) <- rbind(values(object), values)
  
  if (!is.null(values)) {      
    for (value in (old_nvalues + 1):nvalues(object)) {
      data[[value]] <- list()
    
      for (rep in 1:old_nreps) {
        if (!quiet)
          cat(paste0("value: ",value," of ",nvalues(object),"  replicate: ", rep," of ",new_nreps,"\n"))
        
          data[[value]][[rep]] <- data_jags(data_model(object), values(object)[value,,drop = FALSE])
      }
    }
  }
  
  if (nreps > 0) {      
    for (value in 1:nvalues(object)) {
      for (rep in (old_nreps + 1):(new_nreps)) {
        if (!quiet)
          cat(paste0("value: ",value," of ",nvalues(object),"  replicate: ", rep," of ",new_nreps,"\n"))
        
        data[[value]][[rep]] <- data_jags(data_model(object), values(object)[value,,drop = FALSE])
      }
    }
  }
  
  
  data_jags(object) <- data
  
  return (object)
}

#' @method update_jags jags_power_analysis
#' @export 
update_jags.jags_power_analysis <- function (object, nreps = 0, values = NULL, mode = "current", ...) {
  
  old_opts <- opts_jagr(mode = mode)
  on.exit(opts_jagr(old_opts))
  
  quiet <- opts_jagr("quiet")
  rhat_threshold <- opts_jagr("rhat")
  
  if(opts_jagr("mode") == "debug")
    nreps <- min(2,nreps)
  
  old_nvalues <- nvalues(object)
  old_nreps <- nreps(object)
  
  if(nreps > 0 || !is.null(values)) {
    
    if(!quiet)
      cat("\nupdating data\n")
    
    class(object) <- c("jags_simulation")
    
    object <- update_jags(object, nreps = nreps, values = values, ...)
    
    class(object) <- c("jags_power_analysis","jags_simulation")
  }
    
  if (!quiet)
    cat("\nanalysing data\n")
  
  niters <- min(niters(object))
  
  analyses <- analyses(object)
  
  for (value in 1:old_nvalues) {
    for (rep in 1:old_nreps) {
      if(!quiet)
        cat(paste0("value: ",value," of ",nvalues(object),"  replicate: ", rep," of ",nreps(object),"\n"))
      if (!is_converged(analyses[[value]][[rep]], rhat_threshold = rhat_threshold))
        analyses[[value]][[rep]] <- update_jags(analyses[[value]][[rep]])
    }
  }
    
  data <- data_jags(object)
  
  if (!is.null(values)) {
    for (value in (old_nvalues + 1):nvalues(object)) {
      print(value)
      print(length(data))
      analyses[[value]] <- list()
      for (rep in 1:old_nreps) {
        if(!quiet)
          cat(paste0("value: ",value," of ",nvalues(object),"  replicate: ", rep," of ",nreps(object),"\n"))

        analysis <- jags_analysis(model = model(object), 
                                  data = data[[value]][[rep]], 
                                  niters = niters)  
        
        analyses[[value]][[rep]] <- as.jagr_power_analysis(analysis(analysis))        
      }
    }
  }
    
  if (nreps > 0) {
    for (value in 1:nvalues(object)) {
      for (rep in (old_nreps+1):nreps(object)) {
        if(!quiet)
          cat(paste0("value: ",value," of ",nvalues(object),"  replicate: ", rep," of ",nreps(object),"\n"))
        
        analysis <- jags_analysis(model = model(object), 
                                  data = data[[value]][[rep]], 
                                  niters = niters)  
        
        analyses[[value]][[rep]] <- as.jagr_power_analysis(analysis(analysis))        
      }
    }
  }  
  
  analyses(object) <- analyses
  rhat_threshold(object) <- rhat_threshold

  return (object)  
}
