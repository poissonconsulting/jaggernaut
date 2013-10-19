
#' @export
update_jags <- function (object, ...) {
  UseMethod("update_jags", object)
}

update_jags.jagr_power_analysis <- function (object, ...) {  
  
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

update_jags_jagr_power_analysis <- function (object, ...) {
  stopifnot(is.jagr_power_analysis(object))
  return (update_jags(object, ...))
}

#' @method update_jags jags_analysis
#' @export 
update_jags.jags_analysis <- function (object, mode = "current", ...) {

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
  
  analyses <- llply_jg(analyses(object), update_jags_jagr_power_analysis)
  
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

#' @method update_jags jags_simulation
#' @export 
update_jags.jags_simulation <- function (object, nreps, values = NULL, mode = "current", ...) {
    
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
  
  if (mode != "current") {
    old_opts <- opts_jagr(mode = mode)
    on.exit(opts_jagr(old_opts))
  }
  
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
