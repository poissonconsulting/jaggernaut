
#' @title Add JAGS objects
#'
#' @description
#' Adds two or more JAGS object of the same class.  
#' 
#' @param object a JAGS object.
#' @param object2 a second JAGS object to add to object.
#' @param ... additional JAGS objects to add to object.
#' @return a JAGS object of the original class
#' @export
add_jags <- function (object, object2, ...) {
  UseMethod("add_jags", object)
}

add_jags.mcarray <- function (object, object2, ..., by = "sims") {
  
  if(!inherits(by,"character"))
    stop("by must be class character")
  if(length(by) != 1)
    stop("by must be a character element")
  if(is.na(by))
    stop("by must not be a missing value")
  
  if(!by %in% c("sims","chains"))
    stop("by must be 'sims' or 'chains'")
  
  if(by == "chains") {

  if (!inherits (object2, "mcarray"))
    stop ("object2 should be class mcarray")
  if (nsims (object) / nchains(object) != nsims(object2) / nchains(object2))
    stop ("object and object2 should have the same number of sims")
  
  dimobj <- dim (object)
  dimobject2 <- dim (object2)
  dnames <- names(dim (object))
  
  if (!identical(dimobj[-length(dimobj)],dimobject2[-length(dimobject2)]))
    stop ("object and object2 should have the same dimensions (except chains)")
  
  class(object)<-"array"
  class(object2)<-"array"
  object <- abind (object,object2,along=length(dimobj))
  
  names(dim(object)) <- dnames
  class(object)<-"mcarray"
  } else if(by == "sims") {
    
    if (!inherits (object, "mcarray"))
      stop ("object should be class mcarray")
    if (!inherits (object2, "mcarray"))
      stop ("object2 should be class mcarray")
    if (nchains (object) != nchains (object2))
      stop ("object and object2 should have the same number of chains")
    
    dimobj <- dim (object)
    dimiter <- dim (object2)
    dnames <- names(dim (object))
    
    if (!identical(dimobj[-(length(dimobj)-1)],dimiter[-(length(dimiter)-1)]))
      stop ("object and object2 should have the same dimensions (except sims)")
    
    class(object)<-"array"
    class(object2)<-"array"
    object <- abind (object,object2,along=length(dimobj)-1)
    
    names(dim(object)) <- dnames
    class(object)<-"mcarray"
  }
  
  args <- list(...)
  nargs <- length(args)
  if (nargs > 0) {
    for (i in 1:nargs) {
      object[[i]] <- add_jags(object, args[[i]], by = by)
    }
  }
  return (object)
}

add_jags.list <- function (object, object2, ..., by = "sims") {
 
  for (i in seq(along = object))
    object[[i]] <- add_jags(object[[i]], object2[[i]], by = by)
  
  args <- list(...)
  nargs <- length(args)
  if (nargs > 0) {
    for (i in 1:nargs) {
      object[[i]] <- add_jags(object, args[[i]], by = by)
    }
  }
  return (object)
}

add_jags.jagr_chains <- function (object, object2, ...) {
  
  jags(object) <- c(jags(object), jags(object2))  
  samples(object) <- add_jags (samples(object), samples(object2), by = "chains")
    
  args <- list(...)
  nargs <- length(args)
  if (nargs > 0) {
    for (i in 1:nargs) {
      object <- add_jags(object, args[[i]], by = "chains")
    }
  }
  return (object)
}

add_jags_jagr_chains <- function (object, object2) {
  stopifnot(is.jagr_chains(object))
  return (add_jags(object, object2))
}

#' @method add_jags jags_model
#' @export 
add_jags.jags_model <- function (object, object2, ...)
{
  object$models <- c(object$models,object2$models) 
  
  args <- list(...)
  nargs <- length(args)
  if (nargs > 0) {
    for (i in 1:nargs) {
      object <- add_jags(object, args[[i]])
    }
  }
  return (object)
}
    
#' @method add_jags jags_simulation
#' @export 
add_jags.jags_simulation <- function (object, object2, mode = "current", ...) {

  if(!is.jags_simulation(object2))
    stop("object2 should be of class jags_simulation")
  
  if(!identical(data_model(object),data_model(object2)))
    stop("objects must have identical data_models")
  
  if (mode != "current") {
    old_opts <- opts_jagr(mode = mode)
    on.exit(opts_jagr(old_opts))
  }
  
  if (nreps(object) > nreps(object2)) {
    object2 <- update(object2, nreps = nreps(object) - nreps(object2))
  } else if (nreps(object2) > nreps(object))
    object <- update(object, nreps = nreps(object2) - nreps(object))
  
  values(object) <- rbind(values(object), values(object2))

  data_jags(object)  <- clist(data_jags(object), data_jags(object2))
    
  args <- list(...)
  nargs <- length(args)
  if (nargs > 0) {
    for (i in 1:nargs) {
      object <- add_jags(object, args[[i]])
    }
  }
  return (object)
}

#' @method add_jags jags_power_analysis
#' @export 
add_jags.jags_power_analysis <- function (object, object2, mode = "current", ...) {
  if(!is.jags_simulation(object2))
    stop("object2 should be of class jags_power_analysis")
  
  if(!identical(data_model(object),data_model(object2)))
    stop("objects must have identical data_models")

  if(!identical(model(object),model(object2)))
    stop("objects must have identical analysis models")
  
  if (mode != "current") {
    old_opts <- opts_jagr(mode = mode)
    on.exit(opts_jagr(old_opts))
  }
  
  if (nreps(object) > nreps(object2)) {
    object2 <- update(object2, nreps = nreps(object) - nreps(object2))
  } else if (nreps(object2) > nreps(object))
    object <- update(object, nreps = nreps(object2) - nreps(object))
  
  values(object) <- rbind(values(object), values(object2))

  data_jags(object) <- clist(data_jags(object), data_jags(object2))
    
  analyses(object) <- clist(analyses(object), analyses(object2))
  
  rhat_threshold(object) <- min(rhat_threshold(object), rhat_threshold(object2))

  return (object)
}
  