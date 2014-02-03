#' @title Combines objects
#'
#' @description
#' Adds two or more JAGS object of the same class.  
#' 
#' @param object a JAGS object.
#' @param ... additional JAGS objects to add to object.
#' @return a JAGS object of the original class
#' @export
combine <- function (object, ...) {
  UseMethod("combine", object)
}

combine.mcarray <- function (object, ..., by = "sims") {
  
  args <- list(...)
  
  if (length(args) == 0)
    return (object)
  
  object2 <- args[[1]]
  args <- args[-1]
  
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
      stop ("objects should be class mcarray")
    if (nsims (object) / nchains(object) != nsims(object2) / nchains(object2))
      stop ("objects should have the same number of sims")
    
    dimobj <- dim (object)
    dimobject2 <- dim (object2)
    dnames <- names(dim (object))
    
    if (!identical(dimobj[-length(dimobj)],dimobject2[-length(dimobject2)]))
      stop ("objects should have the same dimensions (except chains)")
    
    class(object)<-"array"
    class(object2)<-"array"
    object <- abind (object,object2,along=length(dimobj))
    
    names(dim(object)) <- dnames
    class(object)<-"mcarray"
  } else if(by == "sims") {
    
    if (!inherits (object, "mcarray"))
      stop ("objects should be class mcarray")
    if (!inherits (object2, "mcarray"))
      stop ("objects should be class mcarray")
    if (nchains (object) != nchains (object2))
      stop ("objects should have the same number of chains")
    
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
  
  nargs <- length(args)
  if (nargs > 0) {
    for (i in 1:nargs) {
      object[[i]] <- combine(object, args[[i]], by = by)
    }
  }
  return (object)
}

combine.list <- function (object, ..., by = "sims") {
  
  args <- list(...)
  
  if (length(args) == 0)
    return (object)
  
  object2 <- args[[1]]
  args <- args[-1] 
  
  for (i in seq(along = object))
    object[[i]] <- combine(object[[i]], object2[[i]], by = by)
  
  nargs <- length(args)
  if (nargs > 0) {
    for (i in 1:nargs) {
      object[[i]] <- combine(object, args[[i]], by = by)
    }
  }
  return (object)
}

combine_lists_by_sims <- function (object, ...) {
  assert_that(is.list(object))
  return (combine(object, ..., by = "sims"))
}

combine.jagr_chains <- function (object, ...) {
  
  args <- list(...)
  
  if (length(args) == 0)
    return (object)
  
  object2 <- args[[1]]
  args <- args[-1]
  
  jags(object) <- c(jags(object), jags(object2))  
  samples(object) <- combine (samples(object), samples(object2), by = "chains")
  
  nargs <- length(args)
  if (nargs > 0) {
    for (i in 1:nargs) {
      object <- combine(object, args[[i]], by = "chains")
    }
  }
  return (object)
}

combine_jagr_chains <- function (object, ...) {
  stopifnot(is.jagr_chains(object))
  return (combine(object, ...))
}

#' @title Add JAGS model objects
#'
#' @description
#' Adds two or more \code{jags_model} objects.  
#' 
#' @param object a \code{jags_model} object.
#' @param ... additional \code{jags_model} objects to add to \code{object}.
#' @return a \code{jags_model} object with multiple models
#' @seealso \code{\link{combine}}, \code{\link{jags_model}}
#' and \code{\link{jaggernaut}}.
#' @method combine jags_model
#' @export 
combine.jags_model <- function (object, ...) {
  args <- list(...)
  
  if (length(args) == 0)
    return (object)
  
  object2 <- args[[1]]
  args <- args[-1]
  
  object$models <- c(object$models,object2$models) 
  
  nargs <- length(args)
  if (nargs > 0) {
    for (i in 1:nargs) {
      object <- combine(object, args[[i]])
    }
  }
  return (object)
}

#' @title Add JAGS simulation objects
#'
#' @description
#' Adds two or more \code{jags_simulation} objects. If the objects have
#' different numbers of replicates then extra replicates are generated until all the
#' objects have the same number.   
#' 
#' @param object a \code{jags_simulation} object.
#' @param ... additional \code{jags_simulation} objects to add to \code{object}.
#' @param mode a character element specifying the mode to use if generating 
#' replicates. See \code{opts_jagr} for further information 
#' @return a \code{jags_simulation} object
#' @seealso \code{\link{combine}}, \code{\link{jags_simulation}},
#' \code{\link{opts_jagr}} and \code{\link{jaggernaut}}.
#' @method combine jags_simulation
#' @export 
combine.jags_simulation <- function (object, ..., mode = "current") {
  
  args <- list(...)
  
  if (length(args) == 0)
    return (object)
  
  object2 <- args[[1]]
  args <- args[-1]
  
  if(!is.jags_simulation(object2))
    stop("objects should be of class jags_simulation")
  
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
  
  dataset(object)  <- clist(dataset(object), dataset(object2))
  
  nargs <- length(args)
  if (nargs > 0) {
    for (i in 1:nargs) {
      object <- combine(object, args[[i]])
    }
  }
  return (object)
}

#' @title Add JAGS power analysis objects
#'
#' @description
#' Adds two or more \code{jags_power_analysis} objects with the same 
#' \code{jags_data_model} object. If the objects have
#' different numbers of replicates then extra replicates are generated until all the
#' objects have the same number.   
#' 
#' @param object a \code{jags_power_analysis} object.
#' @param ... additional \code{jags_power_analysis} objects to add to \code{object}.
#' @param mode a character element specifying the mode to use if generating 
#' replicates. See \code{opts_jagr} for further information 
#' @return a \code{jags_power_analysis} object
#' @seealso \code{\link{combine}}, \code{\link{jags_power_analysis}},
#' \code{\link{jags_data_model}},
#' \code{\link{opts_jagr}} and \code{\link{jaggernaut}}.
#' @method combine jags_power_analysis
#' @export 
combine.jags_power_analysis <- function (object, ..., mode = "current") {
  
  args <- list(...)
  
  if (length(args) == 0)
    return (object)
  
  object2 <- args[[1]]
  args <- args[-1]
  
  if(!is.jags_simulation(object2))
    stop("objects should be of class jags_power_analysis")
  
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
  
  dataset(object) <- clist(dataset(object), dataset(object2))
  
  analyses(object) <- clist(analyses(object), analyses(object2))
  
  rhat_threshold(object) <- min(rhat_threshold(object), rhat_threshold(object2))
  
  nargs <- length(args)
  if (nargs > 0) {
    for (i in 1:nargs) {
      object[[i]] <- combine(object, args[[i]])
    }
  }
  return (object)
}

#' @title Combine JAGS samples
#'
#' @description
#' Combine JAGS samples in multiple jags_sample objects by by using function fun
#' 
#' @param object a \code{jags_sample} object.
#' @param ... additional \code{jags_sample} objects to add to \code{object}.
#' @param by the variables to combine by (using merge).
#' @param fun the function to using when combining samples (by default fun = sum). 
#' @return a jags_sample object
#' #' @seealso \code{\link{combine}}, \code{\link{predict.jags_analysis}} and 
#' \code{\link{ddply_jags_sample}}
#' @method combine jags_sample
#' @export 
combine.jags_sample <- function (object, ..., by = NULL, fun = sum) {
  
  assert_that(is.jags_sample(object))
  assert_that(is_null(by) || is_character_vector(by))
  assert_that(is.function(sum))
  
  args <- list(...)
  
  if (length(args) == 0)
    return (object)

  assert_that(is_list_jags_sample(args))
  
  args <- c(list(object), args)
  
  nargs <- length(args)
    
  colnames <- colnames(args[[1]])
  for (i in 2:nargs)
    colnames <- colnames[colnames %in% colnames(args[[i]])]
  
  if (is.null(by)) {
    by <- colnames[grep("^[[:digit:]]", colnames, invert = TRUE)]
  } else {
    bby <- by[by %in% colnames[grep("^[[:digit:]]", colnames, invert = TRUE)]]
    if (length(bby) != length(by))
      warning("the following variables are in by but not all the jags_sample",
              " objects: ", paste(by[!by %in% bby], collapse = ", "))
  }
  
  if(length(by) == 0)
    stop("jags_samples have no column names in common")
  
  colnames <- colnames[grep("^[[:digit:]]", colnames)]
  
  merge <- args[[1]][,colnames(args[[1]]) %in% by, drop=FALSE]
  
  for (i in 2:nargs)
    merge <- merge(merge, args[[i]][,colnames(args[[i]]) %in% by, drop=FALSE])
  
  for (i in 1:nargs)
    args[[i]] <- merge(merge, args[[i]])
  
  array <- as.matrix((args[[1]][,colnames]))
  for (i in 2:nargs) {
    mat <- as.matrix((args[[i]][,colnames]))
    array <- abind(array, mat, along = 3)
  }  
  samples <- apply(array, MARGIN=c(1,2), fun)  
  
  if (!is.null(by)) {
    data <- args[[1]][,colnames(args[[1]]) %in% by, drop=FALSE]
  }
  object <- cbind(data, samples)
  class(object) <- c("data.frame","jags_sample")
  return (object)
}
