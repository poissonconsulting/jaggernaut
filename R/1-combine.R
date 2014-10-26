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

combine.mcarray <- function (object, ..., by = "samples") {
  
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
  
  if(!by %in% c("samples","chains"))
    stop("by must be 'samples' or 'chains'")
  
  if(by == "chains") {
    
    if (!inherits (object2, "mcarray"))
      stop ("objects should be class mcarray")
    if (nsamples (object) / nchains(object) != nsamples(object2) / nchains(object2))
      stop ("objects should have the same number of samples")
    
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
  } else if(by == "samples") {
    
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
      stop ("object and object2 should have the same dimensions (except samples)")
    
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

#' @title Combines elements of list
#'
#' @description
#' Combines elements of a list by calling the specific combine function.
#' 
#' @param object a list of objects of the same class to combine.
#' @param ... additional arguments to pass to the specific combine function.
#' @return The result of calling specific combine function.
#' @seealso \code{\link{combine}}
#' @export 
combine_list <- function (object, ...) {
  assert_that(is.list(object))
  
  cmd <- paste0("object[[", 1:length(object), "]]")
  cmd <- paste0(cmd, collapse = ", ")
  
  cmd <- paste0("combine(",cmd,", ...)")
  
  eval(parse(text = cmd))
}

combine.list <- function (object, ..., by = "samples") {
  
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

combine_lists_by_samples <- function (object, ...) {
  assert_that(is.list(object))
  return (combine(object, ..., by = "samples"))
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
