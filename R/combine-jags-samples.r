
#' @title Combine JAGS samples
#'
#' @description
#' Combines JAGS samples in multiple jags_sample objects by by using function fun
#' 
#' @param object a list of jags_sample objects.
#' @param by the variables to combine by (using merge).
#' @param fun the function to using when combining samples (by default fun = sum). 
#' @return a jags_sample object
#' @seealso \code{\link{predict.jags_analysis}}
combine_jags_samples <- function (object, by = NULL, fun = sum) {
  
  if(!is.list(object))
    stop("object must be a list of jags_sample objects")
  
  if(!is.null(by) && !is.character(by))
    stop("by must be NULL or a character vector")
  
  if(length(object) == 0)
    stop("object must contain at least one element")
  
  if(length(by) == 0)
    stop("by must be NULL or be a character vector of length one or greater")
  
  if(length(object) == 1) {
    object <- object[[1]]
    if (!is.null(by)) {
      data <- subset(object, select = by)
    } else {
      data <- object[,-grep("V[[:digit:]]", colnames(object))]
    }
    samples <- object[,grep("V[[:digit:]]", colnames(object))]
    object <- cbind(data,samples)
    class(object) <- c("data.frame","jags_sample")
    return (object)
  }
  
  colnames <- lapply(object, colnames)
  
  colnames <- colnames(object[[1]])
  for (i in 2:length(object)) {
    colnames <- colnames[colnames %in% colnames(object[[i]])]
  }
  
  if (is.null(by)) {
    by <- colnames[-grep("V[[:digit:]]", colnames)]
  } else {
    bby <- by[by %in% colnames[-grep("V[[:digit:]]", colnames)]]
    if (length (bby) != length(by))
      warning(paste0("the following variables are in by but all the jags_sample objects:", by[!by %in% bby]))
  }
  
  if(length(by) == 0)
    stop("jags_samples have no column names in common")
  
  colnames <- colnames[grep("V[[:digit:]]", colnames)]
  
  merge <- subset(object[[1]], select = by)
  
  for (i in 2:length(object)) {
    merge <- merge(merge,subset(object[[i]],select = by))
  }
  
  for (i in 1:length(object)) {
    object[[i]] <- merge(merge,object[[i]])
  }  
  
  array <- as.matrix((object[[1]][,colnames]))
  for (i in 2:length(object)) {
    mat <- as.matrix((object[[i]][,colnames]))
    array <- abind::abind(array, mat, along = 3)
  }  
  samples <- apply(array, MARGIN=c(1,2), fun)  
    
  if (!is.null(by)) {
    data <- subset(object[[1]],select = by)
  }
  object <- cbind(data,samples)
  class(object) <- c("data.frame","jags_sample")
  return (object)
}
