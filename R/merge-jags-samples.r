#' @title Merge JAGS samples
#'
#' @description
#' Merges JAGS samples in multiple jags_sample objects by by using function fun
#' 
#' @param object a list of jags_sample objects.
#' @param by the variables to combine by (using merge).
#' @param fun the function to using when combining samples (by default fun = sum). 
#' @return a jags_sample object
#' @seealso \code{\link{predict.jags_analysis}} and 
#' \code{\link{ddply_jags_sample}}
#' @export
merge_jags_samples <- function (object, by = NULL, fun = sum) {
  
  if(!is_list_jags_sample(object))
    stop("object must be a list of jags_sample objects")
  
  if(!(is_null(by) || is_character_vector(by)))
    stop("by must be NULL or a character vector")
  
  if(length(object) == 1)
    return (object[[1]])
    
  colnames <- colnames(object[[1]])
  for (i in 2:length(object))
    colnames <- colnames[colnames %in% colnames(object[[i]])]
  
  if (is.null(by)) {
    by <- colnames[-grep("[[:digit:]]", colnames)]
  } else {
    bby <- by[by %in% colnames[-grep("[[:digit:]]", colnames)]]
    if (length(bby) != length(by))
      warning(paste("the following variables are in by but not all the jags_sample",
              "objects:", by[!by %in% bby], collapse = " "))
  }
  
  if(length(by) == 0)
    stop("jags_samples have no column names in common")
  
  colnames <- colnames[grep("[[:digit:]]", colnames)]
  
  merge <- object[[1]][,colnames(object[[1]]) %in% by, drop=FALSE]
  
  for (i in 2:length(object))
    merge <- merge(merge, object[[i]][,colnames(object[[i]]) %in% by, drop=FALSE])
    
  for (i in 1:length(object))
    object[[i]] <- merge(merge, object[[i]])
  
  array <- as.matrix((object[[1]][,colnames]))
  for (i in 2:length(object)) {
    mat <- as.matrix((object[[i]][,colnames]))
    array <- abind::abind(array, mat, along = 3)
  }  
  samples <- apply(array, MARGIN=c(1,2), fun)  
    
  if (!is.null(by)) {
    data <- object[[1]][,colnames(object[[1]]) %in% by, drop=FALSE]
  }
  object <- cbind(data, samples)
  class(object) <- c("data.frame","jags_sample")
  return (object)
}
