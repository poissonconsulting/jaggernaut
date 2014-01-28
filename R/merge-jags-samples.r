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
  
  warning("merge_jags_samples is deprecated by combine.jags_sample in v1.6")
  if(length(object) == 1) {
    return(combine(object[[1]], by = by, fun = fun))
  } 
  if(length(object) == 2) {
    return(combine(object[[1]], object[[2]], by = by, fun = fun))
  }
  if(length(object) == 3) {
    return(combine(object[[1]], object[[2]], object[[3]], by = by, fun = fun))
  }
  if(length(object) == 4) {
    return(combine(object[[1]], object[[2]], object[[3]], object[[4]], by = by, fun = fun))
  }
  stop("use combine.jags_sample instead with more than 4 jags_samples")
}
