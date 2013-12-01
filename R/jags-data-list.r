
#' @title JAGS data list
#'
#' @description 
#' Creates an object of class \code{jags_data_list}.
#' 
#' @param x a named list.
#' @return A \code{jags_data_list} object.
#' @seealso \code{\link{jags_data}},
#' \code{\link{jags_data_frame}} and \code{\link{jaggernaut}}.
jags_data_list <- function (x) {
  
  x <- as.list(x)
  
  if(!is_data_list(x)) {
    stop("x must be a uniquely named list with all elements of class integer, numeric, 
          factor, Date, POSIXt, matrix or array")
  }
  
  object <- x
  class(object) <- c("jags_data_list")
  ntries(object) <- 1
  
  return (object)
}
