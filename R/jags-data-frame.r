
#' @title JAGS data frame
#'
#' @description 
#' Creates an object of class \code{jags_data_frame}.
#' 
#' @param x a data.frame from which to generate a \code{jags_data_frame} object.
#' @return A \code{jags_data_frame} object.
#' @seealso \code{\link{jags_data}},
#' \code{\link{jags_data_list}} and \code{\link{jaggernaut}}.
jags_data_frame <- function (x) {
    
  if(!is.data.frame(x) || !is_data_list(as.list(x))) {
    stop("x must be a data.frame with all variables of class integer, numeric, 
          factor, Date or POSIXt")
  }
  
  object <- x
  class(object) <- c("jags_data_frame", "data.frame", "jags_data_list")
  ntries(object) <- 1
    
  return (object)
}
