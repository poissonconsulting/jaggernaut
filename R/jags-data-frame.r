#' @title JAGS data frame
#'
#' @description 
#' Creates an object of class \code{jags_data_frame}.
#' 
#' @param x a data.frame from which to generate a \code{jags_data_frame} object.
#' @return A \code{jags_data_frame} object.
#' @seealso \code{\link{jags_data}},
#' \code{\link{jags_data_list}} and \code{\link{jaggernaut}}.
#' @export
jags_data_frame <- function (x) {
  
  data <- x
  
  if (!is.data.frame(data))
    stop("data must be a data.frame")
  
  object <- data
  class(object) <- c("jags_data_frame", "data.frame", "jags_data_list")
  ntries(object) <- 1
  
  jags_data_list(data)
  
  return (object)
}
