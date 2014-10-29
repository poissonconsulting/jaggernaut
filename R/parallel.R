#' @title Run analyses in parallel
#'
#' @description 
#' In jaggernaut analyses can be run in parallel using a registered
#' parallel backend.
#' @name parallel
#' @seealso \code{\link{jags_model}},
#' \code{\link{jags_analysis}} and \code{\link{opts_jagr}}.
#' @examples
#'  
#' \dontrun{ 
#' # run peregrine demo using registered parallel backend
#' 
#' registerDoParallel() 
#' opts_jagr("parallel" = TRUE)
#' 
#' demo(peregrine)
#' 
#' # stop cluster if registered parallel backend in windows
#' if(.Platform$OS.type == "windows") stopImplicitCluster()
#' 
#' opts_jagr("parallel" = FALSE)
#' registerDoSEQ()
#' 
#' # further information on the peregrine dataset and demo
#' ?peregrine
#' }
#'
NULL
