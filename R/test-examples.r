#' @title Test Examples
#'
#' @description 
#' Test examples to be run manually by package maintainer.
#' @name test_examples
#' @seealso \code{\link{jaggernaut}}
#' @examples
#' \dontrun{
#'  
#' registerDoParallel() 
#' opts_jagr("parallel" = TRUE)
#' 
#' demo(peregrine, ask= FALSE)
#' demo(hm, ask = FALSE)
#' demo(woodchat, ask = FALSE)
#' demo(tits, ask = FALSE)
#' demo(p610, ask = FALSE)
#' 
#' # stop cluster if registered parallel backend in windows
#' if(.Platform$OS.type == "windows") stopImplicitCluster()
#' 
#' opts_jagr("parallel" = FALSE)
#' registerDoSEQ()
#' }
#'
NULL
