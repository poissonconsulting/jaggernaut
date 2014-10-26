
#' Coal tits breeding survey data
#'
#'
#' The Swiss coal tit (\emph{Parus ater}) annual territory counts from 
#' the Swiss breeding bird survey MHB from 1999 to 2007.
#' 
#' The variables are as follows:
#' \itemize{
#'   \item \code{site} the site code (factor with 235 levels).
#'   \item \code{spec} the species (factor with one level = "Coaltit").
#'   \item \code{elevation} the elevation im masl (integer).
#'   \item \code{forest} the percent forest cover (integer).
#'   \item \code{y1999, y2000, ..., y2007} the site count by year (integer).
#'   \item \code{obs1999, obs2000, ..., obs2007} the observer code by year (integer).
#'   \item \code{first1999, first2000, ..., first2007} the first-time observer indicator by year (integer with two values 0 or 1).
#' }
#'
#' @format A data frame with 235 rows and 31 columns
#' @source Kery & Schaub (2011 p.95-96) 
#' @references 
#' Kery M & Schaub M Bayesian Population Analysis
#' using WinBUGS. Academic Press. (\url{http://www.vogelwarte.ch/bpa})
#' @docType data
#' @name tits
#' @usage tits
#' @keywords datasets
#' @examples
#' data(tits)
#' summary(tits)
#' # demo(tits)
NULL
