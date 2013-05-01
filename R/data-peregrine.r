
#' @title Peregrine falcon breeding population data
#'
#' @description
#' The peregrine falcon (\emph{Falco peregrinus}) population
#' breeding in the French Jura from 1964 to 2003.
#'
#'  The variables are as follows:
#' \itemize{
#'   \item \code{Year} the year (integer).
#'   \item \code{Pairs} the number of adult pairs (integer).
#'   \item \code{R.pairs} the number of reproductive pairs (integer).
#'   \item \code{Eyasses} the number of fledged young (integer).
#' }
#'
#' @format A data frame with 40 rows and 4 columns
#' @source Kery & Schaub (2011 p.64-65) courtesy of R.-J. Monneret
#' @references 
#' Kery M & Schaub M (2011) Bayesian Population Analysis
#' using WinBUGS. Academic Press. (\url{http://www.vogelwarte.ch/bpa})
#' @docType data
#' @name peregrine
#' @usage peregrine
#' @keywords datasets
#' @examples
#' data(peregrine)
#' summary(peregrine)
#' # demo(peregrine)
NULL
