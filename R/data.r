
#' Peregrine falcon breeding population
#'
#'
#' The peregrine falcon (\emph{Falco peregrinus}) population
#' breeding in the French Jura from 1964 to 2003 from 
#' Kery & Schaub (2011 p.64-65 - data courtesy of R.-J. Monneret).
#' The variables are as follows:
#' \itemize{
#'   \item \code{Year} the year.
#'   \item \code{Pairs} the number of adult pairs.
#'   \item \code{R.pairs} the number of reproductive pairs.
#'   \item \code{Eyasses} the number of fledged young.
#' }
#'
#' @docType data
#' @name peregrine
#' @usage peregrine
#' @format A data frame with 40 rows and 4 columns
#' @references 
#' Kery M & Schaub M (2011) Bayesian Population Analysis
#' using WinBUGS. Academic Press. (\url{http://www.vogelwarte.ch/bpa})
#' @keywords datasets
NULL


#' Swiss coal tits
#'
#'
#' The Swiss coal tit (\emph{Parus ater}) annual territory counts from 235
#' sites from 1999 to 2007 from 
#' Kery & Schaub (2011 p.95-96 - data from Swiss breeding bird survey MHB).
#' The variables are as follows:
#' \itemize{
#'   \item \code{site} the site.
#'   \item \code{spec} the species (Coaltit).
#'   \item \code{elevation} the elevation.
#'   \item \code{forest} the forest cover.
#'   \item \code{y1999, y2000, ..., y2007} the site count by year.
#'   \item \code{obs1999, obs2000, ..., obs2007} the observer code by year.
#'   \item \code{first1999, first2000, ..., first2007} the first-time observer indicator by year.
#' }
#'
#' @docType data
#' @name tits
#' @usage tits
#' @format A data frame with 235 rows and 31 columns
#' @references 
#' Kery M & Schaub M (2011) Bayesian Population Analysis
#' using WinBUGS. Academic Press. (\url{http://www.vogelwarte.ch/bpa})
#' @keywords datasets
NULL


#' Point count number 610
#'
#'
#' The bird point count data from the Czech republic for point count number
#' 610 in 2004-2005 from 
#' Kery & Schaub (2011 p.157 - data courtesy of Jiri Reif).
#' 
#' The variables are as follows:
#' \itemize{
#'   \item \code{species} the species.
#'   \item \code{point} the point count number (610).
#'   \item \code{bm} unknown.
#'   \item \code{specnr} the species number .
#'   \item \code{count1, count2, ..., count5} the number of individuals counted by occasion (1-5).
#' }
#'
#' @docType data
#' @name p610
#' @usage p610
#' @format A data frame with 146 rows and 9 columns
#' @references 
#' Kery M & Schaub M (2011) Bayesian Population Analysis
#' using WinBUGS. Academic Press. (\url{http://www.vogelwarte.ch/bpa})
#' @keywords datasets
NULL


#' Pen shell detection data
#'
#'
#' The pen shell (\emph{Pinna nobilis}) detection data from the Balearic Islands 
#' in 2010 from 
#' Kery & Schaub (2011 p.166). The data consist of the detection history of each 
#' shell along with its width.
#' 
#' The variables are as follows:
#' \itemize{
#'   \item \code{d1} indicator for shell detected by first team.
#'   \item \code{d2} indicator for shell detected by second team.
#'   \item \code{width} shell width (cm).
#' }
#'
#' @docType data
#' @name pinna
#' @usage pinna
#' @format A data frame with 143 rows and 3 columns
#' @references 
#' Kery M & Schaub M (2011) Bayesian Population Analysis
#' using WinBUGS. Academic Press. (\url{http://www.vogelwarte.ch/bpa})
#' @keywords datasets
NULL


#' Showy lady's slipper capture-recapture data
#'
#'
#' The showy lady's slipper (\emph{Cypripedium reginae}) 
#' capture-recapture data collected by Kathy Gregg in Big Draft (West Virginia) from
#' 1989 to 1999 from 
#' Kery & Schaub (2011 p.166). 
#' 
#' The variables are as follows:
#' \itemize{
#'   \item \code{V1, V2, ..., V11} the state of the individual plant by year
#'   where 0 = undetected, 1 = vegetative and 2 = flowering.
#' }
#'
#' @docType data
#' @name orchids
#' @usage orchids
#' @format A data frame with 250 rows and 11 columns
#' @references 
#' Kery M & Schaub M (2011) Bayesian Population Analysis
#' using WinBUGS. Academic Press. (\url{http://www.vogelwarte.ch/bpa})
#' @keywords datasets
NULL

