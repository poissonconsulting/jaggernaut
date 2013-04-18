
#' @title Peregrine falcon breeding population
#'
#' @description
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
#' @examples
#' # Poisson GLM analysis of peregrine breeding pairs (Kery & Schaub 2011 p.55-66)
#' mod <- model(" 
#'  model { 
#'    alpha ~ dunif(-20, 20)
#'    beta1 ~ dunif(-10, 10)
#'    beta2 ~ dunif(-10, 10)
#'    beta3 ~ dunif(-10, 10)
#'    
#'    for (i in 1:nrow) { 
#'      log(eCount[i]) <- alpha + beta1 * Year[i] 
#'        + beta2 * Year[i]^2 + beta3 * Year[i]^3
#'      Count[i] ~ dpois(eCount[i])
#'    } 
#'  }",
#'  derived_code = "model{
#'    for (i in 1:nrow) {
#'      log(eCount[i]) <- alpha + beta1 * Year[i] 
#'        + beta2 * Year[i]^2 + beta3 * Year[i]^3    
#'    }
#'  }",
#' select = c("Count","Year*")
#')
#'
#' dat <- peregrine
#' dat$Count <- dat$Pairs
#'
#' ana <- analysis (mod, dat)
#'
#' der <- derived(ana, "eCount", data = "Year")
#'
#' gp <- ggplot(data = der, aes(x = Year, y = estimate))
#' gp <- gp + geom_line()
#' gp <- gp + geom_line(aes(y = lower), linetype = "dotted")
#' gp <- gp + geom_line(aes(y = upper), linetype = "dotted")
#' gp <- gp + geom_point(data = peregrine, aes(y = Pairs))
#' gp <- gp + scale_y_continuous(name = "Pair count")
#' gp <- gp + expand_limits(y = 0)
#' 
#' print(gp)
NULL
