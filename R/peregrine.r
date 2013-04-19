
#' @title Peregrine falcon breeding population
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
#' @docType data
#' @name peregrine
#' @usage peregrine
#' @format A data frame with 40 rows and 4 columns
#' @source Kery & Schaub (2011 p.64-65) courtesy of R.-J. Monneret
#' @seealso \code{\link{jaggernaut}} 
#' @references 
#' Kery M & Schaub M (2011) Bayesian Population Analysis
#' using WinBUGS. Academic Press. (\url{http://www.vogelwarte.ch/bpa})
#' @keywords datasets
#' @examples
#' # Poisson GLM analysis of peregrine breeding pairs (Kery & Schaub 2011 p.55-66)
#'
#' # GLM_Poisson (Kery & Schaub 2011 p.58-59)
#' mod <- model(" 
#'  model { 
#'    alpha ~ dunif(-20, 20)
#'    beta1 ~ dunif(-10, 10)
#'    beta2 ~ dunif(-10, 10)
#'    beta3 ~ dunif(-10, 10)
#'    
#'    for (i in 1:nrow) { 
#'      log(eC[i]) <- alpha + beta1 * Year[i] 
#'        + beta2 * Year[i]^2 + beta3 * Year[i]^3
#'      C[i] ~ dpois(eC[i])
#'    } 
#'  }",
#'  derived_code = "model{
#'    for (i in 1:nrow) {
#'      log(eC[i]) <- alpha + beta1 * Year[i] 
#'        + beta2 * Year[i]^2 + beta3 * Year[i]^3    
#'    }
#'  }",
#' select = c("C","Year*")
#')
#'
#' dat <- peregrine
#' 
#' dat$C <- dat$Pairs
#' an <- analysis (mod, dat)
#' estimates(an)
#' exp <- derived(an, "eC", data = "Year")
#'
#' gp <- ggplot(data = exp, aes(x = Year, y = estimate))
#' gp <- gp + geom_line(data = dat, aes(y = C), alpha = 1/3)
#' gp <- gp + geom_point(data = dat, aes(y = C), shape = 1)
#' gp <- gp + geom_line()
#' gp <- gp + scale_y_continuous(name = "Pair count")
#' gp <- gp + expand_limits(y = 0)
#' 
#' print(gp)
#' 
#' # Poisson GLM analysis of peregrine nestlings (Kery & Schaub 2011 p.66-67)
#' 
#' # uses GLM_Poisson model from previous example
#' 
#' dat <- peregrine
#' 
#' dat$C <- dat$Eyasses
#' an <- analysis (mod, dat)
#' estimates(an)
#' exp <- derived(an, "eC", data = "Year")

#' gp <- ggplot(data = exp, aes(x = Year, y = estimate))
#' gp <- gp + geom_line(data = dat, aes(y = C), alpha = 1/3)
#' gp <- gp + geom_point(data = dat, aes(y = C), shape = 1)
#' gp <- gp + geom_line()
#' gp <- gp + scale_y_continuous(name = "Nestling count")
#' gp <- gp + expand_limits(y = 0)
#' 
#' print(gp)
#' 
#' # Binomial GLM analysis of peregrine reproductive success (Kery & Schaub 2011 p.67-71)
#'
#' # GLM_Binomial (Kery & Schaub 2011 p.68-69)
#' mod <- model(" 
#'  model { 
#'    alpha ~ dnorm(0, 10^-2)
#'    beta1 ~ dnorm(0, 10^-2)
#'    beta2 ~ dnorm(0, 10^-2)
#'    
#'    for (i in 1:nrow) { 
#'      logit(eP[i]) <- alpha + beta1 * Year[i] + beta2 * Year[i]^2 
#'      C[i] ~ dbin(eP[i], N[i])
#'    } 
#'  }",
#'  derived_code = "model{
#'    for (i in 1:nrow) {
#'      logit(eP[i]) <- alpha + beta1 * Year[i] + beta2 * Year[i]^2  
#'    }
#'  }",
#' select = c("C","N","Year*")
#')
#'
#' dat <- peregrine
#'
#' dat$C <- dat$R.Pairs
#' dat$N <- dat$Pairs
#' 
#' an <- analysis (mod, dat)
#' estimates(an)
#' exp <- derived(an, "eP", data = "Year")

#' gp <- ggplot(data = exp, aes(x = Year, y = estimate))
#' gp <- gp + geom_line(data = dat, aes(y = C/N), alpha = 1/3)
#' gp <- gp + geom_point(data = dat, aes(y = C/N), shape = 1)
#' gp <- gp + geom_line()
#' gp <- gp + scale_y_continuous(name = "Proportion successful pairs", expand=c(0,0))
#' gp <- gp + expand_limits(y = c(0,1))
#' 
#' print(gp)
#' 
#' # Overdispersed Poisson GLMM analysis of peregrine breeding pairs (Kery & Schaub 2011 p.82-90)
#'
#' # GLMM_Poisson (Kery & Schaub 2011 p.87)
#' mod <- model(" 
#'  model { 
#'    alpha ~ dunif(-20, 20)
#'    beta1 ~ dunif(-10, 10)
#'    beta2 ~ dunif(-10, 10)
#'    beta3 ~ dunif(-10, 10)
#'    sd ~ dunif(0, 5)
#'    
#'    for (i in 1:nrow) { 
#'      eps[i] ~ dnorm(0, sd^-2)
#'      eLogC[i] <- alpha + beta1 * Year[i] 
#'        + beta2 * Year[i]^2 + beta3 * Year[i]^3
#'      log(eC[i]) <- eLogC[i] + eps[i]
#'      C[i] ~ dpois(eC[i])
#'    } 
#'  }",
#'  derived_code = "model{
#'    for (i in 1:nrow) {
#'      log(eC[i]) <- alpha + beta1 * Year[i] 
#'        + beta2 * Year[i]^2 + beta3 * Year[i]^3
#'    }
#'  }",
#' random = list(eps = "Year"),
#' select = c("C","Year*")
#')
#'
#' dat <- peregrine
#' 
#' dat$C <- dat$Pairs
#' an <- analysis (mod, dat, n.iter = 10^4)
#' estimates(an)
#' estimates(an, parameters = "fixed")
#' estimates(an, parameters = "random")
#' estimates(an, parameters = "all")
#' 
NULL
