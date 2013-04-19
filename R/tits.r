
#' Swiss coal tits
#'
#'
#' The Swiss coal tit (\emph{Parus ater}) annual territory counts from 
#' the Swiss breeding bird survey from 1999 to 2007 from 
#' Kery & Schaub (2011 p.95-96).
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
#' @docType data
#' @name tits
#' @usage tits
#' @format A data frame with 235 rows and 31 columns
#' @seealso \code{\link{jaggernaut}} 
#' @references 
#' Kery M & Schaub M (2011) Bayesian Population Analysis
#' using WinBUGS. Academic Press. (\url{http://www.vogelwarte.ch/bpa})
#' @keywords datasets
#' @examples
#' # Poisson GLM analyses of coal tit counts (Kery & Schaub 2011 p.95-110)
#'
#' # Whereas Kery & Schaub (2011) pass the counts, observer codes and
#' # first time observer indicators as site x year matrices in the following
#' # code they are melted into long format and passed as a single data frame.
#' # Also in the following code the order of the models is reversed with the
#' # full model being the first model.
#'
#' # GLMM5 (Kery & Schaub 2011 p.108-109)
#' mod1 <- model(" 
#'  model { 
#'    mu ~ dnorm(0, 10^-2)
#'    beta1 ~ dnorm(0, 10^-2)
#'    beta2 ~ dnorm(0, 10^-2)
#'    
#'    sd.alpha ~ dunif(0, 3)
#'    for (j in 1:nsite) {
#'      alpha[j] ~ dnorm (0, sd.alpha^-2)
#'    }
#'    
#'    sd.eps ~ dunif(0, 1)  
#'    for (i in 1:nyearFac) {
#'      eps[i] ~ dnorm (0, sd.eps^-2)
#'    }
#'    
#'    sd.gamma ~ dunif(0, 1)
#'    for (i in 1:nobs) {
#'      gamma[i] ~ dnorm (0, sd.gamma^-2)
#'    }   
#'    
#'    for (i in 1:nrow) {    
#'      log(eC[i]) <- mu + beta1 * year[i] + beta2 * first[i] 
#'          + alpha[site[i]] + gamma[obs[i]] + eps[yearFac[i]]
#'      C[i] ~ dpois(eC[i])
#'    } 
#'  }",
#'  derived_code = "model{
#'    for (i in 1:nrow) {
#'      log(eC[i]) <- mu + beta1 * year[i] + beta2 * first[i] 
#'          + alpha[site[i]] + gamma[obs[i]] + eps[yearFac[i]]  
#'    }
#'  }",
#' random = list(alpha = "site", eps = "nyearFac", gamma = "obs"),
#' select = c("C","site","year*","yearFac","obs","first")
#')
#'
#' # GLMM4 (Kery & Schaub 2011 p.106-107)
#' mod2 <- model(" 
#'  model { 
#'    mu ~ dnorm(0, 10^-2)
#'    beta1 ~ dnorm(0, 10^-2)
#'    beta2 ~ dnorm(0, 10^-2)
#'    
#'    sd.alpha ~ dunif(0, 3)
#'    for (j in 1:nsite) {
#'      alpha[j] ~ dnorm (0, sd.alpha^-2)
#'    }
#'    
#'    sd.eps ~ dunif(0, 1)  
#'    for (i in 1:nyearFac) {
#'      eps[i] ~ dnorm (0, sd.eps^-2)
#'    }
#'    
#'    for (i in 1:nrow) {    
#'      log(eC[i]) <- mu + beta1 * year[i] + beta2 * first[i] 
#'          + alpha[site[i]] + eps[yearFac[i]]
#'      C[i] ~ dpois(eC[i])
#'    } 
#'  }",
#'  derived_code = "model{
#'    for (i in 1:nrow) {
#'      log(eC[i]) <- mu + beta1 * year[i] + beta2 * first[i] 
#'          + alpha[site[i]] + eps[yearFac[i]]  
#'    }
#'  }",
#' random = list(alpha = "site", eps = "nyearFac"),
#' select = c("C","site","year*","yearFac","first")
#')
#'
#' # GLMM3 (Kery & Schaub 2011 p.105)
#' mod3 <- model(" 
#'  model { 
#'    mu ~ dnorm(0, 10^-2)
#'    beta2 ~ dnorm(0, 10^-2)
#'    
#'    sd.alpha ~ dunif(0, 3)
#'    for (j in 1:nsite) {
#'      alpha[j] ~ dnorm (0, sd.alpha^-2)
#'    }
#'    
#'    sd.eps ~ dunif(0, 1)  
#'    for (i in 1:nyearFac) {
#'      eps[i] ~ dnorm (0, sd.eps^-2)
#'    }
#'    
#'    for (i in 1:nrow) {    
#'      log(eC[i]) <- mu + beta2 * first[i] 
#'          + alpha[site[i]] + eps[yearFac[i]]
#'      C[i] ~ dpois(eC[i])
#'    } 
#'  }",
#'  derived_code = "model{
#'    for (i in 1:nrow) {
#'      log(eC[i]) <- mu + beta2 * first[i] 
#'          + alpha[site[i]] + eps[yearFac[i]]  
#'    }
#'  }",
#' random = list(alpha = "site", eps = "nyearFac"),
#' select = c("C","site","yearFac","first")
#')
#'
#' # GLMM2 (Kery & Schaub 2011 p.103-104)
#' mod4 <- model(" 
#'  model { 
#'    mu ~ dnorm(0, 10^-2)
#'        
#'    sd.alpha ~ dunif(0, 3)
#'    for (j in 1:nsite) {
#'      alpha[j] ~ dnorm (0, sd.alpha^-2)
#'    }
#'    
#'    sd.eps ~ dunif(0, 1)  
#'    for (i in 1:nyearFac) {
#'      eps[i] ~ dnorm (0, sd.eps^-2)
#'    }
#'    
#'    for (i in 1:nrow) {    
#'      log(eC[i]) <- mu + alpha[site[i]] + eps[yearFac[i]]
#'      C[i] ~ dpois(eC[i])
#'    } 
#'  }",
#'  derived_code = "model{
#'    for (i in 1:nrow) {
#'      log(eC[i]) <- mu + alpha[site[i]] + eps[yearFac[i]]
#'    }
#'  }",
#' random = list(alpha = "site", eps = "nyearFac"),
#' select = c("C","site","yearFac")
#')
#'
#' # GLMM1 (Kery & Schaub 2011 p.102)
#' mod5 <- model(" 
#'  model { 
#'    mu ~ dnorm(0, 10^-2)
#'        
#'    sd.alpha ~ dunif(0, 3)
#'    for (j in 1:nsite) {
#'      alpha[j] ~ dnorm (0, sd.alpha^-2)
#'    }
#'    
#'    for (i in 1:nrow) {    
#'      log(eC[i]) <- mu + alpha[site[i]] 
#'      C[i] ~ dpois(eC[i])
#'    } 
#'  }",
#'  derived_code = "model{
#'    for (i in 1:nrow) {
#'      log(eC[i]) <- mu + alpha[site[i]] 
#'    }
#'  }",
#' random = list(alpha = "site"),
#' select = c("C","site")
#')
#'
#'#' # GLM0 (Kery & Schaub 2011 p.102)
#' mod6 <- model(" 
#'  model { 
#'    mu ~ dnorm(0, 10^-2)
#'    
#'    for (i in 1:nrow) {    
#'      log(eC[i]) <- mu
#'      C[i] ~ dpois(eC[i])
#'    } 
#'  }",
#'  derived_code = "model{
#'    for (i in 1:nrow) {
#'      log(eC[i]) <- mu
#'    }
#'  }",
#' select = c("C")
#')
#' 
#' mods <- list(mod1,mod2,mod3,mod4,mod5,mod6)
#'
#' dat <- tits
#' year <- subset(dat,select=c("site",paste0("y",1999:2007)))
#' obs <- subset(dat,select=c("site",paste0("obs",1999:2007)))
#' first <- subset(dat,select=c("site",paste0("first",1999:2007)))
#' 
#' year <- melt(year,id.vars = "site", variable.name = "year", value.name = "C")
#' obs <- melt(obs,id.vars = "site", variable.name = "year", value.name = "obs")
#' first <- melt(first,id.vars = "site", variable.name = "year", value.name = "first")
#' 
#' year$year <- as.integer(substr(as.character(year$year),2,5))
#' obs$year <- as.integer(substr(as.character(obs$year),4,7))
#' first$year <- as.integer(substr(as.character(first$year),6,9))
#' 
#' dat <- merge(year, obs, by = c("site","year"))
#' dat <- merge(dat, first, by = c("site","year"))
#' 
#' dat$obs[is.na(dat$obs)] <- 272
#' dat$first[is.na(dat$first)] <- 0
#' 
#' dat$yearFac <- factor(dat$year)
#' dat$obs <- factor(dat$obs)
#' 
#' gp <- ggplot(data = dat, aes(x = year, y = C))
#' gp <- gp + geom_line(aes(group = site, color = site))
#' gp <- gp + scale_y_continuous(name = "Territory count")
#' gp <- gp + scale_x_continuous(name = "Year", breaks = seq(2000,2006,by=2))
#' gp <- gp + expand_limits(y = 0)
#' gp <- gp + theme(legend.position = "none")
#' 
#' print(gp)
#'
#' an <- analysis (mods, dat, n.iter = 10^4)
#'
#' summary(an)
#' 
#' exp1 <- derived(an, "eC", data = "site")
#'
#' gp <- ggplot(data = exp1, aes(x = site, y = estimate))
#' gp <- gp + geom_pointrange(aes(ymin = lower, ymax = upper))
#' gp <- gp + scale_y_continuous(name = "Territory count", expand = c(0,0))
#' gp <- gp + scale_x_discrete(name = "Territory", breaks = NULL)
#' gp <- gp + expand_limits(y = 0)
#' 
#' print(gp)
#' 
#' exp2 <- derived(an, "eC", data = "site", model = 2)
#' 
#' gp <- gp %+% exp2
#'
#' print(gp)
#' 
NULL
