# Simple Poisson model for fritillary butterfly data set (Kéry & Schaub, 2011, §12.3.1)
# Patrck M. Hogan, 01 March 2014

# library(devtools)
# install_github("jaggernaut", "joethorley", "v1.5.6")    # This demo built on v1.5.6
library(jaggernaut)
library(plyr)
library(reshape2)
library(pingr)
rm(list=ls())     # Clear out memory

# We apply an open-population binomial mixture model to estimate population during each day.
# Count for sites i = 1..95, replicates j = 1,2 and days k = 1..7

data(fritillary)    # Load the fritillary data contained in the jaggernaut package
fritillary <- rename(fritillary, c("site" = "Site", "day" = "Day", "count1" = "Rep1", "count2" = "Rep2"))
fritillary.melted <- melt(fritillary, id.vars = c("Site", "Day"), variable.name = "Replicate", value.name = "Count")

# In Kéry & Schaub's code, the data is passed as a 3-dim array. Instead, we melt the data into long format, as preferred in jaggernaut. However, we retain this 3-dim structure in the priors for convinence.

fritillary.melted$Site <- factor(fritillary.melted$Site)
fritillary.melted$Day <- factor(fritillary.melted$Day)
fritillary.melted$Replicate <- factor(fritillary.melted$Replicate)

# Specify the model in JAGS using jaggernaut:
simple.poisson <- jags_model("model{
  for (j in 1:nDay){
      alpha.lam[j] ~ dnorm(0, 0.01)
      p[j] ~ dunif(0, 1)
      
      lambda[j] <- exp(alpha.lam[j])
      
      for(i in 1:nSite){
          N[i, j] ~ dpois(lambda[j])
      }
  }
  
  for (ii in 1:length(Count)){
      Count[ii] ~ dbin(p[Day[ii]], N[Site[ii], Day[ii]])
      eval[ii] <- p[Day[ii]] * N[Site[ii], Day[ii]]
      E[ii] <- ((Count[ii] - eval[ii])^2) / (eval[ii] + 0.5)

      Count.new[ii] ~ dbin(p[Day[ii]], N[Site[ii], Day[ii]])
      E.new[ii] <- ((Count.new[ii] - eval[ii])^2) / (eval[ii] + 0.5)
  }

  for(k in 1:nDay){
    totalN[k] <- sum(N[,k])
    mean.abundance[k] <- exp(alpha.lam[k])
  }
   fit <- sum(E)
   fit.new <- sum(E.new)
}",
derived_code = "model{
}",
gen_inits = function (data) {
    
    inits <- list()
    inits.N.long <- data$Count
    inits.N.long[is.na(inits.N.long)] <- 0
    inits.N  <- matrix(, data$nSite, data$nDay)
    for(k in 1:data$nDay){
        for(i in 1:data$nSite){
            inits.N[i, k] <- 1+max(inits.N.long[i + (k-1)*data$nSite], inits.N.long[i + (k-1)*data$nSite + (data$nSite*data$nDay)])
        }
    }
    inits$N <- inits.N
    inits$alpha.lam <- runif(data$nDay, -1, 1)
    
    return (inits)
},
# No random effects
monitor = c("totalN", "mean.abundance", "alpha.lam", "p", "fit", "fit.new"),
select = c("Count", "Site", "Day")
)

opts_jagr(nchains = 3, nsims = 2000) # To accurately parallel the Kéry & Schaub example this should be 3000, but limited by jaggernaut.
analysis.output <- jags_analysis(simple.poisson, fritillary.melted, niters = 10^4) # Convergence can be elusive, even for niters = 10^5 -- needs fixing!

summary(analysis.output)

plot(analysis.output$analyses$Model1$chains$samples$fit, analysis.output$analyses$Model1$chains$samples$fit.new, main = "", xlab = "Discrepancy actual data", ylab = "Discrepancy replicate data", frame.plot = FALSE)
abline(0, 1, lwd = 2, col = "black")
