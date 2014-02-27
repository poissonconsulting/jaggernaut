# Simple Poisson model for fritillary butterfly data set (Kéry & Schaub, 2011, §12.3.1)

# library(devtools)
# install_github("jaggernaut", "joethorley", "v1.5.6")    # This demo built on v1.5.6
library(jaggernaut)
library(plyr)
library(reshape2)
#library(ggplot2)
#library(scales)
rm(list=ls())   # Clear out memory

# Count for sites i = 1..95, replicates j = 1,2 and days k = 1..7
# We apply an open-population binomial mixture model to estimate population during each day.

data(fritillary)    # Load the fritillary data contained in the jaggernaut package
fritillary <- rename(fritillary, c("site" = "Site", "day" = "Day", "count1" = "Rep1", "count2" = "Rep2"))
fritillary.melted <- melt(fritillary, id.vars = c("Site", "Day"), variable.name = "Replicate", value.name = "Count")

fritillary.melted$Site <- factor(fritillary.melted$Site)
fritillary.melted$Day <- factor(fritillary.melted$Day)
fritillary.melted$Replicate <- factor(fritillary.melted$Replicate)

# Specify the model in JAGS using jaggernaut:
simple.poisson <- jags_model("model{
  for(j in 1:nDay){                           # Loop over days
    alpha.lam[j] ~ dnorm(0, 0.01)
    p[j] ~ dunif(0, 1)

    lambda[j] <- exp(alpha.lam[j])

    for(i in 1:nSite){                        # Loop over sites
      N.singleRep[i, j] ~ dpois(lambda[j])
      for(k in 1:nReplicate){                 # Loop over replicates
        N[i, j, k] <- N.singleRep[i, j]
      }
    }
  }

  for(ii in 1:(nDay*nSite*nReplicate)){
    N.long[ii] <- N[Site[ii], Day[ii], Replicate[ii]]
  }

  for(ii in 1:length(Count)){              # Loop over all days, sites and replicates
    Count[ii] ~ dbin(p[Day[ii]], N.long[ii])
    eval[ii] <- p[Day[ii]] * N.long[ii]
    E[ii] <- ((Count[ii] - eval[ii])^2) / (eval[ii] + 0.5)

    Count.new[ii] ~ dbin(p[Day[ii]], N.long[ii])
    E.new[ii] <- ((Count.new[ii] - eval[ii])^2) / (eval[ii] + 0.5)
  }
}",
derived_code = "model{
  for(k in 1:7){
#    totalN[k] <- sum(N[,k])
    mean.abundance[k] <- exp(alpha.lam[k])
  }
  fit <- sum(E)
  fit.new <- sum(E.new)
}",
gen_inits = function (data) {
  
  inits <- list()
  inits.N.singleRep <- numeric(665)
  for (ii in 1:665){                           # Loop over days and sites
    inits.N.singleRep[ii] <- pmax(data$Count[ii], data$Count[ii + 665], na.rm = TRUE)
  }
  inits$N.long <- c(inits.N.singleRep, inits.N.singleRep)
  
  return (inits)
},
# No random effects
select = c("Count", "Site", "Day", "Replicate")
)

analysis.output <- jags_analysis(simple.poisson, fritillary.melted, mode = "demo")
