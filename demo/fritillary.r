# Simple Poisson model for fritillary butterfly data set (Kery & Schaub, 2011, 12.3.1)
# Patrick M. Hogan, 01 March 2014

library(plyr)
library(reshape2)
library(ggplot2)
library(scales) 

# We apply an open-population binomial mixture model to estimate population during each day.
# Count for sites i = 1..95, replicates j = 1,2 and days k = 1..7
model <- jags_model("model{
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
monitor = c("totalN", "mean.abundance", "alpha.lam", "p", "fit", "fit.new"),
select_data = c("Count", "Site", "Day")
)

# In Kery & Schaub's code, the data is passed as a 3-dim array. Instead, we melt the data into long format, as preferred in jaggernaut. However, we retain this 3-dim structure in the priors for convenience.
data(fritillary)  
data <- fritillary
data <- rename(data, c("site" = "Site", "day" = "Day", "count1" = "Rep1", "count2" = "Rep2"))
data <- melt(data, id.vars = c("Site", "Day"), variable.name = "Replicate", value.name = "Count")

data$Site <- factor(data$Site)
data$Day <- factor(data$Day)
data$Replicate <- factor(data$Replicate)

opts_jagr(nchains = 3, nsamples = 3000)
analysis <- jags_analysis(model, data, niters = 10^4) 
# Convergence can be elusive, even for niters = 10^5 -- needs fixing!

summary(analysis)

fit <- coef(analysis, parm = c("fit","fit.new"), level = "no")
fit <- as.data.frame(t(fit))

gp <- ggplot(data = fit, aes(x = fit, y = fit.new))
gp <- gp + geom_abline(intercept = 0, slope = 1)
gp <- gp + geom_point()
gp <- gp + scale_x_continuous(name = "Discrepancy actual data")
gp <- gp + scale_y_continuous(name = "Discrepancy replicate data")

print(gp)
