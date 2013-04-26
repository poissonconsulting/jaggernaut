
library(ggplot2)
library(scales)

# Mark-recapture analysis of species detections

# M_tbh (Kery and Schaub 2011 p.158-159)
mod1 <- jags_model(" model {
  omega ~ dunif(0, 1)
  sd ~ dunif(0, 3)
  gamma ~ dnorm(0, 10^-2)

  for (j in 1:ncol) {
    alpha[j] ~ dnorm(0, 2^-2)
    logit(mean.p[j]) <- alpha[j]
  }

  for (i in 1:nrow) {
    z[i] ~ dbern(omega)
    eps[i] ~ dnorm(0, sd^-2) T(-16, 16)
    logit(p[i, 1]) <- alpha[1] + eps[i]
    eY[i, 1] <- z[i] * p[i, 1]
    y[i,1] ~ dbern(eY)

    for (j in 2:ncol) {
      logit(p[i, j]) <- alpha[j] + eps[i] + gamma * y[i, (j-1)]
      eY[i, j] <- z[i] * p[i, j]
      y[i, j] ~ dbern(eY[i, j])
    }
  }
  N <- sum(z[])
}",
modify_data = function (data) {
  
  data$nrow <- nrow(data$y)
  data$ncol <- ncol(data$y)

  return (data)
},
gen_inits = function (data) {
  inits <- list()
  inits$z <- rep(1,data$nrow)
  return (inits)
},

random = list(z = NULL, eps = NULL, p = NULL)
)

# M0 (Kery and Schaub 2011 p.160-161)
mod2 <- jags_model(" model {
  omega ~ dunif(0, 1)
  p ~ dunif(0, 1)
           
  for (i in 1:nrow) {
    z[i] ~ dbern(omega)
    for (j in 1:ncol) {
      eY[i, j] <- z[i] * p
      y[i, j] ~ dbern(eY[i, j])
    }
  }
  N <- sum(z[])
}",
modify_data = function (data) {
  
  data$nrow <- nrow(data$y)
  data$ncol <- ncol(data$y)
  
  return (data)
  },
gen_inits = function (data) {
  inits <- list()
  inits$z <- rep(1,data$nrow)
  return (inits)
},
random = list(z = NULL)
)

mods <- list(mod1, mod2)

data(p610)
dat <- p610

dat <- dat[,substr(colnames(dat),1,5) == "count"]
for (i in 1:ncol(dat)) {
  dat[,i] <- as.logical(dat[,i])
}
dat <- as.matrix(dat)
dat <- list(y = dat)

an <- jags_analysis (mod, dat, niter = 10^5, mode = "debug")

coef(an)
