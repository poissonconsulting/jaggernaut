
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
    y[i,1] ~ dbern(eY[i, 1])

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
random = list(z = NULL, eps = NULL, p = NULL),
select = c("y")
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
random = list(z = NULL),
select = c("y")                  
)

# M_t+X (Kery and Schaub 2011 164-165)
mod3 <- jags_model (" model {
  omega ~ dunif(0, 1)
  beta ~ dnorm(0, 10^-2)
  mu.size ~ dnorm(0, 10^-2)
  sd.size ~ dunif(0, prior.sd.upper)
  
  for (j in 1:ncol) {
    alpha[j] ~ dnorm(0, 2^-2)
    logit(mean.p[j]) <- alpha[j]
  }
                   
  for (i in 1:nrow) {
    z[i] ~ dbern(omega)
    size[i] ~ dnorm(mu.size, sd.size^-2) T(-6, 6)
    for (j in 1:ncol) {
      logit(p[i, j]) <- alpha[j] + beta * size[i]
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
 random = list(z = NULL, eps = NULL, p = NULL),
select = c("y","size","prior.sd.upper")                                      
)


mods <- list(mod1,mod2,mod3)

data(p610)
dat <- p610

bm <- dat$bm

dat <- dat[,substr(colnames(dat),1,5) == "count"]
for (i in 1:ncol(dat)) {
  dat[,i] <- as.logical(dat[,i])
}
dat <- as.matrix(dat)
is.na(bm[!apply(dat,1,max)]) <- T
bm <- log(bm^(1/3))
mean.bm <- mean(bm, na.rm = T)
bm <- bm - mean.bm

dat <- list(y = dat, size = bm, prior.sd.upper = 33)

an <- jags_analysis (mods, dat, niter = 10^5, mode = "debug")

coef(an, model_number = 1)
coef(an,model_number = 2)
coef(an,model_number = 3)
