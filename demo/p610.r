
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
derived_code = "model {
  for (i in 1:nrow) {
    logit(prediction[i]) <- alpha[1] + beta * size[i] 
  }
}",
modify_data = function (data, analysis) {
  
  stopifnot(nrow(data$y) == length(data$size))
  
  data$nrow <- length(data$size)
  
  if (analysis) {
    data$ncol <- ncol(data$y)
  }
  return (data)
  },
 gen_inits = function (data) {
   inits <- list()
   inits$z <- rep(1,data$nrow)
   
   return (inits)
 },
 random = list(z = NULL, eps = NULL, p = NULL),
select = c("y","log_cbrt(size)*","prior.sd.upper")                                      
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

dat <- list(y = dat, size = bm, prior.sd.upper = 33)

log_cbrt <- function (x) {
  return (log(x^(1/3)))
}

an <- jags_analysis (mods, dat, niter = 10^5, mode = "default")

summary(an)

plot(an, model_number = 1, parm = "N")
plot(an, model_number = 2, parm = "N")

newdata <- list(size = seq(from = min(dat$size), to = 2000, length.out = 50))

pred <- predict(an, newdata, model_number = 3)

gp <- ggplot(data = pred, aes(x = size, y = estimate))
gp <- gp + geom_line()
gp <- gp + scale_x_continuous(name = "Body mass (g)")
gp <- gp + scale_y_continuous(name = "Detection probability",labels=percent,expand=c(0,0))
gp <- gp + expand_limits(y = c(0,0.55))

print(gp)
