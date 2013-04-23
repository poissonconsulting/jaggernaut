
library(ggplot2)
library(scales)

# Mark-recapture analysis of species detections

# M_tbh (Kery and Schaub 2011 p.158-159)
mod <- jags_model(" model {
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
    p.eff[i, 1] <- z[i] * p[i, 1]
    y[i,1] ~ dbern(p.eff[i,1])

    for (j in 2:ncol) {
      logit(p[i, j]) <- alpha[j] + eps[i] + gamma * y[i, (j-1)]
      p.eff[i, j] <- z[i] * p[i, j]
      y[i, j] ~ dbern(p.eff[i, j])
    }
  }
  N <- sum(z[])
}",
modify_data = function (data) {
  data$y <- cbind(data$count1,data$count2,data$count3,data$count4,data$count5)
  data$ncol <- 5
  data$count1 <- NULL
  data$count2 <- NULL
  data$count3 <- NULL
  data$count4 <- NULL
  data$count5 <- NULL
  return (data)
},
gen_inits = function (data) {
  inits <- list()
  inits$z <- rep(1,data$nrow)
  return (inits)
},
select = paste0("count",1:5)                  
)

data(p610)
dat <- p610

dat <- dat[,substr(colnames(dat),1,5) == "count"]
for (i in 1:ncol(dat)) {
  dat[,i] <- as.logical(dat[,i])
}

an <- jags_analysis (mod, dat, niter = 10^5, mode = "default")

coef(an)


