
library(ggplot2)
library(scales) 

# Mark-recapture analysis of species detections

# M_t+X (Kery and Schaub 2011 164-165)
mod1 <- jags_model (" model {
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
                    size[i] ~ dnorm(mu.size, sd.size^-2) T(-15, 15)
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
      
  data$nrow <- length(data$size)
  
  if (analysis) {
    stopifnot(nrow(data$y) == length(data$size))
    data$ncol <- ncol(data$y)
  }
  return (data)
  },
                    gen_inits = function (data) {
                      inits <- list()
                      inits$z <- rep(1,data$nrow)
                      inits$mu.size <- 0
                      
                      return (inits)
                    },
                    random = list(z = NULL, eps = NULL, p = NULL),
                    select = c("y","size+","prior.sd.upper")                                      
)

data(pinna)
dat <- pinna
size <- dat$width

y <- dat[,c("d1","d2")]
for (i in 1:ncol(y)) {
  y[,i] <- as.logical(y[,i])
}
y <- as.matrix(y)

dat <- list(y = y, size = size, prior.sd.upper = 5)

an <- jags_analysis (mod1, dat, mode = "default")

coef(an)

plot(an,parm = "N")

newdata <- list(size = seq(from = min(pinna$width), to = 30, length.out = 50))

pred <- predict(an, newdata)

pred$size <- newdata$size 

gp <- ggplot(data = pred, aes(x = size, y = estimate))
gp <- gp + geom_line()
gp <- gp + scale_x_continuous(name = "Shell width (cm)")
gp <- gp + scale_y_continuous(name = "Detection probability",labels=percent,expand=c(0,0))
gp <- gp + expand_limits(y = c(0,1))

print(gp)
