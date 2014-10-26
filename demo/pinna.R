# Mark-recapture analysis of species detections

library(ggplot2)
library(scales) 

# M_t+X (Kery and Schaub 2011 164-165)
model <- jags_model (" model {
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
modify_data = function (data) {
      
  data$nrow <- length(data$size)
  stopifnot(nrow(data$y) == length(data$size))
  data$ncol <- ncol(data$y)

  return (data)
},
modify_data_derived = function (data) {
                       
data$nrow <- length(data$size)
return (data)
},
gen_inits = function (data) {
                      inits <- list()
                      inits$z <- rep(1,data$nrow)
                      inits$mu.size <- 0
                      
                      return (inits)
                    },
random_effects = list(z = NULL, eps = NULL, p = NULL),
select_data = c("y", "size+", "prior.sd.upper")                                      
)

data(pinna)
data <- pinna
size <- data$width

y <- data[,c("d1","d2")]
for (i in 1:ncol(y)) {
  y[,i] <- as.logical(y[,i])
}
y <- as.matrix(y)

data <- list(y = y, size = size, prior.sd.upper = 5)

analysis <- jags_analysis (model, data, mode = "demo")

coef(analysis)

plot(analysis, parm = "N")

newdata <- data.frame(size = seq(from = min(pinna$width), to = 30, length.out = 50))

prediction <- predict(analysis, newdata)

gp <- ggplot(data = prediction, aes(x = size, y = estimate))
gp <- gp + geom_line()
gp <- gp + scale_x_continuous(name = "Shell width (cm)")
gp <- gp + scale_y_continuous(name = "Detection probability",
                              labels = percent, expand = c(0,0))
gp <- gp + expand_limits(y = c(0,1))

print(gp)
