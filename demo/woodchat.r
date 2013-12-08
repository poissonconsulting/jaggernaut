# Binomial mixed-effects model for woodchat shrike (Kery 2010 p.231-235)

data_model <- jags_data_model("data {
  for (gr in 1:n.groups) {
    intercept.effects[gr] ~ dnorm(intercept.mean, intercept.sd^-2)
    slope.effects[gr] ~ dnorm(slope.mean, slope.sd^-2)
                              
    for (yr in 1:n.years) {
      prec[gr, yr] ~ dunif(0, 1)
      eN[gr, yr] ~ dunif(min.N, max.N)
      N[gr, yr] <- round(eN[gr, yr])
      logit(p[gr,yr]) <- intercept.effects[gr] + slope.effects[gr] * prec[gr, yr]
      C[gr, yr] ~ dbin(p[gr,yr], N[gr, yr])
    }
  }
} ",
extract_data = function (data) {
  data <- data[c("C","N","prec","n.groups","n.years")]
  return (data)
})

values <- expand.grid(n.groups = 16, n.years = 10, intercept.mean = 1,
                      intercept.sd = 1, slope.mean = -2, slope.sd = 1,
                      min.N = 10, max.N = 50)

model <- jags_model (" model {
  intercept.mean ~ dnorm(0, 10^-2)
  slope.mean ~ dnorm(0, 10^-2)
                     
  intercept.sd ~ dunif(0, 10)
  slope.sd ~ dunif(0, 10)
                                          
  for (gr in 1:n.groups) {
    intercept.effects[gr] ~ dnorm(intercept.mean, intercept.sd^-2)
    slope.effects[gr] ~ dnorm(slope.mean, slope.sd^-2)

    for (yr in 1:n.years) {
      logit(p[gr,yr]) <- intercept.effects[gr] + slope.effects[gr] * prec[gr, yr]
      C[gr, yr] ~ dbin(p[gr,yr], N[gr, yr])
    }
  }
}",
monitor = c("intercept.mean","slope.mean","intercept.sd","slope.sd",
            "intercept.effects","slope.effects"),
random_effects = list(intercept.effects = NULL, slope.effects = NULL))

data <- data_jags(data_model, values)

analysis <- jags_analysis(model, data, niters = 10^5, mode = "demo")

coef(analysis)
