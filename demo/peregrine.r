# Poisson GLM analysis of peregrine breeding pairs (Kery & Schaub 2011 p.55-66)

library(ggplot2)
library(scales) 

# GLM_Poisson (Kery & Schaub 2011 p.58-59)
model <- jags_model("
             model {
             alpha ~ dunif(-20, 20)
             beta1 ~ dunif(-10, 10)
             beta2 ~ dunif(-10, 10)
             beta3 ~ dunif(-10, 10)
             
             for (i in 1:length(Year)) {
             log(eC[i]) <- alpha + beta1 * Year[i] 
                + beta2 * Year[i]^2 + beta3 * Year[i]^3
             C[i] ~ dpois(eC[i])
             }
             }",
 derived_code = "model{
  for (i in 1:length(Year)) {
    log(prediction[i]) <- alpha + beta1 * Year[i]
        + beta2 * Year[i]^2 + beta3 * Year[i]^3
    }
 }",
select = c("C","Year*")
)

data(peregrine)
data <- peregrine

data$C <- data$Pairs
analysis <- jags_analysis (model, data, mode = "demo")

coef(analysis)
prediction <- predict(analysis, newdata = "Year")

gp <- ggplot(data = prediction, aes(x = Year, y = estimate))
gp <- gp + geom_line(data = data, aes(y = C), alpha = 1/3)
gp <- gp + geom_point(data = data, aes(y = C), shape = 1)
gp <- gp + geom_line()
gp <- gp + scale_y_continuous(name = "Pair count")
gp <- gp + expand_limits(y = 0)

print(gp)

# Poisson GLM analysis of peregrine nestlings (Kery & Schaub 2011 p.66-67)

# uses GLM_Poisson model from previous example

data(peregrine)
data <- peregrine

data$C <- data$Eyasses
analysis <- jags_analysis (model, data, mode = "demo")
coef(analysis)
prediction <- predict(analysis, newdata = "Year")

gp <- ggplot(data = prediction, aes(x = Year, y = estimate))
gp <- gp + geom_line(data = data, aes(y = C), alpha = 1/3)
gp <- gp + geom_point(data = data, aes(y = C), shape = 1)
gp <- gp + geom_line()
gp <- gp + scale_y_continuous(name = "Nestling count")
gp <- gp + expand_limits(y = 0)

print(gp)

# Binomial GLM analysis of peregrine reproductive success (Kery & Schaub 2011 p.67-71)

# GLM_Binomial (Kery & Schaub 2011 p.68-69)
model <- jags_model("
             model {
             alpha ~ dnorm(0, 10^-2)
             beta1 ~ dnorm(0, 10^-2)
             beta2 ~ dnorm(0, 10^-2)
             
             for (i in 1:length(Year)) {
             logit(eP[i]) <- alpha + beta1 * Year[i] + beta2 * Year[i]^2
             C[i] ~ dbin(eP[i], N[i])
             }
             }",
 derived_code = "model{
             for (i in 1:length(Year)) {
             logit(prediction[i]) <- alpha + beta1 * Year[i] + beta2 * Year[i]^2
             }
 }",
select = c("C","N","Year*")
)

data(peregrine)
data <- peregrine

data$C <- data$R.Pairs
data$N <- data$Pairs

analysis <- jags_analysis (model, data, mode = "demo")
coef(analysis)

prediction <- predict(analysis, newdata = "Year")

gp <- ggplot(data = prediction, aes(x = Year, y = estimate))
gp <- gp + geom_line(data = data, aes(y = C/N), alpha = 1/3)
gp <- gp + geom_point(data = data, aes(y = C/N), shape = 1)
gp <- gp + geom_line()
gp <- gp + scale_y_continuous(name = "Proportion successful pairs", expand=c(0,0))
gp <- gp + expand_limits(y = c(0,1))

print(gp)

# Overdispersed Poisson GLMM analysis of peregrine breeding pairs (Kery & Schaub 2011 p.82-90)

# GLMM_Poisson (Kery & Schaub 2011 p.87)
model <- jags_model("
             model {
             alpha ~ dunif(-20, 20)
             beta1 ~ dunif(-10, 10)
             beta2 ~ dunif(-10, 10)
             beta3 ~ dunif(-10, 10)
             sd ~ dunif(0, 5)
             
             for (i in 1:length(Year)) {
             eps[i] ~ dnorm(0, sd^-2)
             eLogC[i] <- alpha + beta1 * Year[i]
             + beta2 * Year[i]^2 + beta3 * Year[i]^3
             log(eC[i]) <- eLogC[i] + eps[i]
             C[i] ~ dpois(eC[i])
             }
             }",
 derived_code = "model{
             for (i in 1:length(Year)) {
             log(prediction[i]) <- alpha + beta1 * Year[i]
             + beta2 * Year[i]^2 + beta3 * Year[i]^3
             }
 }",
random_effects = list(eps = "Year"),
select = c("C","Year*")
)

data(peregrine)
data <- peregrine

data$C <- data$Pairs
analysis <- jags_analysis (model, data, niter = 10^4, mode = "demo")
coef(analysis)
coef(analysis, parm = "random")
