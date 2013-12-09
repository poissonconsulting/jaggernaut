# Poisson GLM analyses of coal tit counts (Kery & Schaub 2011 p.95-110)

library(reshape2) 
library(ggplot2)
library(scales) 

# Whereas Kery & Schaub (2011) pass the counts, observer codes and
# first time observer indicators as site x year matrices in the following
# code they are melted into long format and passed as a single data frame.
# Also in the following code the order of the models is reversed with the
# full model being the first model. The indicator first is passed as a logical
# vector to ensure it is treated correctly by the predict function.

# GLMM5 (Kery & Schaub 2011 p.108-109)
model1 <- jags_model ("
              model {
              mu ~ dnorm(0, 10^-2)
              beta1 ~ dnorm(0, 10^-2)
              beta2 ~ dnorm(0, 10^-2)
              
              sd.alpha ~ dunif(0, 3)
              for (j in 1:nsite) {
              alpha[j] ~ dnorm (0, sd.alpha^-2)
              }
              
              sd.eps ~ dunif(0, 1)
              for (i in 1:nyearFac) {
              eps[i] ~ dnorm (0, sd.eps^-2)
              }
              
              sd.gamma ~ dunif(0, 1)
              for (i in 1:nobs) {
              gamma[i] ~ dnorm (0, sd.gamma^-2)
              }
              
              for (i in 1:nrow) {
              log(eC[i]) <- mu + beta1 * year[i] + beta2 * first[i]
                + alpha[site[i]] + gamma[obs[i]] + eps[yearFac[i]]
              C[i] ~ dpois(eC[i])
              }
              }",
 derived_code = "model{
              for (i in 1:nrow) {
              log(prediction[i]) <- mu + beta1 * year[i] + beta2 * first[i]
                + alpha[site[i]] + gamma[obs[i]] + eps[yearFac[i]]
              }
 }",
random_effects = list(alpha = "site", eps = "nyearFac", gamma = "obs"),
select = c("C","site","year*","yearFac","obs","first")
)

# GLMM4 (Kery & Schaub 2011 p.106-107)
model2 <- jags_model("
              model {
              mu ~ dnorm(0, 10^-2)
              beta1 ~ dnorm(0, 10^-2)
              beta2 ~ dnorm(0, 10^-2)
              
              sd.alpha ~ dunif(0, 3)
              for (j in 1:nsite) {
              alpha[j] ~ dnorm (0, sd.alpha^-2)
              }
              
              sd.eps ~ dunif(0, 1)
              for (i in 1:nyearFac) {
              eps[i] ~ dnorm (0, sd.eps^-2)
              }
              
              for (i in 1:nrow) {
              log(eC[i]) <- mu + beta1 * year[i] + beta2 * first[i]
                + alpha[site[i]] + eps[yearFac[i]]
              C[i] ~ dpois(eC[i])
              }
              }",
 derived_code = "model{
              for (i in 1:nrow) {
              log(prediction[i]) <- mu + beta1 * year[i] + beta2 * first[i]
                + alpha[site[i]] + eps[yearFac[i]]
              }
 }",
random_effects = list(alpha = "site", eps = "nyearFac"),
select = c("C","site","year*","yearFac","first")
)

# GLMM3 (Kery & Schaub 2011 p.105)
model3 <- jags_model("
              model {
              mu ~ dnorm(0, 10^-2)
              beta2 ~ dnorm(0, 10^-2)
              
              sd.alpha ~ dunif(0, 3)
              for (j in 1:nsite) {
              alpha[j] ~ dnorm (0, sd.alpha^-2)
              }
              
              sd.eps ~ dunif(0, 1)
              for (i in 1:nyearFac) {
              eps[i] ~ dnorm (0, sd.eps^-2)
              }
              
              for (i in 1:nrow) {
              log(eC[i]) <- mu + beta2 * first[i]
              + alpha[site[i]] + eps[yearFac[i]]
              C[i] ~ dpois(eC[i])
              }
              }",
 derived_code = "model{
              for (i in 1:nrow) {
              log(prediction[i]) <- mu + beta2 * first[i]
                + alpha[site[i]] + eps[yearFac[i]]
              }
 }",
random_effects = list(alpha = "site", eps = "nyearFac"),
select = c("C","site","yearFac","first")
)

# GLMM2 (Kery & Schaub 2011 p.103-104)
model4 <- jags_model("
              model {
              mu ~ dnorm(0, 10^-2)
              
              sd.alpha ~ dunif(0, 3)
              for (j in 1:nsite) {
              alpha[j] ~ dnorm (0, sd.alpha^-2)
              }
              
              sd.eps ~ dunif(0, 1)
              for (i in 1:nyearFac) {
              eps[i] ~ dnorm (0, sd.eps^-2)
              }
              
              for (i in 1:nrow) {
              log(eC[i]) <- mu + alpha[site[i]] + eps[yearFac[i]]
              C[i] ~ dpois(eC[i])
              }
              }",
 derived_code = "model{
              for (i in 1:nrow) {
              log(prediction[i]) <- mu + alpha[site[i]] + eps[yearFac[i]]
              }
 }",
random_effects = list(alpha = "site", eps = "nyearFac"),
select = c("C","site","yearFac")
)

# GLMM1 (Kery & Schaub 2011 p.102)
model5 <- jags_model("
              model {
              mu ~ dnorm(0, 10^-2)
              
              sd.alpha ~ dunif(0, 3)
              for (j in 1:nsite) {
              alpha[j] ~ dnorm (0, sd.alpha^-2)
              }
              
              for (i in 1:nrow) {
              log(eC[i]) <- mu + alpha[site[i]]
              C[i] ~ dpois(eC[i])
              }
              }",
 derived_code = "model{
              for (i in 1:nrow) {
              log(prediction[i]) <- mu + alpha[site[i]]
              }
 }",
random_effects = list(alpha = "site"),
select = c("C","site")
)

#' # GLM0 (Kery & Schaub 2011 p.102)
model6 <- jags_model ("
              model {
              mu ~ dnorm(0, 10^-2)
              
              for (i in 1:nrow) {
              log(eC[i]) <- mu
              C[i] ~ dpois(eC[i])
              }
              }",
 derived_code = "model{
              for (i in 1:nrow) {
              log(prediction[i]) <- mu
              }
 }",
select = c("C")
)

models <- list(model1, model2, model3, model4, model5, model6)

data(tits)
data <- tits

year <- subset(data, select=c("site",paste0("y",1999:2007)))
obs <- subset(data, select=c("site",paste0("obs",1999:2007)))
first <- subset(data, select=c("site",paste0("first",1999:2007)))

year <- melt(year, id.vars = "site", variable.name = "year", value.name = "C")
obs <- melt(obs, id.vars = "site", variable.name = "year", value.name = "obs")
first <- melt(first, id.vars = "site", variable.name = "year",
              value.name = "first")

year$year <- as.integer(substr(as.character(year$year),2,5))
obs$year <- as.integer(substr(as.character(obs$year),4,7))
first$year <- as.integer(substr(as.character(first$year),6,9))

data <- merge(year, obs, by = c("site","year"))
data <- merge(data, first, by = c("site","year"))

data$obs[is.na(data$obs)] <- 272
data$first[is.na(data$first)] <- 0
data$first <- as.logical(data$first)

data$yearFac <- factor(data$year)
data$obs <- factor(data$obs)

gp <- ggplot(data = na.omit(data), aes(x = year, y = C))
gp <- gp + geom_line(aes(group = site, color = site))
gp <- gp + scale_y_continuous(name = "Territory count")
gp <- gp + scale_x_continuous(name = "Year", breaks = seq(2000,2006,by=2))
gp <- gp + expand_limits(y = 0)
gp <- gp + theme(legend.position = "none")

print(gp)

analysis <- jags_analysis (models, data, niter = 10^4, mode = "demo")

summary(analysis)

prediction <- predict(analysis, newdata = "site")

gp <- ggplot(data = prediction, aes(x = site, y = estimate))
gp <- gp + geom_pointrange(aes(ymin = lower, ymax = upper))
gp <- gp + scale_y_continuous(name = "Territory count", expand = c(0,0))
gp <- gp + scale_x_discrete(name = "Territory", breaks = NULL)
gp <- gp + expand_limits(y = 0)

print(gp)

prediction <- predict(analysis, newdata = "site", model_number = 2)

gp <- gp %+% prediction

print(gp)
