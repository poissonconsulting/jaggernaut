# State-space model for annual population counts

library(ggplot2)
library(scales)

# ssm (Kery and Schaub 2011 p.127)
model <- jags_model("
             model {
             logN.est[1] ~ dnorm(5.6, 10^-2)
             mean.r ~ dnorm(1, 10^-2)
             sigma.proc ~ dunif(0, 1)
             sigma.obs ~ dunif(0, 1)
             
             for (yr in 1:nyear) {
             r[yr] ~ dnorm(mean.r, sigma.proc^-2)
             logN.est[yr+1] <- logN.est[yr] + r[yr]
             }
             
             for (i in 1:length(year)) {
             C[i] ~ dlnorm(logN.est[year[i]], sigma.obs^-2)
             }
             }",
 derived_code = "model{
             for (i in 1:length(year)) {
             log(prediction[i]) <- logN.est[year[i]]
             }
 }",
random_effects = list(r = "year", logN.est = "year"),
select_data = c("C","year")
)

data(hm)

data <- hm
pyears <- 6
C <- c(data$hm,rep(NA,pyears))
year <- c(data$year,max(data$year+1):max(data$year+pyears))
data <- data.frame(C = C, year = year)

data$year <- factor(data$year)

analysis <- jags_analysis (model, data, niters = 10^5, mode = "demo")

coef(analysis, parm = c("mean.r","sigma.obs","sigma.proc"))

prediction <- predict(analysis, newdata = "year")

prediction$Year <- as.integer(as.character(prediction$year))
data$Year <- as.integer(as.character(data$year))

gp <- ggplot(data = prediction, aes(x = Year, y = estimate))
gp <- gp + geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 1/4)
gp <- gp + geom_line(data = na.omit(data), aes(y = C), alpha = 1/3)
gp <- gp + geom_line()
gp <- gp + scale_y_continuous(name = "Population Size",expand = c(0,0))
gp <- gp + scale_x_continuous(name = "Year",expand = c(0,0))
gp <- gp + expand_limits(y = 0)

print(gp)

base <- data.frame(year = "2009")
prediction <- predict(analysis, newdata = "year", base = base)
print(prediction)

prediction$Year <- as.integer(as.character(prediction$year))

gp <- ggplot(data = prediction, aes(x = Year, y = estimate))
gp <- gp + geom_hline(yintercept = 0, alpha = 1/3)
gp <- gp + geom_pointrange(aes(ymin = lower, ymax = upper))
gp <- gp + scale_y_continuous(name = "Population Change", labels = percent)
gp <- gp + scale_x_continuous(name = "Year")
gp <- gp + expand_limits(y = 0)

print(gp)
