
library(ggplot2)
library(scales)

# State-space model for annual population counts

# ssm (Kery and Schaub 2011 p.127)
mod <- jags_model("
             model {
             logN.est[1] ~ dnorm(5.6, 10^-2)
             mean.r ~ dnorm(1, 10^-2)
             sigma.proc ~ dunif(0, 1)
             sigma.obs ~ dunif(0, 1)
             
             for (yr in 1:nyear) {
             r[yr] ~ dnorm(mean.r, sigma.proc^-2)
             logN.est[yr+1] <- logN.est[yr] + r[yr]
             }
             
             for (i in 1:nrow) {
             C[i] ~ dlnorm(logN.est[year[i]], sigma.obs^-2)
             }
             }",
 derived_code = "model{
             for (i in 1:nrow) {
             log(prediction[i]) <- logN.est[year[i]]
             }
 }",
random_effects = list(r = "year", logN.est = "year"),
select = c("C","year")
)

data(hm)

dat <- hm
pyears <- 6
C <- c(dat$hm,rep(NA,pyears))
year <- c(dat$year,max(dat$year+1):max(dat$year+pyears))
dat <- data.frame(C = C, year = year)

dat$year <- factor(dat$year)

an <- jags_analysis (mod, dat, niter = 10^5, mode = "default")

coef(an, parm = c("mean.r","sigma.obs","sigma.proc"))

pred <- predict(an, newdata = "year")

pred$Year <- as.integer(as.character(pred$year))
dat$Year <- as.integer(as.character(dat$year))

gp <- ggplot(data = pred, aes(x = Year, y = estimate))
gp <- gp + geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 1/4)
gp <- gp + geom_line(data = na.omit(dat), aes(y = C), alpha = 1/3)
gp <- gp + geom_line()
gp <- gp + scale_y_continuous(name = "Population Size",expand = c(0,0))
gp <- gp + scale_x_continuous(name = "Year",expand = c(0,0))
gp <- gp + expand_limits(y = 0)

print(gp)

base <- data.frame(year = "2009")
pred <- predict(an, newdata = "year", base = base)
print(pred)

pred$Year <- as.integer(as.character(pred$year))

gp <- ggplot(data = pred, aes(x = Year, y = estimate))
gp <- gp + geom_hline(yintercept = 0, alpha = 1/3)
gp <- gp + geom_pointrange(aes(ymin = lower, ymax = upper))
gp <- gp + scale_y_continuous(name = "Population Change", labels = percent)
gp <- gp + scale_x_continuous(name = "Year")
gp <- gp + expand_limits(y = 0)

print(gp)
