\docType{data}
\name{hm}
\alias{hm}
\title{House martin annual counts}
\format{A data frame with 20 rows and 2 columns}
\description{
  The house martin (\emph{Delichon urbica}) population
  annual counts from Magden (a small village in Northern
  Switzerland) collected Reto Freuler from 1990 to 2009
  from Kery & Schaub (2011 p.126). The variables are as
  follows: \itemize{ \item \code{hm} the count (integer).
  \item \code{year} the year (integer). }
}
\examples{
# State-space model for annual population counts

# ssm (Kery and Schaub 2011 p.127)
mod <- model("
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
     log(eC[i]) <- logN.est[year[i]]
   }
 }",
random = list(r = "year"),
select = c("C","year")
)

dat <- hm
pyears <- 6
dat <- data.frame(C = c(dat$hm,rep(NA,pyears)),
                 year = c(dat$year,max(dat$year+1):max(dat$year+pyears)))
dat$year <- factor(dat$year)

an <- analysis (mod, dat, n.iter = 10^5)

estimates(an,parameters = c("mean.r","sigma.obs","sigma.proc"))

exp <- derived(an, "eC", data = "year")

exp$Year <- as.integer(as.character(exp$year))
dat$Year <- as.integer(as.character(dat$year))

gp <- ggplot(data = exp, aes(x = Year, y = estimate))
gp <- gp + geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 1/4)
gp <- gp + geom_line(data = dat, aes(y = C), alpha = 1/3)
gp <- gp + geom_line()
gp <- gp + scale_y_continuous(name = "Population Size",expand = c(0,0))
gp <- gp + scale_x_continuous(name = "Year",expand = c(0,0))
gp <- gp + expand_limits(y = 0)

print(gp)

base <- data.frame(year = "2009")
dif <- derived(an, "eC", data = "year", base = base)
print(dif)
dif$Year <- as.integer(as.character(dif$year))
gp <- ggplot(data = dif, aes(x = Year, y = estimate))
gp <- gp + geom_hline(yintercept = 0, alpha = 1/3)
gp <- gp + geom_pointrange(aes(ymin = lower, ymax = upper))
gp <- gp + scale_y_continuous(name = "Population Change", labels = percent)
gp <- gp + scale_x_continuous(name = "Year")
gp <- gp + expand_limits(y = 0)
print(gp)
}
\references{
  Kery M & Schaub M (2011) Bayesian Population Analysis
  using WinBUGS. Academic Press.
  (\url{http://www.vogelwarte.ch/bpa})
}
\seealso{
  \code{\link{jaggernaut}}
}
\keyword{#'}
\keyword{datasets}

