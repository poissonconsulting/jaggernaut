
binomjags <- function (n,s) {
  
  stopifnot(all(s <= n))
  
  old_opts <- opts_jagr()
  on.exit(opts_jagr(old_opts))
  
  model <- jags_model("model {
    for (i in 1:length(n)) {
      p[i] ~ dunif(0, 1)
      s[i] ~ dbin(p[i],n[i])
    }
  }",
  select_data = c("n","s"))
  
  data <- data.frame(n = n, s = s)
  
  opts_jagr(rhat = 1.05, resample = 3, nsamples = 1000, 
            quiet = TRUE, level = 0.95, nchains = 2)
  
  analysis <- jags_analysis(model, data)
  
  coef <- coef(analysis)
  
  return(round(coef[,c("estimate","lower","upper")],3))
}