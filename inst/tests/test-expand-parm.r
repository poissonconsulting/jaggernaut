context("expand_parm")

test_that("expand_parm returns correct values", {
  
  mod <- jags_model("
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
  dat <- peregrine

  dat$C <- dat$Pairs
  an <- jags_analysis (mod, dat, niter = 10^4, mode = "test")
    
  expect_that(expand_parm(analysis(an)), equals(c("alpha","beta1","beta2","beta3","eps","sd")))
  expect_that(expand_parm(analysis(an),"fixed"), equals(c("alpha","beta1","beta2","beta3","sd")))
  expect_that(expand_parm(analysis(an),"all"), equals(c("alpha","beta1","beta2","beta3","eps","sd")))  
  expect_that(expand_parm(analysis(an),"random"), equals("eps"))
  expect_that(expand_parm(analysis(an), c("random","alpha")), equals(c("alpha","eps")))              
})
