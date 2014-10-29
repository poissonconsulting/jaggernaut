context("expand_parm")

test_that("expand_parm returns correct values", {
  
  mod <- jags_model("
                    model {
                    alpha ~ dunif(-20, 20)
                    beta1 ~ dunif(-10, 10)
                    beta2 ~ dunif(-10, 10)
                    beta3 ~ dunif(-10, 10)
                    sigma ~ dunif(0, 5)
                    
                    for (i in 1:length(Year)) {
                    eps[i] ~ dnorm(0, sigma^-2)
                    eLogC[i] <- alpha + beta1 * Year[i]
                    + beta2 * Year[i]^2 + beta3 * Year[i]^3
                    log(eC[i]) <- eLogC[i] + eps[i]
                    C[i] ~ dpois(eC[i])
                    }
                    }",
 derived_code = "data{
                    for (i in 1:length(Year)) {
                    log(prediction[i]) <- alpha + beta1 * Year[i]
                    + beta2 * Year[i]^2 + beta3 * Year[i]^3
                    }
 }",
  random_effects = list(eps = "Year"),
  select_data = c("C","Year*")
  )

  data(peregrine)
  dat <- peregrine

  dat$C <- dat$Pairs
  an <- jags_analysis (mod, dat, niters = 10^3, mode = "test")
    
  expect_that(expand_parm(chains(analysis(an)),"all"), equals(sort(c("alpha","beta1","beta2","beta3",paste0("eps[",1:40,"]"),"sigma"))))
  expect_that(expand_parm(chains(analysis(an)),"fixed"), equals(c("alpha","beta1","beta2","beta3","sigma")))
  expect_that(expand_parm(chains(analysis(an)),"random"), equals(sort(paste0("eps[",1:40,"]"))))
  expect_that(expand_parm(chains(analysis(an)), c("random","alpha")), equals(sort(c("alpha",paste0("eps[",1:40,"]")))))   
})
