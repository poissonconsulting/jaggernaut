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
    
  expect_equal(expand_parm(analysis(an),"all"), sort(c("alpha","beta1","beta2","beta3",paste0("eps[",1:40,"]"),"sigma")))
  expect_equal(expand_parm(analysis(an),"fixed"), c("alpha","beta1","beta2","beta3","sigma"))
  expect_equal(expand_parm(analysis(an),"random"), sort(paste0("eps[",1:40,"]")))
  expect_equal(expand_parm(analysis(an), c("random","alpha")), sort(c("alpha",paste0("eps[",1:40,"]"))))   
})

test_that("expand_parm returns correct values", {
  
  mod <- jags_model("
                    model {
                    alpha ~ dunif(-20, 20)
                    beta[1] ~ dunif(-10, 10)
                    beta[2] ~ dunif(-10, 10)
                    beta[3] ~ dunif(-10, 10)
                    sigma ~ dunif(0, 5)
                    
                    for (i in 1:length(Year)) {
                    eps[i] ~ dnorm(0, sigma^-2)
                    eLogC[i] <- alpha + beta[1] * Year[i]
                    + beta[2] * Year[i]^2 + beta[3] * Year[i]^3
                    log(eC[i]) <- eLogC[i] + eps[i]
                    C[i] ~ dpois(eC[i])
                    }
                    }",
 derived_code = "data{
 for (i in 1:length(Year)) {
 prediction[i] <- sigma
 }
 }",
  random_effects = list(eps = "Year"),
 monitor = c("beta", "sigma-"),
 select_data = c("C","Year*")
  )
 
 data(peregrine)
 dat <- peregrine
 
 dat$C <- dat$Pairs
 an <- jags_analysis (mod, dat, niters = 10^3, mode = "test")

 expect_equal(monitor(an), c("beta", "sigma-"))
 expect_equal(rownames(coef(an)), c("beta[1]", "beta[2]", "beta[3]"))
 expect_equal(rownames(coef(an, parm = "sigma")), c("sigma"))
 expect_equal(rownames(coef(an, parm = c("all", "sigma"))), c("beta[1]", "beta[2]", "beta[3]", "sigma"))

 expect_equal(rownames(convergence(an, combine = FALSE)), c("beta[1]", "beta[2]", "beta[3]"))
 expect_equal(rownames(convergence(an, parm = "sigma", combine = FALSE)), c("sigma"))
 expect_equal(rownames(convergence(an, parm = c("all", "sigma"), combine = FALSE)), c("beta[1]", "beta[2]", "beta[3]", "sigma"))
 
 expect_equal(colnames(auto_corr(an)), c("beta[1]", "beta[2]", "beta[3]"))
 expect_equal(colnames(auto_corr(an, parm = "sigma")), c("sigma"))
 expect_equal(colnames(auto_corr(an, parm = c("sigma", "all"))), c("beta[1]", "beta[2]", "beta[3]", "sigma"))  

expect_equal(rownames(cross_corr(an)), c("beta[1]", "beta[2]", "beta[3]"))
expect_equal(rownames(cross_corr(an, parm = "sigma")), c("sigma"))
expect_equal(rownames(cross_corr(an, parm = c("all", "sigma"))), c("beta[1]", "beta[2]", "beta[3]", "sigma"))

expect_equal(predict(an, newdata = "")$estimate, coef(an, parm = "sigma")$estimate)
})
