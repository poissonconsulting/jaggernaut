context("trees")

model <- jmodel("model{ 
  sVolume ~ dunif(0, 5)
                bIntercept ~ dnorm(0, 5^-2)
                bSlope ~ dnorm(0, 5^-2)
                
                for (i in 1:nrow){ 
                eLogVolume[i] <- bIntercept + bSlope * LogHeight[i]
                Volume[i] ~ dlnorm(eLogVolume[i],sVolume^-2) 
                } 
                }",
derived = "model{
                for (i in 1:nrow){ 
                log(eVolume[i]) <- bIntercept + bSlope * LogHeight[i]
                } 
}",
select = c("log(Height)*","Volume"))

analysis <- janalysis (model, data = datasets::trees, quiet = T)
  
test_that("analysis of trees completes", {

  expect_that(analysis, is_a("janalysis"))
})

test_that("derived returns correct object", {
  
  expected <- derived(analysis, data = "Height", parameter = "eVolume")

  expect_that(expected, is_a("data.frame"))
  expect_equal(nrow(expected), 30)
  expect_equal(colnames(expected), c("Girth","Height","Volume","estimate","lower","upper","error","significance"))
  expect_equal(round(expected$Girth[1],5), 13.24839)
  expect_equal(round(expected$Volume[1],5), 30.17097)
  expect_equal(round(expected$Height[1],5), 63.00000)
  expect_equal(round(expected$Height[30],5), 87.00000)
})
