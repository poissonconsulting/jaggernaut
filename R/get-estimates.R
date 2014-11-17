get_estimates <- function (x, level, estimate) {
  
  assert_that(is.numeric(x) && noNA(x))
  assert_that(is.number(level))
  assert_that(is.string(estimate) && estimate %in% c("mean", "median"))
  
  estimate <- ifelse(estimate == "mean", mean(x), median(x))
  sd <- sd(x)
  
  lower <- (1 - level) / 2
  upper <- level + lower

  lower <- quantile(x, lower)
  upper <- quantile(x, upper)
  
  bound <- upper - lower
    
  error <- bound / 2 / abs(estimate)
  error <- round(error * 100)
  
  n <- length(x)
  
  digits <- round(log(bound^-1, base = 10)) + ceiling(log(n + 1, base = 10))
  
  estimate <- round(estimate, digits = digits)
  lower <- round(lower, digits = digits)
  upper <- round(upper, digits = digits)
  sd <- round(sd, digits = digits)
  
  significance <- significance(x)
    
  estimates <- c(estimate, lower, upper, sd, error, significance)
  names(estimates) <- c("estimate", "lower", "upper", "sd", "error", "significance")
  estimates
}
