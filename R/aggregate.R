#' Aggregate jags_analysis iterations
#' 
#' Runs aggregation code for jags_analysis to aggregate
#' iterations - used for posterior predictive checking.
#' 
#' @param x jags_analysis
#' @param model a count or string specifying the jags model to select. 
#' @param data data.frame to aggregate over
#' @param aggregation_code string of aggregation_code to replace analyse's aggregation code
#' @param ... arguments passed to nowhere
#' @method aggregate jags_analysis 
aggregate.jags_analysis <- function (x, model = 1, data = dataset(x),
  aggregation_code = aggregation_code(x), ...) {
  
  assert_that(is.count(model) || is.string(model))  
  assert_that(is.string(aggregation_code))
  
  x <- subset_one(x, model = model)
  aggregation_code(x) <- aggregation_code
  
  x <- jaggregate (x, data = data) 
  ags_sample(x)
  # needs work - see jags_discrepancies
}
