#' Aggregate jags_analysis iterations
#' 
#' Runs aggregation code for jags_analysis to aggregate
#' iterations - used for posterior predictive checking.
#' 
#' @param x jags_analysis
#' @param model_number integer scalar of model number
#' @param data data.frame to aggregate over
#' @param aggregation_code string of aggregation_code to replace analyse's aggregation code
#' @param ... arguments passed to nowhere
#' @method aggregate jags_analysis 
aggregate.jags_analysis <- function (x, model_number = 1, data = dataset(x),
  aggregation_code = aggregation_code(x), ...) {
  
  object <- x
  rm(x)
  assert_that(is.count(model_number))
  assert_that(is.string(aggregation_code))
  
  object <- subset(object, model_number = model_number)
  aggregation_code(object) <- aggregation_code
  
  agg <- jaggregate (object, data = data) 
  agg <- ags_sample(agg)
  agg
  # needs work - see jags_discrepancies
}
