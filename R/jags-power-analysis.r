#' @title Perform a JAGS power analysis
#'
#' @description 
#' Performs a JAGS power analysis. 
#' 
#' @param model a \code{jags_model}.
#' @param data_model a \code{jags_data_model}.
#' @param values a data.frame of input values.
#' @param nreps an integer element indicating the number of datasets to generate for each set of input values.
#' @param niters an integer element indicating the number of iterations.
#' @param mode a character element indicating the mode for the analysis.
#' @return a \code{jags_power_analysis} object
#' @references 
#' Plummer M (2012) JAGS Version 3.3.0 User Manual \url{http://sourceforge.net/projects/mcmc-jags/files/Manuals/}
#' @seealso \code{\link{jags_data_model}}, \code{\link{jags_model}} and \code{\link{jaggernaut}}
#' @examples
#'  
#' data_model <- jags_data_model("data {
#'  for (gp in 1:nGroup) {
#'   bGroup[gp] ~ dnorm(0, sGroup^-2)
#'   for (i in 1:nSample) {
#'     z[gp, i] ~ dbern(bProp)
#'     y[gp, i] ~ dnorm(bGroup[gp] + bEffect * z[gp, i],sSample^-2)
#'   }
#'  }
#' } ",
#'  extract_data = function (data) {
#'  data$bGroup <- NULL
#'  data$sGroup <- NULL
#'  data$sSample <- NULL
#'  data$bEffect <- NULL
#'  data$bProp <- NULL
#'  return (data)
#' })
#'
#' values <- expand.grid(nGroup = 10,
#' sGroup = 0.1,
#' nSample = 10,
#' sSample = 0.1,
#' bEffect = c(0,0.01),
#' bProp = 0.5)
#'
#' model <- jags_model("model {
#'  
#' sGroup ~ dunif(0,1)
#' sSample ~ dunif(0,1)
#' bEffect ~ dnorm(0, 1^-2)
#' bProp ~ dunif(0, 1)
  
#' for (gp in 1:nGroup) {
#'  bGroup[gp] ~ dnorm(0, sGroup^-2)
#'  for (i in 1:nSample) {
#'    z[gp, i] ~ dbern(bProp)
#'    y[gp, i] ~ dnorm(bGroup[gp] + bEffect * z[gp, i],sSample^-2)
#'  }
#'}
#'}",
#'random_effects = list(bGroup = NULL))
#' 
#' power <- jags_power_analysis(model, data_model, values, nreps = 5, mode = "demo") 
#'
#' print(power)    
#' 
#' \dontshow{
#'                           
#' nvalues(power)
#' nreps(power)
#' nchains(power)
#' niters(power)
#' nsims(power)
#' }
#' 
#' rhat(power)
#' is_converged(power, percent = TRUE)
#' 
#' values$bEffect <- c(0.025,0.05)
#' 
#' power <- update(power, nreps = 10, values = values)
#' rhat(power)
#' opts <- opts_jagr()
#' opts_jagr(rhat = 1.05)
#' power <- update(power, mode = "current")
#' opts_jagr(opts)
#' rhat(power) 
#' 
#' values$bEffect <- c(0.1, 0.2)
#' power2 <- jags_power_analysis(model, data_model, values, nreps = 10, mode = "demo") 
#'  power <- combine(power, power2)
#' 
#' coef(power, parm = "bEffect", combine = TRUE)
#' 
#' power_jags(power, parm = c(bGroup = 0))
#' power_jags(power, parm = c(bEffect = 0))
#' 
#' @export
jags_power_analysis <- function (model, data_model, values, nreps = 100, 
                                 niters = 10^3, mode = "current") {
  
  if(!is.jags_model(model) && !is_one_model(model))
    stop("model must be a jags_model with a single model")
    
  if (mode != "current") {
    old_opts <- opts_jagr(mode = mode)
    on.exit(opts_jagr(old_opts))
  }
  
  if(opts_jagr("mode") == "debug")
    nreps <- min(2,nreps)
  
  quiet <- opts_jagr("quiet")
  
  if (!quiet)
    cat("\nGenerating Data\n")
  
  object <- jags_simulation(data_model, values = values, nreps = nreps)

  if (!quiet)
    cat("\nAnalysing Data\n")
    
  analyses <- llply_jg(dataset(object), jags_analysis_datafirst, model = model,
                       niters = niters, .recursive = 2)
  
  as.jagr_power_analysis_analysis <- function (object) {
    stopifnot(is.jags_analysis(object))
    return (as.jagr_power_analysis(analysis(object)))
  }
  
  analyses <- llply_jg(analyses, as.jagr_power_analysis_analysis, .recursive = 2)

  class(object) <- c("jags_power_analysis","jags_simulation")
  
  model(object) <- model
  rhat_threshold(object) <- opts_jagr("rhat")
  analyses(object) <- analyses
  
  return (object)
}
