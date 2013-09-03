
#' @title Perform a JAGS simulation
#'
#' @description 
#' Performs a JAGS simulation by using a 
#' \code{jags_data_model} and a data frame of values  
#' to generate a simulation data frame using JAGS (Plummer 2012). 
#' 
#' @param model a \code{jags_data)model}.
#' @param values a data.frame of values.
#' @param quiet a logical element indicating whether to print updates.
#' @return a \code{jags_simulation} object
#' @references 
#' Plummer M (2012) JAGS Version 3.3.0 User Manual \url{http://sourceforge.net/projects/mcmc-jags/files/Manuals/}
#' @seealso \code{\link{jags_data_model}} and \code{\link{jaggernaut}}
#' @examples
#' 
#' mod <- jags_data_model("
#' data { 
#'  for (i in 1:nx) { 
#'    x[i] ~ dpois(bIntercept) 
#'    for (j in 1:nx) {
#'      y[i,j] ~ dpois(bIntercept) 
#'    }
#'  } 
#'  z <- bIntercept
#'}    
#' ")
#'
#' val <- data.frame(nx = 10, bIntercept = 10)
#' 
#' sim <- jags_simulation (mod, val, nrep = 100, quiet = FALSE)
#' 
#' @export
jags_simulation <- function (data_model, values, nrep = 1, quiet = opts_jagr("quiet")) {
  
  model <- data_model
  
  if (!is.jags_data_model(model)) 
    stop("model must be class jags_model")
  
  if(!(is.data.frame(values) && nrow(values) == 1))
    stop ("values must be a data frame with one row of parameter values")

  nrep <- as.integer(nrep)
  if(!is.numeric(nrep))
    stop("quiet must be class integer")
  
  if(!length(nrep) == 1)
    stop("nrep must be a single value")
  
  if(nrep < 1)
    stop("nrep must be positive")
  
  if(!is.logical(quiet))
    stop("quiet must be class logical")
  
  if(!length(quiet) == 1)
    stop("quiet must be a single value")
  
  if(is.na(quiet) == 1)
    stop("quiet must be TRUE or FALSE")
  
  if(!"basemod" %in% list.modules())
    load.module("basemod")  
  
  if(!"bugs" %in% list.modules())
    load.module("bugs")
  
  data <- list()
  for (i in 1:nrep) {
    if (!quiet)
      print(paste("Rep:",i))
    
    object <- jagr_simulation(model = model, data = values, quiet = quiet)

    est <- calc_estimates(object)
    
    est <- est[rownames(est) != "deviance",]
    
    data[[i]] <- extract_estimates(est)[["estimate"]]
  }
  
  object$simulated <- data
  
  class(object) <- "jags_simulation"
  
  return (object)
}

#' ## jags_simulation()
#' 
#' ## so need option to have multiple rows in values
#' ## need to take quiet from mode
#' ## note debug mode different to quiet i.e. if in debug mode proper
#' ## then show model fitting information plus in debug mode
#' ## nrep = 1 maybe set nrep in mode?
#' ## once fitted need ability to pull out data lists by values and rep 
#' ## and also variable names some function like 
#' 
#' ## data_set(sim, value = NULL, rep = NULL, variables = NULL)
#' 
#' ## note I think jags_data_models no conversion in select i.e. (*+
#' ## also only allow integer and numeric
#' ## enhancement possibility to specify type and range for things 
#' ## in select
#' ## could be used to automatically generate values
#' ## get this sorted so useful for other programs in and of itself
#' 
#' ## then write 
#' 
#' ## jags_power (...,niter = 10^3) 
#' 
#' #' ## note if not converged then keeps but records that not conv
#' # option to update number of iter if use then update those that not 
#' # converged..
#' ## applies jags_model to each simulated data and gets parameter
#' ## key requirement to add reps so that do in stages
#' 
#' ## update(power, nrep = 1, values = NULL, niter = NULL)
#' 
#' ## finally ability to add power analyses 
#' 
#' ## add(power1, power2)
#' 
#' ## must have same jags_data_model and if not same number of reps
#' ## will update shorter one finally if shared values will drop
#' ## value from power with less reps or second power if same number
#' ## of reps also if different. also update niter so same (only update
#' unconverged iterations...)
#' 
#' # finally need ability to query out power different parameters

#' power(power1)
