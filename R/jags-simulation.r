
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
#' val <- data.frame(nx = c(1,10), bIntercept = c(5,10))
#' 
#' sim <- jags_simulation (mod, val, nrep = 5)
#'
#' sim1 <- subset_jags(sim, value = 1, rep = 1:2)
#' sim1 <- update_jags(sim1, nrep = 2)
#' 
#' sim2 <- subset_jags(sim, value = 2, rep = 1)
#' 
#' sims <- add_jags(sim1, sim2)
#' 
#' dataset(sims, variables = c("x","z"))
#' 
#' @export
jags_simulation <- function (data_model, nrep = 1, values = NULL, mode = "current") {
    
  if (!is.jags_data_model(data_model)) 
    stop("data_model must be class jags_data_model")
  
  if(!is.null(values)) {
    if(!is.data.frame(values))
      stop ("values must be NULL or a data frame")
    
    if(nrow(values) == 0)
      stop ("values must have at least one row of data")
    
    if(ncol(values) == 0)
      stop ("values must have at least one column of data")
  }
     
  if(!is.numeric(nrep))
    stop("nrep must be class integer")
  
  if(!length(nrep) == 1)
    stop("nrep must be a single value")
  
  if(nrep < 1)
    stop("nrep must be positive")
  
  nrep <- as.integer(nrep)
  
  if(!"basemod" %in% list.modules())
    load.module("basemod")  
  
  if(!"bugs" %in% list.modules())
    load.module("bugs")
  
  old_opts <- opts_jagr(mode = mode)
  on.exit(opts_jagr(old_opts))
  
  data <- list()
          
  nvalues <- nrow(values)
  for (value in 1:nvalues) {
    data[[value]] <- list()
    for (rep in 1:nrep) {
      if (!opts_jagr("quiet"))
        print(paste0("Value: ",value," of ",nvalues,"  Rep: ", rep," of ",nrep))
      
      object <- jagr_simulation(model = data_model, 
                                data = values[value,,drop = FALSE], 
                                quiet = opts_jagr("mode") != "debug")
      
      est <- calc_estimates(object)
      
      est <- est[rownames(est) != "deviance",]
      
      data[[value]][[rep]] <- extract_estimates(est)[["estimate"]]
    }
  }
  
  object <- list(
    data_model = data_model, 
    values = values,
    nvalues = nrow(values),
    nrep = nrep,
    simulated = data
  )
  
  class(object) <- "jags_simulation"
  
  return (object)
}
