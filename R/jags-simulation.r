
#' @title Create a JAGS simulation object
#'
#' @description 
#' Create a JAGS simulation object by generating data based on JAGS data model object and a set of values
#' 
#' @param data_model a \code{jags_data_model}.
#' @param values a data.frame of input values.
#' @param nrep an integer element indicating the number of datasets to generate for each set of input values.
#' @param mode a character element indicating the mode for the analysis.
#' @return a \code{jags_simulation} object
#' @seealso \code{\link{jags_data_model}} and \code{\link{jaggernaut}}
#' @examples
#' 
#' data_model <- jags_data_model("
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
#' values <- data.frame(nx = c(1,10), bIntercept = c(5,10))
#' 
#' simulation <- jags_simulation (data_model, values, nrep = 5)
#'
#' simulation1 <- subset_jags(simulation, value = 1, rep = 1:2)
#' simulation1 <- update_jags(simulation1, nrep = 2)
#' 
#' simulation2 <- subset_jags(simulation, value = 2, rep = 1)
#' 
#' #simulation <- add_jags(simulation1, simulation2)
#' 
#' data_jags(simulation)
#' data_jags(simulation, value = 1, rep = NULL)
#' data_jags(simulation, value = NULL, rep = 1)
#' data_jags(simulation, value = NULL, rep = NULL)
#' 
#' @export
jags_simulation <- function (data_model, values, nrep = 100, mode = "current") {
    
  if (!is.jags_data_model(data_model)) 
    stop("data_model must be class jags_data_model")
  
  if(!is.data.frame(values))
    stop ("values must be a data frame")
  
  if(nrow(values) == 0)
    stop ("values must have at least one row of data")
  
  if(ncol(values) == 0)
    stop ("values must have at least one column of data")
     
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
      
      data[[value]][[rep]] <- data_jags(data_model, values[value,,drop = FALSE])
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
