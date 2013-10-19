
#' @title Create a JAGS simulation object
#'
#' @description 
#' Create a JAGS simulation object by generating data based on JAGS data model object and a set of values
#' 
#' @param data_model a \code{jags_data_model}.
#' @param values a data.frame of input values.
#' @param nreps an integer element indicating the number of datasets to generate for each set of input values.
#' @param mode a character element indicating the mode for the analysis.
#' @return a \code{jags_simulation} object
#' @seealso \code{\link{jags_data_model}} and \code{\link{jaggernaut}}
#' @examples
#' 
#' model <- jags_data_model("
#' data { 
#'  bLambda ~ dunif(3,6)
#'  for (i in 1:nx) { 
#'   x[i]~dpois(bLambda) 
#'  } 
#' }")
#' 
#' values <- data.frame(nx = c(2,10))
#' simulation <- jags_simulation (model, values, nreps = 5, mode = "demo")
#' 
#' print(simulation)
#' 
#' nreps(simulation)
#' nvalues(simulation)
#' 
#' simulation1 <- subset_jags(simulation, rep = 1:2)
#' data_jags(simulation1)
#'  
#' nreps(simulation1)
#' simulation1 <- update_jags(simulation1, nreps = 2)
#' nreps(simulation1)
#' 
#' simulation2 <- subset_jags(simulation, value = 2, rep = 1)
#' simulation <- add_jags(simulation1, simulation2)
#' nreps(simulation)
#' values(simulation)
#' nvalues(simulation)
#' 
#' simulation <- update_jags(simulation1, nreps = 2, values = data.frame(nx = c(25)))
#' nreps(simulation)
#' values(simulation)
#' nvalues(simulation)
#' 
#' @export
jags_simulation <- function (data_model, values, nreps = 100, mode = "current") {
    
  if (!is.jags_data_model(data_model)) 
    stop("data_model must be class jags_data_model")
     
  if(!is.numeric(nreps))
    stop("nreps must be class integer")
  
  if(!length(nreps) == 1)
    stop("nreps must be a single value")
  
  if(nreps < 1)
    stop("nreps must be positive")
  
  if (mode != "current") {
    old_opts <- opts_jagr(mode = mode)
    on.exit(opts_jagr(old_opts))
  }
  
  if(opts_jagr("mode") == "debug")
    nreps <- min(2,nreps)
  
  quiet <- opts_jagr("quiet")
  
  nreps <- as.integer(nreps)
    
  object <- list()
  class(object) <- "jags_simulation"
  
  data_model(object) <- data_model
  values(object) <- values
  
  try_data_jags <- function (data_model, values) {
    
    data <- try(data_jags(data_model, values))
                
    onetwothree <- c("one","two","three")
    
    retries <- 2
    while (inherits(data, "try-error")) {
      
      if(retries == 0)
        stop(paste("data simulation failed three times"))

      message(paste("data simulation failed",onetwothree[3-retries],"times"))
      
      retries <- retries - 1 
      
      data <- try(data_jags(data_model, values))
    }
    return (data)
  }
  
  data <- list()
          
  for (value in 1:nvalues(object)) {
    data[[value]] <- list()
    for (rep in 1:nreps) {
      data[[value]][[rep]] <- data_model
    }
    data[[value]] <- llply_jg(data[[value]], 
                              try_data_jags, 
                              values = values(object)[value,,drop = FALSE])
  }

  data_jags(object) <- data
  
  return (object)
}
