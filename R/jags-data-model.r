
#' @title Create a JAGS data model
#'
#' @description 
#' Creates a JAGS model object which defines a Bayesian model
#' in the JAGS dialect of the BUGS language that can be used to simulate #' data.
#' In addition to defining the model
#' a JAGS model object can also specify the parameters to monitor, 
#' the variables to select, a function to manipulate the input data, 
#' and a function
#' to generate all (or some) of the initial values.
#' 
#' @param model_code a character element defining the model in the JAGS dialect of 
#' the BUGS language
#' @param monitor a character vector of the parameters to monitor
#' @param select a character vector of the variables to select from the 
#' input values
#' @param modify_data a function to modify the values being analysed
#' (after it has been converted to list form)
#' @param gen_inits a function to generate initial values for an MCMC chain
#' (it is passed the (modified) data in list form)
#' @details 
#' The \code{jags_data_model} function defines a JAGS data model that can then be passed to the 
#' \code{jags_simulation} function together with a data frame of values to perform a simulation.
#' The idea is that a JAGS model can be defined once and then used to
#' perform multiple simulations on different values. The only argument
#'  that 
#' needs to be set is a character element defining the model block in the JAGS dialect of the 
#' the BUGS language. However various other arguments can also be set 
#' to provide additional control. 
#' 
#' The \code{monitor} argument is used
#' to define the parameters in the model to monitor - by default all model parameters
#' are monitored except those that begin with the character d, e or i and are followed by an 
#' any upper case character, i.e., \code{eCount} would not be monitored while \code{bIntercept}
#' and \code{ecount} would. 
#' 
#' If \code{select} is \code{NULL} (the default) all variables in the
#' data frame are passed to the analysis.  If select is defined then only those
#' variables named in select are passed to the analysis. As a warning is given if a 
#' variable named in select is not in the data frame this can be useful for ensuring
#' all the required variables are present in the data frame.
#'  
#' Once the \code{select} argument has been applied to the the data, the data frame is converted into list form for input into JAGS. 
#' As well as each variable the list also contains a named element for each
#' factor that gives the number of levels of the factor. For example if the factor 
#' \code{Type} with three levels is present in the input data then an addition element
#' would be created in the list of data with the name \code{nType} and value three. This is useful
#' for iterating over factor levels in the JAGS model code. 
#' 
#' In some cases additional manipulations of the data may be required such as
#' conversion of variables into matrix or array form based on input factor levels.
#' This can be achieved by defining a function for the \code{modify_data} argument. The function
#' will be passed the data in list form and should return an updated list of the modified data. 
#' 
#' At this point initial values can be generated for one or more of the model parameters
#' using the \code{gen_inits} argument which expects a function that takes the list of data
#' and returns a list of the initial values - the function is called once for the single chain. 
#' @return a \code{jags_data_model} object
#' @references  
#' Plummer M (2012) JAGS Version 3.3.0 User Manual \url{http://sourceforge.net/projects/mcmc-jags/files/Manuals/}
#' @seealso \code{\link{jags_simulation}}, 
#' \code{\link{jags_power_analysis}} 
#' and \code{\link{jaggernaut}}    
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
#' values <- data.frame(nx = 10)
#' data_jags(model, values)
#'
#' @export 
jags_data_model <- function (model_code, monitor = NULL, select = NULL, 
                             modify_data = NULL, gen_inits = NULL,
                             extract_data = NULL) {  
  
  object <- jagr_model(model_code = model_code,
                       monitor = monitor, 
                       select = select,
                       modify_data = modify_data,
                       gen_inits = gen_inits)
    
  class(object) <- c("jags_data_model","jagr_model")
  
  extract_data(object) <- extract_data

  return (object)
}
