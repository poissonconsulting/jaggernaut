
#' @title Create a JAGS model
#'
#' @description 
#' Creates a JAGS model (\code{jmodel} object) which defines a Bayesian model
#' in the JAGS dialect of the BUGS language.  In addition to defining the model
#' a JAGS model object can also specify the parameters to monitor, 
#' the variables to select, a function to manipulate the input data, a function
#' to generate all (or some) of the initial values and JAGS code
#' to extract derived values from the final model among other things.
#' 
#' @param model a character element defining the model in the JAGS dialect of 
#' the BUGS language
#' @param monitor a character vector of the parameters to monitor
#' @param select a character vector of the variables to select from the 
#' data set being analysed (can also specify variables to transform and/or centre)
#' @param modify_data a function to modify the data set being analysed
#' (after it has been converted to list form)
#' @param gen_inits a function to generate initial values for an MCMC chain
#' (it is passed the (modified) data in list form)
#' @param derived_model a character element defining a model in the JAGS dialect of 
#' the BUGS language that specifies derived parameters
#' @param random a named list of parameters to be treated as random effects with the
#' related data as values
#' @param extract_data a function to convert the parameter estimates into a data set
#'  (after they have been converted to list form)
#' @param description a named character vector descriping each of the parameters 
#' in the model where the name indicates the parameter to which the description applies
#' @details 
#' The \code{jmodel} function defines a JAGS model that can then be passed to the 
#' \code{\link{janalysis}} function together with a data frame to perform a Bayesian analysis.
#' The idea is that a JAGS model can be defined once and then used to perform 
#' analyses on different data frames. To facilitate use the only argument that 
#' needs to be provided is a character element defining the model in the JAGS dialect of the 
#' the BUGS language. However various other arguments can also be set 
#' to provide additional control. 
#' 
#' The monitor argument is used
#' to define the parameters in the model to monitor - by default all model parameters
#' are monitored except those that begin with a d, e or i and are followed by an 
#' upper case character, i.e., dIntercept would not be monitored while bIntercept
#' and dintercept would. 
#' 
#' If select is null (the default) all variables in the
#' data frame are passed to the analysis.  If select is defined then only those
#' variables named in select are passed to the analysis. As a warning is given if a 
#' variable named in select is not in the data frame this can be useful for ensuring
#' all the required variables are present in the data frame.  In addition if
#' a variable name in select is followed by a * then the variables is standardised
#' (as described in \code{\link{convert_data}}) before
#' it is passed to the analysis i.e., \code{select=c("Weight", "Length*")}
#'  would result in \code{Length} being
#' standardised in the analysis while \code{select=c("Weight", "Length")} would not. A transformation can also
#' be applied to a variable - for example the
#'  argument \code{select=c("Weight", "log(Length)*")} would result in 
#' \code{Length} being logged (and renamed \code{LogLength}) and then standardised. 
#' 
#' Once the select argument has been applied to the dates and factors are converted
#' into integers and the data frame is converted into list form for input into JAGS. 
#' As well as each variable the list also contains a named element for each
#' factor that gives the number of levels of the factor. For example if the factor 
#' \code{Type} with three levels is present in the input data then an addition element
#' would be created in the list of data with the name \code{nType} and value three. This is useful
#' for iterating over factor levels in the JAGS model code. To facilitate
#' iterating over all the data the list also contains an element named \code{nrow}
#' that defines the number of rows in the input data.
#' 
#' In some cases additional manipulations of the data may be required such as
#' conversion of variables into matrix or array form based on input factor levels.
#' This can be achieved by defining a function for the \code{modify_data} argument. The function
#' will be passed the data in list form and should return a list of the modified data. 
#' 
#' At this point initial values can be generated for one or more of the model parameters
#' using the \code{gen_inits} argument which expects a function that takes the list of data
#' and returns a list of the initial values - the function is called once for each chain
#' and should therefore use random generation of initial values if different starting
#' values are desired. The remaining arguments are used after the analysis has completed. 
#' 
#' The \code{derived_model} argument is used to define a model in the JAGS dialect of 
#' the BUGS language that specifies derived parameters. For further information on the
#' use of the \code{derived_model} argument see \code{\link{derived}}.
#' 
#' The \code{random} argument is used specify which parameters represent random effects.
#' It takes 
#' the form of a named list where the parameters are the names of the list elements 
#' and the values are character vectors of the variables in the input data frame that
#' the parameters are random with respect to. For further information on the
#' use of the \code{random} argument see \code{\link{derived}}.
#' 
#' The \code{extract_data} argument converts the parameter estimates into a 
#' data set (after they have been converted to list form)
#' 
#' The \code{description} argument is a named character vector that can be used
#' to provide a description of parameters or variables in the JAGS model code. Currently
#' the \code{description} argument has no functionality.
#' 
#' @return a \code{jmodel} object
#' @seealso \code{\link{convert_data}}, \code{\link{derived}} 
#' @examples
#' # Poisson GLM from Kery & Schaub (2011) p.55-66
#' model <- jmodel(" 
#'  model { 
#'    alpha ~ dunif(-20, 20)
#'    beta1 ~ dunif(-10, 10)
#'    beta2 ~ dunif(-10, 10)
#'    beta3 ~ dunif(-10, 10)
#'    
#'    for (i in 1:nrow) { 
#'      log(lambda[i]) <- alpha + beta1 * Year[i] 
#'        + beta2 * Year[i]^2 + beta3 * Year[i]^3
#'      Count[i]~dpois(lambda[i])

#'    } 
#'  }",
#'  gen_inits = function (data) {
#'    inits <- list()
#'    inits$alpha <- runif(1,-2,-2)
#'    inits$beta1 <- runif(1,-3,-3)
#'    
#'    return (inits)
#'  }
#')
#'@references 
#'Kery & Schaub (2011) Bayesian Population Analysis
#' using WinBUGS. Academic Press, Oxford. \url{http://store.elsevier.com/Bayesian-Population-Analysis-using-WinBUGS/Marc-Kery/isbn-9780123870209/}
#' 
#'Plummer M (2012) JAGS Version 3.3.0 User Manual \url{http://sourceforge.net/projects/mcmc-jags/files/Manuals/}
#' @export 
jmodel <- function (model, monitor = NULL, select = NULL, modify_data = NULL, 
                    gen_inits = NULL, derived_model = NULL, random = NULL, 
                    extract_data = NULL,  description = NULL
) {

  if (length(model) != 1 || !is.character(model)) 
    stop ("model must be a character vector of length 1")
  if (!(is.null(monitor) || (is.character(monitor) && length(monitor) >= 1))) 
    stop ("monitor must be NULL or a character vector of length 1 or more")
  if (!is.null(select) && !is.character(select))
    stop ("select must be NULL or a character vector")
  if (!(is.null(modify_data) || is.function(modify_data)))
    stop ("modify_data must be NULL or a function")
  if (!(is.null(gen_inits) || is.function(gen_inits)))
    stop ("gen_inits must be NULL or a function")
  if (!(is.null(random) || (is.list(random) & !is.null(names(random)))))
    stop ("random must be NULL or a named list")  
  if (!(is.null(derived_model) || (is.character(derived_model) && length(derived_model)==1)))
    stop ("derived_model must be NULL or a character vector of length 1")
  if(!(is.null(extract_data) || is.function(extract_data)))
    stop("extract_data must be NULL or a function")
  if (!(is.null(description) || (is.character(description) & !is.null(names(description)))))
    stop ("description must be NULL or a named character vector")
  
  if(!is.null(monitor)) {
    monitor <- sort(unique(monitor))
  }
  
  object<-list(
    model = model,
    monitor = monitor,
    select = select,
    modify_data = modify_data,
    gen_inits = gen_inits,
    random = random,
    derived_model = derived_model,
    extract_data = extract_data,
    description = description
  )
  
  class(object) <- "jmodel"
  return (object)
}




