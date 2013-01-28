
#' Create JAGS model object
#'
#' Creates a JAGS model object
#' 
#' @param model a character element defining the model in the JAGS dialect of 
#' the BUGS language
#' @param monitor a character vector of the parameters to monitor
#' @param select a character vector of the variables to select from the 
#' data set being analysed
#' @param modify_data a function to modify the data set being analysed
#' (after it has been converted to list form)
#' @param gen_inits a function to generate initial values for an MCMC chain
#' (it is passed the modified data in list form)
#' @param random a named list of parameters representing random variables
#' @param derived a character element defining a model in the JAGS dialect of 
#' the BUGS language that defines derived parameters
#' @param extract_data a function to convert the parameter estimates into a data set
#'  (after they have been converted to list form)
#' @param description a named character vector descriping each of the parameters 
#' in the model where the name indicates the parameter to which the description applies
#' @return a JAGS model object
#' @export
#' @examples
#' model <- jmodel(" 
#'  model { 
#'    bLambda ~ dlnorm(0,10^-2) 
#'    for (i in 1:nrow) { 
#'      x[i]~dpois(bLambda) 
#'    } 
#'  }"
#')
#' 
jmodel <- function (model, monitor = NULL, select = NULL, modify_data = NULL, 
                    gen_inits = NULL, random = NULL, derived = NULL, 
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
  if (!(is.null(derived) || (is.character(derived) && length(derived)==1)))
    stop ("derived must be NULL or a character vector of length 1")
  if(!(is.null(extract_data) || is.function(extract_data)))
    stop("extract_data must be NULL or a function")
  if (!(is.null(description) || (is.character(description) & !is.null(names(description)))))
    stop ("description must be NULL or a named character vector")
  
  if(!is.null(monitor))
    monitor <- sort(monitor)
  
  object<-list(
    model = model,
    monitor = monitor,
    select = select,
    modify_data = modify_data,
    gen_inits = gen_inits,
    random = random,
    derived = derived,
    extract_data = extract_data,
    description = description
  )
  
  class(object) <- "jmodel"
  return (object)
}




