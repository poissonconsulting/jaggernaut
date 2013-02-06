
#' @export
calc_expected<- function (object, ...) {
  UseMethod("calc_expected", object)
}

calc_expected.jagr_analysis <- function (analysis, parameter, data = "", base = FALSE, 
                           derived = NULL, random = NULL, length.out = 30) {
  
  if (!is.jagr_analysis(analysis))
    stop ("analysis should be class jagr_analysis")
  if (!(is.character(parameter) && length(parameter) == 1))
    stop("parameter should be a character vector of length one")  
  if (!(is.null(data) || is.data.frame(data) || is.character (data)))
    stop ("data must be a data frame, a character or NULL")
  if (!(is.logical(base) || is.data.frame(base)))
    stop ("effect must be a data frame or TRUE or FALSE")
  if(is.data.frame (base) && nrow(base)!=1)
    stop ("if base is a data frame it should only have one row")
  if (!(is.null(random) || (is.list(random) & !is.null(names(random)))))
    stop ("random must be NULL or a named list")  
  if (!(is.null(derived) || is.character(derived)))
    stop ("derived must be NULL or a character")
  
  if(!is.null(random)) {
    analysis$model$random <- random
  }
  
  doSelect <- !is.null(analysis$model$select) && !is.data.frame(data)
  
  if (is.null(data)) {
    data <- analysis$data
  } else if (is.character(data))
    data <- generate_data (analysis$data, range = data, length.out=length.out)
    
  if(is.data.frame(base) || base) {
    bas <- generate_data(analysis$data)
    bas <- bas[,!colnames(bas) %in% colnames(base)]
    base <- cbind(base, bas)
  }
  
  if (!is.null(derived)) {
    model <- derived
  } else
    model <- analysis$model$derived
      
  emcmc <- calc_derived (analysis, model=model, 
    monitor=parameter, data = data, calc_estimates = F)  
      
  if (is.data.frame(base)) {
    
    base <- calc_derived (analysis, model = model, 
      monitor = parameter, data = base, calc_estimates = F)
        
    base <- multiply (base,nrow(data))   
    emcmc <- (emcmc - base) / base
  }

  emcmc <- calc_estimates (emcmc)
  if (doSelect) {
    select <- process_select(analysis$model$select)
    select <- select[select %in% colnames (data)]
    data <- subset(data, select = select) 
  }
  data <- cbind (data,emcmc)
  return (data)
}

#' Calculate expected values for JAGS analysis
#'
#' Calculates expected values with 95% credibility intervals 
#' for a derived parameter.
#' 
#' @param analysis a JAGS analysis object.
#' @param parameter a character element naming the derived parameter of interest.
#' @param data a data.frame of the data values over which to calculate the
#' expected values of the derived parameter. If data.frame is "" xx.
#' @param base a boolean element indicating whether or not to express 
#' the expected value as a percent change.
#' @param derived a character element defining a block in the JAGS dialect of 
#' the BUGS language that defines one or more derived parameter. 
#' If NULL the value is taken from the JAGS model for which the JAGS analysis was performed. 
#' @param random a named list which specifies which parameters to treat 
#' as random variables.
#' @param length.out an integer element indicating the number of values when 
#' creating a sequence of values across the range of a continuous variable.
#' @method calc_expected janalysis
#' @S3method calc_expected janalysis
#' @export
#' @examples
#' model <- jmodel(
#'  model = "model { bLambda ~ dunif(0,10) for (i in 1:nrow) { x[i]~dpois(bLambda) } }",
#'  derived = "model { for (i in 1:nrow) { 
#'  eResidual[i] <- x[i] - bLambda
#'  } }"
#' )
#' data <- data.frame(x = rpois(100,1))
#' analysis <- janalysis (model, data)
#' calc_expected(analysis, "eResidual", data = NULL)
#' 
calc_expected.janalysis <- function (analysis, parameter, data = "", base = FALSE, 
                                     derived = NULL, random = NULL, length.out = 30) {
  
  if (!is.janalysis(analysis))
    stop ("analyses should be class janalysis")

  return (calc_expected(top_model(analysis), parameter = parameter, data = data, 
                        base = base, derived = derived, random = random, 
                        length.out = length.out))
}
