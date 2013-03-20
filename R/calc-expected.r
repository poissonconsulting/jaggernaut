
calc_expected_jagr_analysis <- function (analysis, parameter, data = "", base = FALSE, 
                           values = NULL, derived = NULL, random = NULL, 
                                         length.out = 30, calc_estimates = T) {
  
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
  if(!(is.null(values) || (is.data.frame (values) && nrow(values)==1)))
    stop ("values should be null or a data frame with only one row")
  if(!is.logical(calc_estimates))
    stop ("calc_estimates should be TRUE or FALSE")
  
  if(!is.null(random)) {
    analysis$model$random <- random
  }
  
  if (is.null(data)) {
    data <- analysis$data
  } else if (is.character(data)) {
    data <- generate_data (analysis$data, range = data, length.out=length.out)
  }
     
  if(is.data.frame(base)) {
    bas <- generate_data(analysis$data)
    bas <- bas[,!colnames(bas) %in% colnames(base)]
    base <- cbind(base, bas)
  } else if (base) {
    base <- generate_data(analysis$data)
  }
     
     if(is.data.frame(values)) {
       for (col in colnames(values)) {
         if (col %in% colnames(data)) {
            x <- data[,col,drop = T]
            if (length(unique(x)) == 1) {            
              data[,col] <- values[1,col]
            }
         } else {
            data[,col] <- values[1,col]
         }
         if (is.data.frame(base)) {
           base[,col] <- values[1,col]
         }
       }
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

  if(calc_estimates) {
    emcmc <- calc_estimates (emcmc)
  } else {
    emcmc <- as.data.frame(t(get_sims (emcmc, parameter)))
  }
  
  data <- cbind (data,emcmc)
  return (data)
}

#' @title Calculate expected values for a JAGS analysis (janalysis object)
#'
#' @description
#' Calculates expected values with 95% credibility intervals 
#' for a derived parameter.
#' 
#' @details
#' More information
#' 
#' @param analysis a JAGS analysis (janalysis) object.
#' @param parameter a character element naming the derived parameter of interest.
#' @param data a data.frame of the data values over which to calculate the
#' expected values of the derived parameter. If data.frame is "" xx.
#' @param base a boolean element indicating whether or not to express 
#' the expected value as a percent change of a base level or a data frame 
#' defining the base level.
#' @param values a data frame with a single row that defines the value of particular
#' variables. The variables in data and base are replaced by the corresponding values.
#' @param derived a character element defining a block in the JAGS dialect of 
#' the BUGS language that defines one or more derived parameter. 
#' If NULL the value is taken from the JAGS model for which the JAGS analysis was performed. 
#' @param random a named list which specifies which parameters to treat 
#' as random variables. If NULL the value is taken from the JAGS model for which the JAGS analysis was performed.
#' @param length.out an integer element indicating the number of values when 
#' creating a sequence of values across the range of a continuous variable.
#' @param calc_estimates a logical scalar indicating whether to return the individual
#' iterations or the median and 95% credibility intervals.
#' @return the input data frame with the median and 95% credibility intervals 
#' (or iterations) for
#' the derived parameter of interest
#' @export
#' @examples
#' model <- jmodel(
#'  model = "model { 
#'    bLambda ~ dunif(0,10) 
#'    for (i in 1:nrow) { 
#'      x[i]~dpois(bLambda) 
#'    } 
#'  }",
#'  derived = "model { 
#'    for (i in 1:nrow) { 
#'      eResidual[i] <- x[i] - bLambda
#'    } 
#'  }",
#'  select = c("x")
#' )
#' data <- data.frame(x = rpois(100,1))
#' analysis <- janalysis (model, data)
#' calc_expected(analysis, "eResidual", data = NULL)
#' 
calc_expected <- function (analysis, parameter, data = "", base = FALSE, 
                                     values = NULL, derived = NULL, random = NULL, 
                                     length.out = 30, calc_estimates = T) {
  
  if (!is.janalysis(analysis))
    stop ("analyses should be class janalysis")

  return (calc_expected_jagr_analysis(top_model(analysis), parameter = parameter, data = data, 
                        base = base, values = values, derived = derived, random = random, 
                        length.out = length.out, calc_estimates = calc_estimates))
}
