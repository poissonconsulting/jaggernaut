
#' @title Calculate derived values for a JAGS analysis
#'
#' @description
#' Calculates values for a derived parameter.
#' 
#' @param object a janalysis object.
#' @param parameter a character element naming the derived parameter of interest.
#' @param data a data.frame of the data values over which to calculate the
#' expected values of the derived parameter. If data.frame is "" xx.
#' @param base a boolean element indicating whether or not to express 
#' the expected value as a percent change of a base level or a data frame 
#' defining the base level.
#' @param values a data frame with a single row that defines the value of particular
#' variables. The variables in data and base are replaced by the corresponding values.
#' @param derived_model a character element defining a block in the JAGS dialect of 
#' the BUGS language that defines one or more derived parameter. 
#' If NULL the value is taken from the JAGS model for which the JAGS analysis was performed. 
#' @param random a named list which specifies which parameters to treat 
#' as random variables. If NULL the value is taken from the JAGS model for which the JAGS analysis was performed.
#' @param length_out an integer element indicating the number of values when 
#' creating a sequence of values across the range of a continuous variable.
#' @param conf_int a logical scalar indicating whether to return the individual
#' iterations or the median and 95% credibility intervals.
#' @param model an integer scalar indicating which model to select unless model = 0
#' in which case DIC-based model selection is used.
#' @return the input data frame with the median and 95% credibility intervals 
#' (or iterations) for
#' the derived parameter of interest
#' @seealso \code{\link{jmodel}}, \code{\link[jaggernaut]{janalysis}}
#' @examples
#' # Poisson GLM analysis of peregrine breeding pairs (Kery & Schaub 2011 p.55-66)
#' model <- jmodel(" 
#'  model { 
#'    alpha ~ dunif(-20, 20)
#'    beta1 ~ dunif(-10, 10)
#'    beta2 ~ dunif(-10, 10)
#'    beta3 ~ dunif(-10, 10)
#'    
#'    for (i in 1:nrow) { 
#'      log(eCount[i]) <- alpha + beta1 * Year[i] 
#'        + beta2 * Year[i]^2 + beta3 * Year[i]^3
#'      Count[i] ~ dpois(eCount[i])
#'    } 
#'  }",
#'  derived_model = "model{
#'    for (i in 1:nrow) {
#'      log(eCount[i]) <- alpha + beta1 * Year[i] 
#'        + beta2 * Year[i]^2 + beta3 * Year[i]^3    
#'    }
#'  }",
#' select = c("Count","Year*")
#')
#'
#' data <- peregrine
#' data$Count <- data$Pairs
#'
#' analysis <- janalysis (model, data)
#'
#' der <- derived(analysis, "eCount", data = "Year")
#'
#' gp <- ggplot(data = der, aes(x = Year, y = estimate))
#' gp <- gp + geom_line()
#' gp <- gp + geom_line(aes(y = lower), linetype = "dotted")
#' gp <- gp + geom_line(aes(y = upper), linetype = "dotted")
#' gp <- gp + geom_point(data = peregrine, aes(y = Pairs))
#' gp <- gp + scale_y_continuous(name = "Pair count")
#' gp <- gp + expand_limits(y = 0)
#' 
#' print(gp)
#' @export 
derived <- function (object, parameter, data = "", base = FALSE, 
                           values = NULL, derived_model = NULL, random = NULL, 
                           length_out = 30, conf_int = TRUE,  model = 1) {
  
  if (!is.janalysis(object))
    stop ("object should be class janalysis")
  
  model <- as.integer(model)
  
  if(!is.integer (model))
    stop("model should be an integer")
  if(!is_scalar (model))
    stop("model should be a scalar")  
  if(model < 0)
    stop("model should not be less than 0")

 
  return (calc_expected(object, 
                         parameter = parameter, data = data, 
                         base = base, values = values, 
                        derived_model = derived_model, random = random, 
                         length.out = length_out, 
                         calc_estimates = conf_int,
                        model = model))
  
}
  