jaggregate <- function (object, parameters, data) {
  
  stopifnot(is.jags_analysis(object))
  
  chains <- zero_random (object, data)
  
  file <- tempfile(fileext=".bug")
  code <- aggregation_code(object) 
  code <- paste(code,"model { deviance <- 1}")
  
  cat(code, file=file)
  
  nchains <- nchains (chains)
  nsamples <- nsamples (chains) / nchains
  
  samples <- get_samples(samples = 1:nsamples, chains = chains, data = data, 
    parm = parameters, file = file)
  
  newobject <- list()
  class(newobject) <- "jagr_chains"
  
  samples(newobject) <- samples
  jags(newobject) <- list(NULL)
  random(newobject) <- NULL
  
  return (newobject)
}

ags_sample <- function (chains) {
  
  stopifnot(is.jagr_chains(chains))
  
  samples <- t(as.matrix(chains))
  
  data <- data.frame(parameter = row.names(samples))
  data$index <- ""
  reg <- regexpr("([[].*)", data$parameter)
  data$index[reg != -1] <- regmatches(data$parameter, reg)
  
  data$parameter <- sub("([[].*)", "", data$parameter, perl = TRUE) 
  
  object <- cbind(data, samples)
  
  class(object) <- c("data.frame", "jags_sample", "jags_aggregation")
  
  return (object)
}

discrepancies <- function (object) {
    
  stopifnot(is.jags_aggregation(object))
  
  # just those parameters that occur twice and only twice with indices [1] and [2]
  object <- object[object$index %in% c("[1]","[2]"), ]
  parameters <- object$parameter[duplicated(object$parameter)]
  parameters <- parameters[!duplicated(parameters)]
  
  if(!not_empty(parameters))
    stop("object does not contain any paired aggregative parameters")
  
  object <- object[object$parameter %in% parameters,]
  object <- object[order(object$parameter, object$index),]
  
  object$index[object$index == "[1]"] <- "Actual" 
  object$index[object$index == "[2]"] <- "Replicate"

  class(object) <- c("jags_discrepancies", "jags_sample", "data.frame")
  return (object)
}

#' JAGS analysis discrepancies
#' 
#' Uses aggregation code parameters with just two levels to get an
#' object of class jags_discrepancies that can be used to plot 
#' visuals of discrepancies or calculate bayesian p-values using
#' coef or extract data frame using dataset for own plotting. 
#' 
#' @param object jags_analysis
#' @param model_number a count or string specifying the jags model to select.
#' @param aggregation_code string of JAGS model code defining paired
#' posterior predictive checking aggregation parameters
#' parameters
#' @importFrom juggler jg_vnodes
#' @examples
#'   model1 <- jags_model(
#'" model { 
#'    sVolume ~ dunif(0, 100)
#' bIntercept ~ dnorm (0, 100^-2)
#' bGirth ~ dnorm(0, 100^-2)
#' for (i in 1:length(Volume)) {
#' eVolume[i] <- bIntercept + bGirth * Girth[i] 
#' Volume[i] ~ dnorm(eVolume[i], sVolume^-2) 
#'} 
#'}",
#'aggregation_code = "model {
#'for (i in 1:length(Volume)) {
#'prediction[i] <- bIntercept + bGirth * Girth[i]
#'E[i] <- pow(Volume[i] - prediction[i], 2) / prediction[i]
#'newVolume[i] ~ dnorm(prediction[i], sVolume^-2)
#'E2[i] <- pow(newVolume[i] - prediction[i], 2) / prediction[i]
#'}
#'EE[1] <- sum(E)
#'EE[2] <- sum(E2)
#'}",
#'select_data = c("Volume","Girth")
#'  )
#'  
#'  models <- combine(model1)
#'  
#'  data <- trees
#'  
#'  analysis <- jags_analysis(models, data = data, mode = "test")
#'  
#'  discrepancies <- jags_discrepancies(analysis)
#'  
#'  dataset(discrepancies)
#'  coef(discrepancies)
#'  \dontrun{ 
#'  plot(discrepancies)
#'  }
#' @export
jags_discrepancies <- function (object, model_number = 1, aggregation_code = NULL) {
  
  assert_that(is.jags_analysis(object))

  object <- subset(object, model_number = model_number)
  
  if(!is.null(aggregation_code)) {
    aggregation_code(object) <- aggregation_code
  } else  
    aggregation_code <- aggregation_code(object)
  
  if(is.null(aggregation_code))
    stop("undefined aggregation code")
  
  # get just those parameters with [1] and [2] and indices that occur twice
  parameters <-  jg_vnodes(aggregation_code, type = "both", indices = TRUE)  
  parameters <- parameters[grepl("[[](1|2)[]]", parameters)]
  parameters <- gsub("[[](1|2)[]]", "", parameters)
  parameters <- parameters[duplicated(parameters)]
  
  if(!not_empty(parameters))
    stop("aggregation code does not contain any paired aggregative parameters")
  
  data <- dataset(object)
  
  data <- translate_data(select_data(object), data) 
  
  if (is.function(modify_data(object))) 
    data <- modify_data(object)(data)
  
  assert_that(is_converted_data(data))
    
  agg <- jaggregate (object, parameters = parameters, data = data)
  agg <- ags_sample(agg)
  agg <- discrepancies(agg)
}
