
summary.mcarray <- function (object, ...) {
  dim <- dim (object)
  print(dim)
  
  return (NULL)
}

summary.jags_mcmc <- function (object, ...) {
  cat("\nDimensions:\n")
  dim <- c(chains = nchain(object), simulations = nsim(object))
  print(dim)
  
  for (i in seq_along(object$mcmc)) {
    print(names(object$mcmc)[i])
    summary(object$mcmc[[i]])
  }
  return (NULL)  
}

summary.jagr_analysis <- function (object, level = level, ...)
{
  stopifnot(is.jagr_analysis(object))
  stopifnot(is.numeric(level))
  stopifnot(length(level) == 1)
  stopifnot(level >= 0.75)
  stopifnot(level <= 0.99)
  
  summ <- list()
  
  summ[["Generation"]] <- c(iterations = object$iterations,time = round(object$time,2))
  
  summ[["Dimensions"]] <- c(simulations = nsim(object),chains = nchain(object))

  parm <- get_parm(object, parm = "all")
  
  summ[["Convergence"]] <- convergence_jagr_analysis(object, parm = parm, summarise = TRUE)

  parm <- get_parm(object, parm = "fixed")
  
  summ[["Estimates"]] <- calc_estimates(object, parm = parm, level = level)

  summ[["Deviance Information Criterion"]] <- DIC(object)
  
  class (summ) <- "summary_jagr_analysis"
  
  return (summ)  
}

#' @method summary jags_analysis
#' @export
summary.jags_analysis <- function (object, level = "current", ...)
{
  if (!is.jags_analysis(object)) {
    stop("object must be a jags_analysis")
  }

  old_opts <- opts_jagr()
  on.exit(opts_jagr(old_opts))
  
  if (!is.numeric(level)) {
      opts_jagr(mode = level)
      level <- opts_jagr("level")
  } else {
    if (length(level) != 1) {
      stop("level must be length 1")
    } 
    if (level < 0.75 || level > 0.99) {
      stop("level must lie between 0.75 and 0.99")
    }
  } 
  
  summ <- list()
    
  for (i in 1:nmodel(object)) {
    x <- subset(object,model_number = i)
    x <- as.jagr_analysis(x)
    summ[[paste0("Model",i)]] <- summary(x, level = level)
  }
  summ[["Model Comparison"]] <- object$dic
  
  class (summ) <- "summary_jags_analysis"
  
  return (summ)  
}
