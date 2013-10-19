
summary.mcarray <- function (object, ...) {
  dim <- dim (object)
  print(dim)
  
  return (NULL)
}

summary.jagr_chains <- function (object, ...) {
  cat("\nDimensions:\n")
  dim <- c(chains = nchains(object), simulations = nsims(object))
  print(dim)
  
  for (i in seq_along(samples(object))) {
    print(names(samples(object))[i])
    summary(samples(object)[[i]])
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
  
  summ[["Dimensions"]] <- c(simulations = nsims(object),chains = nchains(object))

  summ[["Convergence"]] <- c(rhat = rhat(object, parm = "all", combine = TRUE))

  summ[["Estimates"]] <- coef(object, parm = "fixed", level = level)
  
  class (summ) <- "summary_jagr_analysis"
  
  return (summ)  
}

#' @method summary jags_analysis
#' @export
summary.jags_analysis <- function (object, level = "current", ...)
{
  
  if (!is.numeric(level)) {
    if (level != "current") {
      old_opts <- opts_jagr(mode = level)
      on.exit(opts_jagr(old_opts))
    }
    level <- opts_jagr("level")
  } else {
    if (level < 0.75 || level > 0.99) {
      stop("level must lie between 0.75 and 0.99")
    }
  } 
  
  analyses <- analyses(object)
  
  summ <- list()
  
  for (i in 1:nmodels(object)) {
    summ[[paste0("Model",i)]] <- summary(analyses[[i]], level = level)
  }
  summ[["Model Comparison"]] <- dic_jags(object)

  class (summ) <- "summary_jags_analysis"
  
  return (summ)  
}
