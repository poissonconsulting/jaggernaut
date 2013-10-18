
as.jagr_chains<- function (x, ...) {
  UseMethod("as.jagr_chains", x)
}

as.jagr_model<- function (x, ...) {
  UseMethod("as.jagr_model", x)
}

#' @export
as.jags_data_model<- function (x, ...) {
  UseMethod("as.jags_data_model", x)
}

as.jagr_analysis_model<- function (x, ...) {
  UseMethod("as.jagr_analysis_model", x)
}

#' @export
as.jags_model <- function (x, ...) {
  UseMethod("as.jags_model", x)
}

as.jagr_power_analysis <- function (x, ...) {
  UseMethod("as.jagr_power_analysis", x)
}

#' @export
as.jags_simulation <- function (x, ...) {
  UseMethod("as.jags_simulation", x)
}

as.array.mcarray <- function (x, ...) {

  dim <- dim(x)
  dim <- dim[-length(dim)]
  dim[length(dim)] <- nsims(x)
  dim(x) <- dim
  names(dim) <- NULL
  class(x)<-"array"
  x<-drop(x)

  return (x)
}

as.matrix.jagr_chains <- function (x, ...) {
  return (as.matrix(as.mcmc.list(x, ...), ...))
}

as.data.frame.jagr_chains <- function (x, ...) {
  return (as.data.frame(as.matrix(x, ...), ...))
}

as.list.jagr_chains <- function (x, ...) {  
  samples <- samples(x)
  list <- list()
  for (name in names(samples))
    list[[name]] <- as.array(samples[[name]])
  
  return (list)
}

as.mcmc.list.jagr_chains <- function (x, ...) {
  
  ans <- list()
  for (ch in 1:nchains(x)) {
    ans.ch <- vector("list", length(samples(x)))
    vnames.ch <- NULL
    for (i in seq(along = samples(x))) {
      varname <- names(samples(x))[[i]]
      d <- dim(samples(x)[[i]])
      vardim <- d[1:(length(d) - 2)]
      nvar <- prod(vardim)
      niters <- d[length(d) - 1]
      nchains <- d[length(d)]
      values <- as.vector(samples(x)[[i]])
      var.i <- matrix(NA, nrow = niters, ncol = nvar)
      for (j in 1:nvar) {
        var.i[, j] <- values[j + (0:(niters - 1)) * nvar + 
          (ch - 1) * niters * nvar]
      }
      vnames.ch <- c(vnames.ch, coda.names(varname, vardim))
      ans.ch[[i]] <- var.i
    }
    ans.ch <- do.call("cbind", ans.ch)
    colnames(ans.ch) <- vnames.ch
    ans[[ch]] <- mcmc(ans.ch)
  }
  return (coda::mcmc.list(ans))
}

as.jagr_chains.jagr_power_analysis <- function (x, ...) {
  return (chains(x))
}

as.jagr_model.jags_data_model <- function (x, ...) {
  
  x$extract_data <- NULL
  
  class(x) <- "jagr_model"
  return (return (x))
}

as.jagr_model.jagr_analysis_model <- function (x, ...) {
  
  x$modify_data_derived <- NULL
  x$derived_code <- NULL
  x$random_effects <- NULL
  
  class(x) <- "jagr_model"
  return (x)
}

as.jagr_model.jagr_analysis <- function (x, ...) {
  
  x <- as.jagr_analysis_model(x, ...)
  
  return (as.jagr_model(x, ...))
}

as.jags_data_model.jags_simulation <- function (x, ...) {
  return (data_model(x))
}

as.jagr_analysis_model.jagr_analysis<- function (x, ...) {
  
  x$init_values <- NULL
  x$chains <- NULL
  x$niters <- NULL
  x$time_interval <- NULL
  
  class(x) <- c("jagr_analysis_model","jagr_model")
  
  return (x)
}

as.jagr_analysis_model_jagr_analysis <- function (x, ...) {
  stopifnot(is.jagr_analysis(x))
  return (as.jagr_analysis_model(x, ...))
}

#' @method as.jags_model jags_analysis
#' @export
as.jags_model.jags_analysis <- function (x, ...) {
  analyses <- analyses(x)
  
  models <- lapply(analyses, as.jagr_analysis_model_jagr_analysis, ...)
  
  x <- list()
  class(x) <- "jags_model"
  
  models(x) <- models
  
  return (x)
}

#' @method as.jags_model jags_analysis
#' @export
as.jags_model.jags_power_analysis <- function (x, ...) {
  analysis_model <- analysis_model(x)
  
  object <- list()
  class(object) <- "jags_model"
  models(object) <- as.list(analysis_model)
  
  return(object)
}

as.jagr_power_analysis.jagr_analysis <- function (x, ...) {

  x$model_code <- NULL
  x$monitor <- NULL
  x$select <- NULL
  x$modify_data <- NULL
  x$gen_inits <- NULL
  x$modify_data_derived <- NULL
  x$derived_code <- NULL
  x$random_effects <- NULL
  
  class(x) <- c("jagr_power_analysis")
  
  return (x)
}

as.jags_simulation.jags_power_analysis <- function (x, ...) {
  
  x$analysis_model <- NULL
  x$analyses <- NULL
  x$rhat_threshold <- NULL
  x$power <- NULL
  
  class(x) <- c("jags_simulation")
  
  return (x)
}
