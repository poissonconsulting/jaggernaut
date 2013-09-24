
as.jags_mcmc<- function (object, ...) {
  UseMethod("as.jags_mcmc", object)
}

as.jags_model<- function (object, ...) {
  UseMethod("as.jags_model", object)
}

as.jagr_analysis<- function (object, ...) {
  UseMethod("as.jagr_analysis", object)
}

as.array.mcarray <- function (object, ...) {
    
  dim <- dim(object)
  dim <- dim[-length(dim)]
  dim[length(dim)] <- nsim (object)
  dim(object) <- dim
  names(dim) <- NULL
  class(object)<-"array"
  object<-drop(object)

  return (object)
}

as.list.jags_mcmc <- function (object, ...) {  
  mcmc <- object$mcmc
  list <- list()
  for (name in names(mcmc))
    list[[name]] <- as.array(mcmc[[name]])
  
  return (list)
}

as.mcmc.list.jags_mcmc <- function (x, ...) {
  if (!inherits (x,"jags_mcmc"))
    stop ("x should be class jags_mcmc")
  
  ans <- list()
  for (ch in 1:nchain(x)) {
    ans.ch <- vector("list", length(x$mcmc))
    vnames.ch <- NULL
    for (i in seq(along = x$mcmc)) {
      varname <- names(x$mcmc)[[i]]
      d <- dim(x$mcmc[[i]])
      vardim <- d[1:(length(d) - 2)]
      nvar <- prod(vardim)
      niter <- d[length(d) - 1]
      nchain <- d[length(d)]
      values <- as.vector(x$mcmc[[i]])
      var.i <- matrix(NA, nrow = niter, ncol = nvar)
      for (j in 1:nvar) {
        var.i[, j] <- values[j + (0:(niter - 1)) * nvar + 
          (ch - 1) * niter * nvar]
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

as.matrix.jags_mcmc <- function (x, parm = "all", ...) {
  mat <- as.matrix(as.mcmc.list(x, ...), ...)
  
  if (identical(parm,"all"))
    return (mat)
  
  return (mat[,x$svars %in% parm,drop=F])
}

as.data.frame.jags_mcmc <- function (x, ...) {
  return (as.data.frame(as.matrix(x, ...), ...))
}

as.jags_mcmc.jagr_analysis <- function (object, ...) {
  return (object$mcmc)
}

as.jags_model.jagr_analysis <- function (object, ...) {
  return (object$model)
}

as.jags_model_jagr_analysis <- function (object, ...) {
  stopifnot(is.jagr_analysis(object))
  return (return (as.jags_model(object, ...)))
}

as.jagr_analysis.jags_analysis <- function (object, ...) {    
  object <- object$analyses
  object <- delist(object)
  return (object)
}

as.jags_model.jags_analysis <- function (object, ...) {
  object <- as.jagr_analysis(object, ...)
  
  if (is.jagr_analysis(object))
    return (as.jags_model(object, ...))
  
  object <- lapply(object, as.jags_model_jagr_analysis, ...)
  
  return (object)
}

as.jags_mcmc.jags_analysis <- function (object, ...) {
  
  object <- as.jagr_analysis(object, ...)
  
  if (is.jagr_analysis(object))
    return (as.jags_mcmc(object, ...))
  
  object <- lapply(object, as.jags_mcmc, ...)
  
  return (object)
}

as.jags_mcmc.jagr_simulation <- function (object, ...) {    
  return (object$mcmc)
}
