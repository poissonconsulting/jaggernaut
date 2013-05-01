
as.gsmcmc<- function (object, ...) {
  UseMethod("as.gsmcmc", object)
}

as.jagr_analysis<- function (object, ...) {
  UseMethod("as.jagr_analysis", object)
}

as.array.mcarray <- function (object) {
  if (!inherits(object,"mcarray"))
    stop("object should be of class mcarray")
    
  dim <- dim(object)
  dim <- dim[-length(dim)]
  dim[length(dim)] <- nsim (object)
  dim(object) <- dim
  names(dim) <- NULL
  class(object)<-"array"
  object<-drop(object)

  return (object)
}

as.list.gsmcmc <- function (object) {
  if (!inherits(object,"gsmcmc"))
    stop("object should be of class gsmcmc")
  
  mcmc <- object$mcmc
  list <- list()
  for (name in names(mcmc))
    list[[name]] <- as.array(mcmc[[name]])
  
  return (list)
}

as.mcmc.list.gsmcmc <- function (x) {
  if (!inherits (x,"gsmcmc"))
    stop ("x should be class gsmcmc")
  
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
      vnames.ch <- c(vnames.ch, rjags:::coda.names(varname, vardim))
      ans.ch[[i]] <- var.i
    }
    ans.ch <- do.call("cbind", ans.ch)
    colnames(ans.ch) <- vnames.ch
    ans[[ch]] <- mcmc(ans.ch)
  }
  return (coda::mcmc.list(ans))
}

as.matrix.gsmcmc <- function (x) {
  if (!inherits (x,"gsmcmc"))
    stop ("x shold be class gsmcmc")
  return (as.matrix(as.mcmc.list(x)))
}

as.data.frame.gsmcmc <- function (x) {
  if (!inherits (x,"gsmcmc"))
    stop ("x should be class gsmcmc")
  return (as.data.frame(as.matrix(x)))
}

as.gsmcmc.jagr_analysis <- function (object) {
  return (object$mcmc)
}

as.jagr_analysis.jags_analysis <- function (object) {
  stopifnot(is.jags_analysis(object))
  stopifnot(nmodel(object) == 1)
    
  return (object$analyses[[1]])
}

as.gmcmc.jags_analysis <- function (object) {
  stopifnot(is.jags_analysis(object))
  stopifnot(nmodel(object) == 1)
  
  object <- as.jagr_analysis(object)
  object <- as.gsmcmc (object)
  
  return (object)
}
  
