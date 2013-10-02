# package coda
# function set.mfrow
set.mfrow <- function (Nchains = 1, Nparms = 1, nplots = 1, sepplot = FALSE) 
{
  mfrow <- if (sepplot && Nchains > 1 && nplots == 1) {
    if (Nchains == 2) {
      switch(min(Nparms, 5), c(1, 2), c(2, 2), c(3, 2), 
             c(4, 2), c(3, 2))
    }
    else if (Nchains == 3) {
      switch(min(Nparms, 5), c(2, 2), c(2, 3), c(3, 3), 
             c(2, 3), c(3, 3))
    }
    else if (Nchains == 4) {
      if (Nparms == 1) 
        c(2, 2)
      else c(4, 2)
    }
    else if (any(Nchains == c(5, 6, 10, 11, 12))) 
      c(3, 2)
    else if (any(Nchains == c(7, 8, 9)) || Nchains >= 13) 
      c(3, 3)
  }
  else {
    if (nplots == 1) {
      mfrow <- switch(min(Nparms, 13), c(1, 1), c(1, 2), 
                      c(2, 2), c(2, 2), c(3, 2), c(3, 2), c(3, 3), 
                      c(3, 3), c(3, 3), c(3, 2), c(3, 2), c(3, 2), 
                      c(3, 3))
    }
    else {
      mfrow <- switch(min(Nparms, 13), c(1, 2), c(2, 2), 
                      c(3, 2), c(4, 2), c(3, 2), c(3, 2), c(4, 2), 
                      c(4, 2), c(4, 2), c(3, 2), c(3, 2), c(3, 2), 
                      c(4, 2))
    }
  }
  return(mfrow)
}

# package coda
# function plot.mcmc.list
plot.mcmc.list <- function (x, trace = TRUE, density = TRUE, smooth = TRUE, bwf, 
                            auto.layout = TRUE, ask = par("ask"), ...) 
{
  oldpar <- NULL
  on.exit(par(oldpar))
  if (auto.layout) {
    mfrow <- set.mfrow(Nchains = nchains(x), Nparms = nvar(x), 
                       nplots = trace + density)
    oldpar <- par(mfrow = mfrow)
  }
  for (i in 1:nvar(x)) {
    if (trace) 
      traceplot(x[, i, drop = FALSE], smooth = smooth, 
                ...)
    if (density) {
      if (missing(bwf)) 
        densplot(x[, i, drop = FALSE], ...)
      else densplot(x[, i, drop = FALSE], bwf = bwf, ...)
    }
    if (i == 1) 
      oldpar <- c(oldpar, par(ask = ask))
  }
}

# package rjags
# function parse.varname
parse.varname <- function (varname) 
{
  v <- try(parse(text = varname, n = 1), silent = TRUE)
  if (!is.expression(v) || length(v) != 1) 
    return(NULL)
  v <- v[[1]]
  if (is.name(v)) {
    return(list(name = deparse(v)))
  }
  else if (is.call(v) && identical(deparse(v[[1]]), "[") && 
             length(v) > 2) {
    ndim <- length(v) - 2
    lower <- upper <- numeric(ndim)
    if (any(nchar(sapply(v, deparse)) == 0)) {
      return(NULL)
    }
    for (i in 1:ndim) {
      index <- v[[i + 2]]
      if (is.numeric(index)) {
        lower[i] <- upper[i] <- index
      }
      else if (is.call(index) && length(index) == 3 && 
                 identical(deparse(index[[1]]), ":") && is.numeric(index[[2]]) && 
                 is.numeric(index[[3]])) {
        lower[i] <- index[[2]]
        upper[i] <- index[[3]]
      }
      else return(NULL)
    }
    if (any(upper < lower)) 
      return(NULL)
    return(list(name = deparse(v[[2]]), lower = lower, upper = upper))
  }
  return(NULL)
}

# package rjags
# function coda.names
coda.names <- function (basename, dim) 
{
  if (prod(dim) == 1) 
    return(basename)
  ndim <- length(dim)
  lower <- rep(1, ndim)
  upper <- dim
  pn <- parse.varname(basename)
  if (!is.null(pn) && !is.null(pn$lower) && !is.null(pn$upper)) {
    if (length(pn$lower) == length(pn$upper)) {
      dim2 <- pn$upper - pn$lower + 1
      if (isTRUE(all.equal(dim[dim != 1], dim2[dim2 != 
                                                 1], check.attributes = FALSE))) {
        basename <- pn$name
        lower <- pn$lower
        upper <- pn$upper
        ndim <- length(dim2)
      }
    }
  }
  indices <- as.character(lower[1]:upper[1])
  if (ndim > 1) {
    for (i in 2:ndim) {
      indices <- outer(indices, lower[i]:upper[i], FUN = paste, 
                       sep = ",")
    }
  }
  paste(basename, "[", as.vector(indices), "]", sep = "")
}
