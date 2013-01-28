# functions from rjags that not exported

parse.varname <- function(varname) {
  
  ## Try to parse string of form "a" or "a[n,p:q,r]" where "a" is a
  ## variable name and n,p,q,r are integers
  
  v <- try(parse(text=varname, n=1), silent=TRUE)
  if (!is.expression(v) || length(v) != 1)
    return(NULL)
  
  v <- v[[1]]
  if (is.name(v)) {
    ##Full node array requested
    return(list(name=deparse(v)))
  }
  else if (is.call(v) && identical(deparse(v[[1]]), "[") && length(v) > 2) {
    ##Subset requested
    ndim <- length(v) - 2
    lower <- upper <- numeric(ndim)
    if (any(nchar(sapply(v, deparse)) == 0)) {
      ##We have to catch empty indices here or they will cause trouble
      ##below
      return(NULL)
    }
    for (i in 1:ndim) {
      index <- v[[i+2]]
      if (is.numeric(index)) {
        ##Single index
        lower[i] <- upper[i] <- index
      }
      else if (is.call(index) && length(index) == 3 &&
        identical(deparse(index[[1]]), ":") &&
        is.numeric(index[[2]]) && is.numeric(index[[3]]))
      {
        ##Index range
        lower[i] <- index[[2]]
        upper[i] <- index[[3]]
      }
      else return(NULL)
    }
    if (any(upper < lower))
      return (NULL)
    return(list(name = deparse(v[[2]]), lower=lower, upper=upper))
  }
  return(NULL)
}

parse.varnames <- function(varnames)
{
  names <- character(length(varnames))
  lower <- upper <- vector("list", length(varnames))
  for (i in seq(along=varnames)) {
    y <- parse.varname(varnames[i])
    if (is.null(y)) {
      stop(paste("Invalid variable subset", varnames[i]))
    }
    names[i] <- y$name
    if (!is.null(y$lower)) {
      lower[[i]] <- y$lower
    }
    if (!is.null(y$upper)) {
      upper[[i]] <- y$upper
    }
  }
  return(list(names=names, lower=lower, upper=upper))
}

# from rjags
coda.names <- function(basename, dim)
{
  ## Utility function used to get the names of the individual elements
  ## of a node array
  
  if (prod(dim) == 1)
    return(basename)
  
  ##Default lower and upper limits
  ndim <- length(dim)
  lower <- rep(1, ndim)
  upper <- dim
  
  ##If the node name is a subset, we try to parse it to get the
  ##names of its elements. For example, if basename is "A[2:3]"
  ##we want to return names "A[2]", "A[3]" not "A[2:3][1]", "A[2:3][2]".
  pn <- parse.varname(basename)
  if (!is.null(pn) && !is.null(pn$lower) && !is.null(pn$upper)) {
    if (length(pn$lower) == length(pn$upper)) {
      dim2 <- pn$upper - pn$lower + 1
      if (isTRUE(all.equal(dim[dim!=1], dim2[dim2!=1],
                           check.attributes=FALSE))) {
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
      indices <- outer(indices, lower[i]:upper[i], FUN=paste, sep=",")
    }
  }
  paste(basename,"[",as.vector(indices),"]",sep="")
}
