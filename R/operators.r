
"-.mcarray" <- function (a, b) {
  if (!is.mcarray (b))
    stop ("b should be of class mcarray")
  
  if (!identical(dim (a), dim (b)))
    stop ("a and b must be of the same dimensions")
  
  dim <- dim(a)
  dnames <- names(dim (a))
  c <- as.array(a) - as.array(b)
  dim(c) <- dim
  names(dim(c)) <- dnames
  class(c) <- 'mcarray'
  return (c)
}

"-.gsmcmc" <- function (a, b) {
  if (!is.gsmcmc (b))
    stop ("b should be of class gsmcmc")
  
  if (length (a$mcmc) != length (b$mcmc))
    stop ("a and b should be of the same dimensions")

  c <- a
  for (i in 1:length(a$mcmc)) {
    c$mcmc[[i]] <- a$mcmc[[i]] - b$mcmc[[i]]
  }
  return (c)
}

"/.mcarray" <- function (a, b) {
  if (!is.mcarray (b))
    stop ("b should be of class mcarray")
  
  if (!identical(dim (a), dim (b)))
    stop ("a and b must be of the same dimensions")
  
  dim <- dim(a)
  dnames <- names(dim (a))
  c <- as.array(a) / as.array(b)
  dim(c) <- dim
  names(dim(c)) <- dnames
  class(c) <- 'mcarray'
  return (c)
}

"/.gsmcmc" <- function (a, b) {
  if (!is.gsmcmc (b))
    stop ("b should be of class gsmcmc")
  
  if (length (a$mcmc) != length (b$mcmc))
    stop ("a and b should be of the same dimensions")
  
  c <- a
  for (i in 1:length(a$mcmc)) {
    c$mcmc[[i]] <- a$mcmc[[i]] / b$mcmc[[i]]
  }
  return (c)
}
