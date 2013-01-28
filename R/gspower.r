
gspower <- function (
  model_block, data_block, values, nreps = 100, pars = NULL, debug = FALSE, ...
)
{
  if(!is.gsmodel(model_block))
    stop ("model_block should be class gsmodel")

  if(!is.gsdata(data_block))
    stop ("data_block should be class gsdata")
  
  if(!(is.data.frame(values) && nrow(values) >= 1))
    stop ("values should be a data frame where each row is the parameter values")

  if(!(length(nreps) == 1 && nreps[1] >= 1))
    stop("nreps should be a single integer >= 1")
  
  if (!(is.null(pars) || (is.numeric(pars) && !is.null(pars))))
    stop ("pars must be NULL or a named numeric vector")
  
  if(!(is.logical(debug) && length(debug) == 1 && !is.null(debug)))
    stop("debug must be single logical value")
  
  if(debug)
    nreps <- 1
  
  nreps <- as.integer(nreps)
    
  object<-list(
    model_block = model_block,
    data_block= data_block,
    values = values,
    nvalues = nrow(values),
    nreps = nreps,
    nconfail = NULL, 
    analyses = NULL,
    parnames = NULL,
    parvalues = NULL,
    power = NULL
  )
  analyses = list()
  
  nconfail <- rep(0, object$nvalues)
  
  for (i in 1:object$nvalues) {
    rep <- list()
    for (j in 1:object$nreps) {
      repeat {
        print (paste("values",i,"of",object$nvalues))
        print (paste("rep",j,"of",object$nreps))

        sim<- gssimulation (block = data_block, as.data.frame(values[i,,drop=F]), debug = debug)
        analysis <- gsanalysis (block = model_block, data = sim$data, debug = debug, ...)
  
        rep[[j]] <- analysis
        
        if (check_convergence(analysis))
          break
        nconfail[i] <-  nconfail[i] + 1
        if(debug)
          break
      }
    }
    analyses[[i]] <- rep
  }
  object$nconfail <- nconfail
  object$analyses <- analyses
  
  class(object) <- c("gspower")
  
  object <- calc_power(object, pars = pars)
  
  return (object)
}




