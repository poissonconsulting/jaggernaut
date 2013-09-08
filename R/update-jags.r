
#' @export
update_jags <- function (object, ...) {
  UseMethod("update_jags", object)
}

#' @method update_jags jags_simulation
#' @export 
update_jags.jags_simulation <- function (object, nrep = 1, values = NULL, mode = "current")
{
  if(!is.jags_simulation(object))
    stop("object should be of class jags_simulation")
  
  if(!is.null(values)) {
    if(!is.data.frame(values))
      stop ("values must be NULL or a data frame")
    
    if(nrow(values) == 0)
      stop ("values must have at least one row of data")
    
    if(ncol(values) == 0)
      stop ("values must have at least one column of data")
  }
  
  if(!is.numeric(nrep))
    stop("nrep must be class integer")
  
  if(!length(nrep) == 1)
    stop("nrep must be a single value")
  
  if(nrep < 0)
    stop("nrep must not be positive")
  
  nrep <- as.integer(nrep)
  
  if(!"basemod" %in% list.modules())
    load.module("basemod")  
  
  if(!"bugs" %in% list.modules())
    load.module("bugs")
  
  old_opts <- opts_jagr(mode = mode)
  on.exit(opts_jagr(old_opts))
  
  if(!is.null(values)) {
  
    if(any(!colnames(object$values) %in% colnames(values)))
      stop("colnames missing from values")
  
    if(any(!colnames(values) %in% colnames(object$values)))
      warning("unrecognised colnames in values")  
  
    values <- subset(values, select = colnames(object$values))

    newObject <- jags_simulation(data_model = object$data_model, 
                                 nrep = object$nrep + nrep, 
                                 values = values, 
                                 mode = "current")
  }
  
  if (nrep > 0) {
    nvalues <- nrow(object$values)
    for (value in 1:nvalues) {
      for (rep in (object$nrep + 1):(object$nrep + nrep)) {
        if (!opts_jagr("quiet"))
          print(paste0("Value: ",value," of ",nvalues,"  Rep: ", rep," of ",(object$nrep + nrep)))
                
        x <- jagr_simulation(model = object$data_model, 
                                  data = object$values[value,,drop = FALSE], 
                                  quiet = opts_jagr("mode") != "debug")
        
        est <- calc_estimates(x)
        
        est <- est[rownames(est) != "deviance",]
        
        object$simulated[[value]][[rep]] <- extract_estimates(est)[["estimate"]]
      }
    }
    object$nrep <- object$nrep + nrep
  }
  
  #  object <- object + newObject
  
  return (object)
}
