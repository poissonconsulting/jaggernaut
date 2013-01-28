
extract_estimates <- function (object, pars = NULL) {
  
  est <- calc_estimates (object, pars = pars)
  
  ss <- strsplit(rownames(est),"\\[|,|]")
  ss <- lapply(ss,function (x) return (x[x != ""]))
  
  pars <- sapply(ss,function (x) return (x[1]))
  ndims <- sapply(ss,function (x) return (length(x)-1))
  
  df <- data.frame(pars,ndims)
  df <- unique(df)
  df <- df[order(df$pars),]

  estimates <- list()
  
  for (i in 1:nrow(df)) {
    par <- as.character(df$pars[i])
    ndim <- df$ndims[i]
    if (ndim == 0) {
      estimates[[par]] <- est[par,"estimate"]
    } else {
      bol <- substr(rownames(est),1,nchar(par)+1) == paste0(par,"[")
      sss <- lapply(ss[bol],function (x) x[-1])
      estimates[[par]] <- array(est$estimate[bol], dim = sss[[length(sss)]])
    }
  }
    
  return (estimates)
}
