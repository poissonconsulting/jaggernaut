
extract_estimates <- function (object, ...) {
  UseMethod("extract_estimates", object)
}

extract_estimates.jagr_chains <- function (object, ...) {
  
  est <- coef(object, parm = "all", level = 0.95)
      
  ss <- strsplit(rownames(est),"\\[|,|]")
  ss <- lapply(ss,function (x) return (x[x != ""]))
  
  pars <- sapply(ss,function (x) return (x[1]))
  ndims <- sapply(ss,function (x) return (length(x)-1))

  
  df <- data.frame(pars,ndims)
  df <- unique(df)
  df <- df[order(df$pars),]

  estimates <- list()
  estimates$estimate <- list()
  estimates$lower <- list()
  estimates$upper <- list()
  
  for (i in 1:nrow(df)) {
    par <- as.character(df$pars[i])
    ndim <- df$ndims[i]
    if (ndim == 0) {
      estimates$estimate[[par]] <- est[par,"estimate"]
      estimates$lower[[par]] <- est[par,"lower"]
      estimates$upper[[par]] <- est[par,"upper"]
    } else {
      bol <- substr(rownames(est),1,nchar(par)+1) == paste0(par,"[")
      sss <- lapply(ss[bol],function (x) x[-1])
      estimates$estimate[[par]] <- array(est$estimate[bol], dim = sss[[length(sss)]])
      estimates$lower[[par]] <- array(est$lower[bol], dim = sss[[length(sss)]])
      estimates$upper[[par]] <- array(est$upper[bol], dim = sss[[length(sss)]])
    }
  }
    
  return (estimates)
}
