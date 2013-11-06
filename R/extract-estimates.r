
extract_estimates <- function (object, ...) {
  UseMethod("extract_estimates", object)
}

extract_estimates.jagr_chains <- function (object, level = "current", estimate = "current", ...) {
  
  if (!is.numeric(level) && level != "current") {
    old_opts <- opts_jagr(mode = level)
    on.exit(opts_jagr(old_opts))
  }
  
  if (!is.numeric(level)) {
    level <- opts_jagr("level")
  }
  
  if(!estimate %in% c("mean","median") && estimate != "current") {
    old_opts <- opts_jagr(mode = level)
    if(is.null(sys.on.exit()))
      on.exit(opts_jagr(old_opts))
  }
  
  if (!estimate %in% c("mean","median")) {
    estimate <- opts_jagr("estimate")
  }
  
  est <- coef(object, parm = "all", level = level, estimate = estimate)
  
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
