                                         
#' @export
power_jags <- function (object, parm = list("fixed" = c("significance < 0.05")), level = "current") {
  if(!is.jags_power_analysis(object))
    stop("object must be a jags_power_analysis")
  
  if(!(is.list(parm) && is_named(parm)))
    stop("parm must be a named list")

  if(length(parm) == 0)
    stop("parm must contain at least one element")
  
  if(!all(sapply(parm, is_character)) || !all(sapply(parm, is_scalar)))
    stop("all the elements in parm must be character scalars")
       
  old_opts <- opts_jagr()
  on.exit(opts_jagr(old_opts))
  
  if (!is.numeric(level)) {
    opts_jagr(mode = level)
    level <- opts_jagr("level")
  }
     
  quiet <- opts_jagr("quiet")
  
  parms <- data.frame(parameter = NA, statistic = NA, comparison = NA, bound = NA)
  parms <- parms[-1,]
  
  for (i in seq_along(parm)) {
    df <- data.frame(parameter = expand_parm(object, names(parm)[i]))
                     
    str <- strsplit(parm[[1]]," ", fixed =TRUE)[[1]]
    
    if(length(str) != 3)
      stop("the elements of parm must use syntax of the form 'p < 0.05' where there
           is a space between the statistic, comparison operator and bound")
    
    if(!str[1] %in% c("estimate","lower","upper","sd","error","significance"))
      stop("the statistics in parm must be 'estimate','lower','upper',
            'sd','error' or 'significance'")

    if(!str[2] %in% c("<","<=",">",">="))
      stop("the comparison operators in parm must be '<','<=','>',
            or '>='")
    if(is.na(is.numeric(str[3])))
      stop("the bounds in parm must be numeric values")   
                     
    df2 <- data.frame(statistic = str[1], comparison = str[2], bound = str[3])
    df <- merge(df, df2)
    
    parms <- rbind(parms, df)
  }
              
  rhat_threshold <- rhat_threshold(object)
  analyses <- analyses(object)
  
  
  melt_coef <- function (object, parm, level, rhat_threshold) {
    stopifnot(is.jagr_power_analysis(object))
        
    coef <- coef(object, parm = parm, level = level)
        
    coef$parameter <- rownames(coef) 
    coef <- reshape2::melt(coef, id.vars = c("parameter"), variable.name = "statistic", value.name = "number")
    
    if(!is_converged(object, rhat_threshold = rhat_threshold))
      is.na(coef$number) <- TRUE
    
    return (coef)
  }
  
  ldply_analyses <- function (x, parm, level, rhat_threshold) {
    return (ldply(x, melt_coef, parm, level, rhat_threshold))
  }
      
  coef <- ldply(analyses, ldply_analyses, parm = parms$parameter, level = level, rhat_threshold = rhat_threshold)
  
  coef$replicate <- paste0("replicate",as.integer(substr(coef$.id,10,15)))
  coef$value <- paste0("value",rep(1:nvalues(object), each = nrow(coef)/nvalues(object)))
  coef$.id <- NULL
  
  coef <- reshape2::dcast(coef,value + parameter + statistic ~ replicate,
                          value.var = "number")
  
  values <- values(object)
  values <- cbind(data.frame(value = row.names(values)),values)
  
  parms <- merge(values, parms)
  power <- merge(parms, coef)
    
  power$converged <- NA
  power$nreps <- NA
  power$power <- NA
  power$lower <- NA
  power$upper <- NA
  
  for(i in 1:nrow(power)) {
    vec <- power[i,substr(colnames(power),1,9) == "replicate"]
    power$converged[i] <- round(length(vec[!is.na(vec)]) / length(vec),3)
    power$nreps[i] <- length(vec[!is.na(vec)])
    cmd <- paste("power$estimate[i] <- length(vec[!is.na(vec) & vec",power$comparison[i],power$bound[i],"])")
    eval(parse(text = cmd))
  }
    
  power[,c("estimate","lower","upper")] <- binomjags(n = power$nreps, s = power$estimate)
  power$power <- power$estimate
  
  power <- subset(power,select = c(colnames(values),"parameter","statistic","comparison","bound","converged","nreps","power","lower","upper"))
  
  return (power)
}
