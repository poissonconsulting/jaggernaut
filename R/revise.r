
revise <- function (object, ...) {
  UseMethod("revise", object)
}

revise.jags_analysis <- function (object, ...) {
  
  analyses <- analyses(object)
    
  dic <- t(sapply(analyses,dic_jags_jagr_analysis))  
  rownames(dic) <- paste0("Model",1:nrow(dic))  
  dic <- dic[order(dic[,"DIC",drop=T]),]
  
  dic_jags(object) <- dic
  
  return (object)
}
