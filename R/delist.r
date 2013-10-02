
delist <- function (object) {
  
  if (!is.list(object) 
      || is.data.frame(object) 
      || is.jagr_model(object)
      || is.jags_model(object)      
      || is.jags_data_model(object) 
      || is.jagr_analysis(object)       
      || is.jags_analysis(object)       
      || is.jags_simulation(object) 
      || is_named(object)
      || length(object) != 1)
    return (object)
  
  object <- object[[1]]
  
  return (delist(object))
}
