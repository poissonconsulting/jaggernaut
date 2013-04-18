
select_model <- function (analysis, model)
{  
  if(!is.janalysis(analysis))
    stop ("analysis should be class janalysis")
  
  model <- as.integer(model)
  
  if(!is.integer (model))
    stop("model should be an integer")
  if(!is_scalar (model))
    stop("model should be a scalar")
  if(model > analysis$n.model)
    stop("model should not be greater than the number of models")  
  if(model < 0)
    stop("model should not be less than 0")  
  
  if(model > 0) {
    return (analysis$analyses[[model]])
  }
  if (analysis$n.model == 1) {
    return (analysis$analyses[[1]])
  }
  
  return (analysis$analyses[[rownames(analysis$dic)[1]]])
}
