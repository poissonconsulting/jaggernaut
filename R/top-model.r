
top_model <- function (analysis)
{  
  if(!is.jags_analysis(analysis))
    stop ("analysis should be class jags_analysis")
  
  if (analysis$n.model == 1) {
    analysis <- analysis$analyses[[1]]
  } else {
    analysis <- analysis$analyses[[rownames(analysis$dic)[1]]]
  }
  return (analysis)
}
