
predict_jagr <- function (object, parm, data, base, level, estimate, ...) {
  
  stopifnot(is.jags_analysis(object) && is_one_model(object))
  stopifnot(is_character_scalar(parm))
  stopifnot(is.jags_data(data))
  stopifnot(is.null(base) || (is.jags_data_frame(base) && is.jags_data_frame(data)))
  stopifnot(is_numeric_scalar(level))
  stopifnot(is_bounded(level, 0.75, 0.99) || level == 0)
  stopifnot(estimate %in% c("mean","median"))
  
  data_sample <- derived (object, parm = parm, data = data) 
  data_sample <- jags_sample(data_sample, parm = parm, data = data)
  
  if (is.jags_data_frame(base)) {
    base_sample <- derived (object, parm = parm, data = base)
    base_sample <- jags_sample(base_sample, parm = parm, data = base)
    
    base_sample <- base_sample[rep(1,nrow(data_sample)),]
    
    percent <- function (x) {
      stopifnot(length(x) == 2)
      return ((x[1] - x[2]) / x[2])
    }
    
    data_sample$all <- 1:nrow(data_sample)

    base_sample <- samples(base_sample)
    base_sample$all <- 1:nrow(base_sample)
    class(base_sample) <- c("data.frame", "jags_sample")
    
    base_sample <- merge_jags_samples(list(data_sample, base_sample), 
                                      by = "all", fun = percent)
            
    data_sample <- merge(data_jags(data_sample), base_sample, by = "all")
    data_sample$all <- NULL
    class(data_sample) <- c("data.frame", "jags_sample")    
  }
  
  if(level == 0)
    return (data_sample)
  
  coef <- coef (data_sample, parm = parm, level = level, estimate = estimate)
  
  return (coef)
}
