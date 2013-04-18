
calc_expected_jagr_analysis <- function (analysis, parameter, data = "", base = FALSE, 
                           values = NULL, derived_model = NULL, random = NULL, 
                                         length.out = 30, calc_estimates = T) {
  
  if (!is.jagr_analysis(analysis))
    stop ("analysis should be class jagr_analysis")
  if (!(is.character(parameter) && length(parameter) == 1))
    stop("parameter should be a character vector of length one")  
  if (!(is.null(data) || is.data.frame(data) || is.character (data)))
    stop ("data must be a data frame, a character or NULL")
  if (!(is.logical(base) || is.data.frame(base)))
    stop ("effect must be a data frame or TRUE or FALSE")
  if(is.data.frame (base) && nrow(base)!=1)
    stop ("if base is a data frame it should only have one row")
  if (!(is.null(random) || (is.list(random) & !is.null(names(random)))))
    stop ("random must be NULL or a named list")  
  if (!(is.null(derived_model) || is.character(derived_model)))
    stop ("derived_model must be NULL or a character")
  if(!(is.null(values) || (is.data.frame (values) && nrow(values)==1)))
    stop ("values should be null or a data frame with only one row")
  if(!is.logical(calc_estimates))
    stop ("calc_estimates should be TRUE or FALSE")
  
  if(!is.null(random)) {
    analysis$model$random <- random
  }
  
  if (is.null(data)) {
    data <- analysis$data
  } else if (is.character(data)) {
    data <- generate_data (analysis$data, range = data, length.out=length.out)
  }
     
  if(is.data.frame(base)) {
    bas <- generate_data(analysis$data)
    bas <- bas[,!colnames(bas) %in% colnames(base)]
    base <- cbind(base, bas)
  } else if (base) {
    base <- generate_data(analysis$data)
  }
     
     if(is.data.frame(values)) {
       for (col in colnames(values)) {
         if (col %in% colnames(data)) {
            x <- data[,col,drop = T]
            if (length(unique(x)) == 1) {            
              data[,col] <- values[1,col]
            }
         } else {
            data[,col] <- values[1,col]
         }
         if (is.data.frame(base)) {
           base[,col] <- values[1,col]
         }
       }
     }
  
  if (!is.null(derived_model)) {
    model <- derived_model
  } else
    model <- analysis$model$derived_model
        
  emcmc <- calc_derived (analysis, model=model, 
    monitor=parameter, data = data, calc_estimates = F)  
      
  if (is.data.frame(base)) {
        
    base <- calc_derived (analysis, model = model, 
      monitor = parameter, data = base, calc_estimates = F)
        
    base <- multiply (base,nrow(data))   
    emcmc <- (emcmc - base) / base
  }

  if(calc_estimates) {
    emcmc <- calc_estimates (emcmc)
  } else {
    emcmc <- as.data.frame(t(get_sims (emcmc, parameter)))
  }
  
  data <- cbind (data,emcmc)
  return (data)
}
 
calc_expected <- function (analysis, parameter, data = "", base = FALSE, 
                                     values = NULL, derived_model = NULL, random = NULL, 
                                     length.out = 30, calc_estimates = T, model = 1) {
  
  if (!is.janalysis(analysis))
    stop ("analyses should be class janalysis")
  

  return (calc_expected_jagr_analysis(select_model(analysis, model = model), parameter = parameter, data = data, 
                        base = base, values = values, derived_model = derived_model, random = random, 
                        length.out = length.out, calc_estimates = calc_estimates))
}
