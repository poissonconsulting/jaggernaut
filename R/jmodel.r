
jmodel <- function (model, monitor = NULL, select = NULL, modify_data = NULL, 
                    gen_inits = NULL, derived_model = NULL, random = NULL, 
                    extract_data = NULL,  description = NULL
) {

  if (length(model) != 1 || !is.character(model)) 
    stop ("model must be a character vector of length 1")
  if (!(is.null(monitor) || (is.character(monitor) && length(monitor) >= 1))) 
    stop ("monitor must be NULL or a character vector of length 1 or more")
  if (!is.null(select) && !is.character(select))
    stop ("select must be NULL or a character vector")
  if (!(is.null(modify_data) || is.function(modify_data)))
    stop ("modify_data must be NULL or a function")
  if (!(is.null(gen_inits) || is.function(gen_inits)))
    stop ("gen_inits must be NULL or a function")
  if (!(is.null(random) || (is.list(random) & !is.null(names(random)))))
    stop ("random must be NULL or a named list")  
  if (!(is.null(derived_model) || (is.character(derived_model) && length(derived_model)==1)))
    stop ("derived_model must be NULL or a character vector of length 1")
  if(!(is.null(extract_data) || is.function(extract_data)))
    stop("extract_data must be NULL or a function")
  if (!(is.null(description) || (is.character(description) & !is.null(names(description)))))
    stop ("description must be NULL or a named character vector")
  
  if(!is.null(monitor)) {
    monitor <- sort(unique(monitor))
  }
  
  object<-list(
    model = model,
    monitor = monitor,
    select = select,
    modify_data = modify_data,
    gen_inits = gen_inits,
    random = random,
    derived_model = derived_model,
    extract_data = extract_data,
    description = description
  )
  
  class(object) <- "jmodel"
  return (object)
}




