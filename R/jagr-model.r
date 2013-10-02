
jagr_model <- function (model_code, monitor = NULL, select = NULL, 
                        modify_data = NULL, gen_inits = NULL) {  
  
  object <- list(
    model_code = NULL,
    monitor = NULL,
    select = NULL,
    modify_data = NULL,
    gen_inits = NULL
  )  

  class(object) <- "jagr_model"
  
  model_code(object) <- model_code
  monitor(object) <- monitor
  select(object) <- select
  modify_data(object) <- modify_data
  gen_inits(object) <- gen_inits
  
  return (object)
}
