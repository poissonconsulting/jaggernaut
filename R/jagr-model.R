jagr_model <- function (model_code, 
                        model_id,                        
                        monitor, 
                        select_data, 
                        modify_data, 
                        gen_inits, 
                        derived_code, 
                        random_effects,
                        select_data_derived,
                        modify_data_derived
) {  
  
  object <- list()  
  
  class(object) <- "jagr_model"
  
  model_code(object) <- model_code
  model_id(object) <- model_id
  monitor(object) <- monitor
  select_data(object) <- select_data
  modify_data(object) <- modify_data
  gen_inits(object) <- gen_inits
  derived_code(object) <- derived_code
  random_effects(object) <- random_effects
  
  if(is.null(select_data_derived))
    select_data_derived <- select_data
  
  if(is.null(modify_data_derived))
    modify_data_derived <- modify_data
  
  select_data_derived(object) <- select_data_derived
  modify_data_derived(object) <- modify_data_derived
  
  return (object)
}
