
jagr_model <- function (model_code, 
                        monitor = NULL, 
                        select_data = NULL, 
                        modify_data = NULL, 
                        gen_inits = NULL, 
                        derived_code = NULL, 
                        aggregation_code = NULL,
                        random_effects = NULL,
                        select_data_derived = NULL,
                        modify_data_derived = NULL,
                        select_data_aggregation = NULL,
                        modify_data_aggregation = NULL
) {  
  
  object <- list()  
  
  class(object) <- "jagr_model"
  
  model_code(object) <- model_code
  monitor(object) <- monitor
  select_data(object) <- select_data
  modify_data(object) <- modify_data
  gen_inits(object) <- gen_inits
  derived_code(object) <- derived_code
  aggregation_code(object) <- aggregation_code
  random_effects(object) <- random_effects
  
  if(is.null(select_data_derived))
    select_data_derived <- select_data
  
  if(is.null(select_data_aggregation))
    select_data_aggregation <- select_data
  
  if(is.null(modify_data_derived))
    modify_data_derived <- modify_data
  
  if(is.null(modify_data_aggregation))
    modify_data_aggregation <- modify_data
  
  select_data_derived(object) <- select_data_derived
  select_data_aggregation(object) <- select_data_aggregation
  modify_data_derived(object) <- modify_data_derived
  modify_data_aggregation(object) <- modify_data_aggregation
  
  return (object)
}
