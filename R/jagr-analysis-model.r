jagr_analysis_model <- function (model_code, monitor = NULL, select = NULL, 
                                 modify_data = NULL, gen_inits = NULL, 
                                 derived_code = NULL, 
                                 aggregation_code = NULL,
                                 random_effects = NULL,
                                 select_derived = NULL,
                                 modify_data_derived = NULL,
                                 select_aggregation = NULL,
                                 modify_data_aggregation = NULL
  ) {  
  
  object <- jagr_model(model_code = model_code,
                       monitor = monitor, 
                       select = select,
                       modify_data = modify_data,
                       gen_inits = gen_inits)
  
  class(object) <- c("jagr_analysis_model","jagr_model")

  if(is.null(select_derived))
    select_derived <- select
  
  if(is.null(select_aggregation))
    select_aggregation <- select
  
  if(is.null(modify_data_derived))
    modify_data_derived <- modify_data
  
  if(is.null(modify_data_aggregation))
    modify_data_aggregation <- modify_data
  
  derived_code(object) <- derived_code
  aggregation_code(object) <- aggregation_code
  random_effects(object) <- random_effects
  select_derived(object) <- select_derived
  select_aggregation(object) <- select_aggregation
  modify_data_derived(object) <- modify_data_derived
  modify_data_aggregation(object) <- modify_data_aggregation
    
  return (object)
}
