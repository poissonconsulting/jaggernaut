rename_models <- function (object) {
  stopifnot(is.jags_model(object))
  
  names <- sapply(models(object), model_name_jagr_model)
  names(names) <- NULL  
  duplicates <- names[duplicated(names)]
  is.na(names[names %in% duplicates]) <- TRUE
  names[is.na(names)] <- paste0("Model", which(is.na(names)))
  names(object$models) <- names
  object
}

rename_analyses <- function (object) {
  stopifnot(is.jags_analysis (object))
  
  names <- sapply(analyses(object), model_name_jagr_model)
  names(names) <- NULL  
  duplicates <- names[duplicated(names)]
  is.na(names[names %in% duplicates]) <- TRUE
  names[is.na(names)] <- paste0("Model", which(is.na(names)))
  names(object$analyses) <- names
  object
}
