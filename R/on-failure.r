on_failure(is_convertible_data_list) <- function(call, env) {
  paste0(deparse(call$x), " is not a convertible list")
}

on_failure(is_convertible_data_frame) <- function(call, env) {
  paste0(deparse(call$x), " is not a convertible data.frame")
}

on_failure(is_convertible_data) <- function(call, env) {
  paste0(deparse(call$x), " is not a convertible data.frame or list")
}
