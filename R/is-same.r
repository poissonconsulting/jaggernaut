 
 is_same <- function (object1, ...) {
   UseMethod("is_same", object1)
 }
 
 is_same.gsdata <- function (object1, object2) {
#   stopifnot(is.gsdata(object1))
#   
#   if(!identical(class(object1), class(object2)))
#     return (FALSE)
#   
#   if(!identical(object1$block, object2$block))
#     return (FALSE)
#   
#   if(!identical(object1$monitor, object2$monitor))
#     return (FALSE)
#   
#   if(!identical(object1$select, object2$select))
#     return (FALSE)
#   
#   if(!identical(object1$gen_inits, object2$gen_inits))
#     return (FALSE)
#   
#   if(!identical(object1$translate_data, object2$translate_data))
#     return (FALSE)
#   
#   if(!identical(object1$extract_data, object2$extract_data))
#     return (FALSE)
#   
  return (TRUE)  
 }
# 
# is_same.gsmodel <- function (object1, object2) {
#   stopifnot(is.gsmodel(object1))
#   
#   if(!identical(class(object1), class(object2)))
#     return (FALSE)
#   
#   if(!identical(object1$block, object2$block))
#     return (FALSE)
#   
#   if(!identical(object1$monitor, object2$monitor))
#     return (FALSE)
#   
#   if(!identical(object1$select, object2$select))
#     return (FALSE)
#   
#   if(!identical(object1$gen_inits, object2$gen_inits))
#     return (FALSE)
#   
#   if(!identical(object1$translate_data, object2$translate_data))
#     return (FALSE)
# 
#   if(!identical(object1$random, object2$random))
#     return (FALSE)
# 
#   if(!identical(object1$derived, object2$derived))
#     return (FALSE)
#   
#   return (TRUE)  
# }
# 
# 
# is_same.gspower <- function (object1, object2) {
#   stopifnot(is.gspower(object1))
# 
#   if(!is_same(object1$model_block, object2$model_block))
#     return (FALSE)
#   
#   if(!identical(object1$values, object2$values))
#     return (FALSE)
#   
#   return (TRUE)  
# }
