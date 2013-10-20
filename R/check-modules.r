
check_modules <- function () {
if(!"basemod" %in% rjags::list.modules())
  rjags::load.module("basemod")  

if(!"bugs" %in% rjags::list.modules())
  rjags::load.module("bugs")

if(!"dic" %in% rjags::list.modules())
  rjags::load.module("dic")

 invisible()
}
