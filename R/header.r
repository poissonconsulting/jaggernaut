
options(jags.pb = "none")

if(!"basemod" %in% list.modules())
  rjags::load.module("basemod")  

if(!"bugs" %in% list.modules())
  rjags::load.module("bugs")

if(!"dic" %in% list.modules())
  rjags::load.module("dic")
