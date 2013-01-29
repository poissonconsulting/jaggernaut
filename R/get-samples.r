#

get_samples <- function (model, data, file) {
  # could remove unnecessary data so not need to suppress warning messages...
  warn <- options('warn')
  options(warn = -1)
  
  jags <- jags.model (file = file, data = data, 
                      n.chains = 1, n.adapt = 0, quiet = T
  )

  samples <- jags.samples(
    model = jags, variable.names = model$monitor, n.iter = 1
  )
  options (warn)
  
  return (samples)
}