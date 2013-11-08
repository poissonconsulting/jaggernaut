
get_samples <- function (monitor, data, file) {
  # could remove unnecessary data so not need to suppress warning messages...
  warn <- options('warn')
  options(warn = -1)
      
  jags <- rjags::jags.model (file = file, data = data, 
                      n.chains = 1, n.adapt = 0, quiet = TRUE
  )

  samples <- rjags::jags.samples(
    model = jags, variable.names = monitor, n.iter = 1
  )
  options (warn)
  
  return (samples)
}
