jaggernaut is an R package to facilitate Bayesian analyses using JAGS (Just Another Gibbs Sampler).

Key features include 

  * reasonable default values so its often possible to perform simple analyses with just the specification of the JAGS model code and the data set

  * specification of the analysis data set in the form of a data frame 

  * ability to automatically increase the length of the MCMC chains until a pre-specified level of convergence is reached

  * option to run chains and models on separate processes (currently only available for unix-based systems)

  * ability to extract derived values from previously converged analyses using JAGS code


