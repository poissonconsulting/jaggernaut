jaggernaut is an R package to facilitate Bayesian analyses using JAGS (Just Another Gibbs Sampler).

Key features include 

  * straightforward functions and reasonable default values so that its often possible to perform simple analyses with just the specification of the JAGS model code and the data set

  * input data set in data frame form with automatic conversion to list of numeric vectors for import into JAGS (option to manipulate vectors to create arrays etc prior to 
import into JAGS)

  * standardization of particular variables covered by single argument

  * the length of the MCMC chains can be automatically increased until a pre-specified level of convergence is reached

  * individual MCMC chains can be run on parallel processes (currently only available for unix-based systems)

  * previous analyses can be easily queried using JAGS code to extract derived values (with 95% CRIs) 

  *  effect size (percent change in response) estimates (with 95% CRIs) can be simply calculated



