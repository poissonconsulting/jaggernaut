# jaggernaut

jaggernaut is an R package to facilitate Bayesian analyses using JAGS 
(Just Another Gibbs Sampler).

Key features include 

* the conversion of a data frame into a suitable format for input into JAGS

* the option to automatically update a model (increase the length of the MCMC chains)
until convergence is achieved

* the option to run MCMC chains on separate processes

* the ability to extract derived parameters from an existing model using BUGS code 
(in the JAGS dialect) without additional MCMC sampling

* simple generation of data frames quantifying the effect (and effect size) of 
particular variables with the other variables held constant

* power analyses with both the simulation and analytic models defined using BUGS 
code (in the JAGS dialect)

## Development

To install the development version of jaggernaut, it's easiest to use the `devtools` package:

    # install.packages("devtools")
    library(devtools)
    install_github("jaggernaut", "joethorley")
    
## Contact

You are welcome to:

* submit suggestions and bug-reports at: https://github.com/joethorley/jaggernaut/issues
* send a pull request on: https://github.com/joethorley/jaggernaut/
* compose a friendly e-mail to: joe@poissonconsulting.ca

