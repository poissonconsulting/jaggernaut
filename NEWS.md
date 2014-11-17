# NEWS

### jaggernaut v2.2.4

* significance values in coefficient tables cannot be less
than 1 / number of samples

### jaggernaut v2.2.3

* stripped out cpp code for internal function `bayesian_p_value` so easier to build from source

### jaggernaut v2.2.2

* `auto_corr` now produces matrix of lags by parameters by averaging chains
and dropping cross-correlations

### jaggernaut v2.2.1

* `summary.jags_analysis` get model names and lists convergence as rhat

### jaggernaut v2.2.0

* `model_name` and `model` replaced with `model_id`
* added `default_model_id` argument and function to specify which model
to use for default predictions, residuals etc

#### jaggernaut v2.1.1

* `predictive_check` now returns parameter names as rownnames as opposed
to a columnn

### jaggernaut v2.1.0

* renamed `jags_discrepancies` as `predictive_check` and now
returns coef table and can be used for multiple discrepancy measures
* now use `derived_code`, `modify_data_derived` and `select_data_derived`
instead of `aggregation_code`, `modify_data_aggregation` and
`select_data_aggregation`
* convergence now uses parallel chains
* use suffix '-' to specify parameters in monitor that monitor
but not use to calculate convergence - useful for parameters with 
bernoulli distribution


## jaggernaut v2.0.0

#### Major changes include:

* able to name models and specify using names
* parameters to monitor can now be specified with a regular expression
* now calculates autocor and crosscor
* no longer calculates DIC as rarely useful
* no longer calculates geweke as will allow through mcmc objects
* no longer performs power analyses as slow in Bayesian framework
* no longer generates datasets using bugs code

#### Minor changes include:

* renamed argument `model_number` as `model`
* renamed `rhat` as `convergence`
* renamed `select` as `select_data` so not conflict with dplyr
* renamed `select_derived` and `select_aggregation` as `select_data_derived`
and `select_data_aggregation`
* renamed `nsims` as `nsamples` so not ambiguous with `niters`

#### Known Bugs

* Throws error if attempts to update model that was initially
run with chains in parallel but not in series (and vice versa)

#### jaggernaut 1.8.5

- added `select_aggregation` and `modify_data_aggregation` arguments to `jags_model()`
- aggregation and derived code now convered to data block

#### jaggernaut 1.8.4

- `geweke` function now implemented using `abs(coda::geweke.diag)`

#### jaggernaut 1.8.3

- `coda::gelman.diag` now has `transform = TRUE` and `autoburnin = FALSE`

####  jaggernaut 1.8.2

- now uses `select_derived` when defined for predictions

####  jaggernaut 1.8.1

- fixed minor bug where printing aggregation object
- updated loaded package versions

####  jaggernaut 1.8.0

- added `jags_discrepancies()` function and `aggregative_code` argument
to allow calculation of posterior predictive checking
- sets mode to report on load

####  jaggernaut 1.7.1

- added `fritillary` demo

###  jaggernaut 1.7

- on load package sets up mode reporting by default
- max number of simulations saved now 6,000 (was 2,000)

### jaggernaut 1.6

* refer to git history...

####  jaggernaut 1.5.6

* `predict` function now gives observed base values when `base = TRUE` and
`obs_by = TRUE`

####  jaggernaut 1.5.5

* `obs_by` argument in `predict` functions now works for continuous 
variables (previously only reliable for categorical variables while returning
a subset of observed values for continuous variables)

####  jaggernaut 1.5.4

* `update` now runs chains in parallel (if workers available)

####  jaggernaut 1.5.3

* Fixed error when running in parallel on windows (was failing to export
needed functions)

#### jaggernaut 1.5.2

* Upgraded to `datalist` 0.2.

## Bug Reports 

For more fine-grained list of changes or to report a bug, consult 

* [The commit log](https://github.com/poissonconsulting/jaggernaut/commits/master)
* [The issues log](https://github.com/poissonconsulting/jaggernaut/issues)

## Versioning

Releases are numbered with the following semantic versioning format:

\<major\>.\<minor\>.\<patch\>

And constructed with the following guidelines:

* Breaking backward compatibility bumps the major (and resets the minor 
  and patch)
* New additions without breaking backward compatibility bumps the minor 
  (and resets the patch)
* Bug fixes and misc changes bumps the patch

For more information on jaggernaut, please visit: 
https://github.com/poissonconsulting/jaggernaut/wiki.

