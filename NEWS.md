# NEWS jaggernaut

### v2.4.2

- update Remotes

### v2.4.1

- `convergence_analysis` now works with different numbers of iterations.

### v2.4.0

- Add function `convergence_analysis` to test if two analyses have similar posteriors.

#### v2.3.4

- `predict.jags_analysis` argument `length_out` maximum increased from 200 to 1000.

#### v2.3.3

- importFrom graphics par plot etc to avoid message

#### v2.3.2

- Fixed bug where coef on jag_sample object grabbing columns with 1 or more digits in name.

#### v2.3.1

- `coef.jags_analysis` now has argument `latex` to replace parameter names
with latex math comments. useful for generation of sweave tables.

### v2.3.0

- added ability to get mean and SD for centered predictor `Var` (#59,63) by
specifying `Var_MU` and `Var_SD` in derived code

#### v2.2.11

- updated to `datalist` v0.4.1

#### v2.2.10

- `predict.jags_analysis` now works with new columns in `select_data_derived`

#### v2.2.9

- implemeted travis checking
- fixed bug `coef.jags_analysis` now works with `parm = "no"`

#### v2.2.8

- if chains run in parallel and not enough workers when updating gives informative error message
- if chains run in series then continues to run in series when updating
- added function `is_parallel` to test if parallel chains
- changed definitions of various modes - "report" has 10^4 samples and 5 chains
- now only runs chains in parallel if at least as many workers
- renamed rhat as Rhat in output

## v2.2.7

- `coef` table estimates now rounded to equivalent number of significant digits
whether or not fraction

## v2.2.6

- `modify_data_derived(analysis) <-` no longer throws error when passed function

## v2.2.5

- rounds coefficient table estimates, credible intervals and standard deviation
depending on number of samples
- rounds `auto_corr` and `cross_cor` estimates to decimal places

## v2.2.4

- significance values in coefficient tables now cannot be less
than 1 / number of samples

## v2.2.3

- stripped out cpp code for internal function `bayesian_p_value` so easier to build from source

## v2.2.2

- `auto_corr` now produces matrix of lags by parameters by averaging chains
and dropping cross-correlations

## v2.2.1

- `summary.jags_analysis` get model names and lists convergence as rhat

## v2.2.0

- `model_name` and `model` replaced with `model_id`
- added `default_model_id` argument and function to specify which model
to use for default predictions, residuals etc

### v2.1.1

- `predictive_check` now returns parameter names as rownnames as opposed
to a columnn

## v2.1.0

- renamed `jags_discrepancies` as `predictive_check` and now
returns coef table and can be used for multiple discrepancy measures
- now use `derived_code`, `modify_data_derived` and `select_data_derived`
instead of `aggregation_code`, `modify_data_aggregation` and
`select_data_aggregation`
- convergence now uses parallel chains
- use suffix '-' to specify parameters in monitor that monitor
but not use to calculate convergence - useful for parameters with 
bernoulli distribution


### v2.0.0

#### Major changes include:

- able to name models and specify using names
- parameters to monitor can now be specified with a regular expression
- now calculates autocor and crosscor
- no longer calculates DIC as rarely useful
- no longer calculates geweke as will allow through mcmc objects
- no longer performs power analyses as slow in Bayesian framework
- no longer generates datasets using bugs code

#### Minor changes include:

- renamed argument `model_number` as `model`
- renamed `rhat` as `convergence`
- renamed `select` as `select_data` so not conflict with dplyr
- renamed `select_derived` and `select_aggregation` as `select_data_derived`
and `select_data_aggregation`
- renamed `nsims` as `nsamples` so not ambiguous with `niters`

#### Known Bugs

- Throws error if attempts to update model that was initially
run with chains in parallel but not in series (and vice versa)

## jaggernaut 1.8.5

- added `select_aggregation` and `modify_data_aggregation` arguments to `jags_model()`
- aggregation and derived code now convered to data block

## jaggernaut 1.8.4

- `geweke` function now implemented using `abs(coda::geweke.diag)`

## jaggernaut 1.8.3

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

# jaggernaut 1.6

- refer to git history...

####  jaggernaut 1.5.6

- `predict` function now gives observed base values when `base = TRUE` and
`obs_by = TRUE`

####  jaggernaut 1.5.5

- `obs_by` argument in `predict` functions now works for continuous 
variables (previously only reliable for categorical variables while returning
a subset of observed values for continuous variables)

####  jaggernaut 1.5.4

- `update` now runs chains in parallel (if workers available)

####  jaggernaut 1.5.3

- Fixed error when running in parallel on windows (was failing to export
needed functions)

## jaggernaut 1.5.2

- Upgraded to `datalist` 0.2.
