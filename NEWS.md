# NEWS

# jaggernaut 1.8.1

- fixed minor bug where printing aggregation object
- updated loaded package versions

# jaggernaut 1.8.0

- added `jags_discrepancies()` function and `aggregative_code` argument
to allow calculation of posterior predictive checking
- sets mode to report on load

# jaggernaut 1.7.1

- added `fritillary` demo

# jaggernaut 1.7

- on load package sets up mode reporting by default
- max number of simulations saved now 6,000 (was 2,000)

# jaggernaut 1.6

* refer to git history...

# jaggernaut 1.5.6

* `predict` function now gives observed base values when `base = TRUE` and
`obs_by = TRUE`

# jaggernaut 1.5.5

* `obs_by` argument in `predict` functions now works for continuous 
variables (previously only reliable for categorical variables while returning
a subset of observed values for continuous variables)

# jaggernaut 1.5.4

* `update` now runs chains in parallel (if workers available)

# jaggernaut 1.5.3

* Fixed error when running in parallel on windows (was failing to export
needed functions)

# jaggernaut 1.5.2

* Upgraded to `datalist` 0.2.

# Bug Reports 

For more fine-grained list of changes or to report a bug, consult 

* [The commit log](https://github.com/joethorley/jaggernaut/commits/master)
* [The issues log](https://github.com/joethorley/jaggernaut/issues)

# Versioning

Releases are numbered with the following semantic versioning format:

\<major\>.\<minor\>.\<patch\>

And constructed with the following guidelines:

* Breaking backward compatibility bumps the major (and resets the minor 
  and patch)
* New additions without breaking backward compatibility bumps the minor 
  (and resets the patch)
* Bug fixes and misc changes bumps the patch

For more information on jaggernaut, please visit: 
https://github.com/joethorley/jaggernaut/wiki.
