
<!-- README.md is generated from README.Rmd. Please edit that file -->

[![Lifecycle:
deprecated](https://img.shields.io/badge/lifecycle-deprecated-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#deprecated)
[![License:
MIT](https://img.shields.io/badge/License-MIT-blue.svg)](https://opensource.org/licenses/MIT)

# jaggernaut

**DEPRECATED**

Development has moved to
[jmbr](https://github.com/poissonconsulting/jmbr).

## Introduction

jaggernaut is an R package to facilitate Bayesian analyses using JAGS
(Just Another Gibbs Sampler).

Key features include

-   the conversion of a data frame into a suitable format for input into
    JAGS

-   the option to automatically update a model (increase the length of
    the MCMC chains) until convergence is achieved

-   the option to run MCMC chains on parallel processes

-   the ability to extract derived parameters from an existing model
    using BUGS code (in the JAGS dialect) without additional MCMC
    sampling

-   simple generation of data frames quantifying the effect (and effect
    size) of particular variables with the other variables held constant

## Installation

To install from GitHub

    # install.packages("devtools")
    devtools::install_github("poissonconsulting/jaggernaut")

## Contribution

Please report any
[issues](https://github.com/poissonconsulting/jaggernaut/issues).

[Pull requests](https://github.com/poissonconsulting/jaggernaut/pulls)
are always welcome.
