
<!-- README.md is generated from README.Rmd. Please edit that file -->
[![Travis-CI Build Status](https://travis-ci.org/poissonconsulting/jaggernaut.svg?branch=master)](https://travis-ci.org/poissonconsulting/jaggernaut) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/poissonconsulting/jaggernaut?branch=master&svg=true)](https://ci.appveyor.com/project/poissonconsulting/jaggernaut) [![codecov](https://codecov.io/gh/poissonconsulting/jaggernaut/branch/master/graph/badge.svg)](https://codecov.io/gh/poissonconsulting/jaggernaut)

jaggernaut
==========

Introduction
------------

jaggernaut is an R package to facilitate Bayesian analyses using JAGS (Just Another Gibbs Sampler).

Key features include

-   the conversion of a data frame into a suitable format for input into JAGS

-   the option to automatically update a model (increase the length of the MCMC chains) until convergence is achieved

-   the option to run MCMC chains on parallel processes

-   the ability to extract derived parameters from an existing model using BUGS code (in the JAGS dialect) without additional MCMC sampling

-   simple generation of data frames quantifying the effect (and effect size) of particular variables with the other variables held constant

Installation
------------

To install from GitHub

    # install.packages("devtools")
    devtools::install_github("poissonconsulting/jaggernaut")

Contribution
------------

Please report any [issues](https://github.com/poissonconsulting/jaggernaut/issues).

[Pull requests](https://github.com/poissonconsulting/jaggernaut/pulls) are always welcome.
