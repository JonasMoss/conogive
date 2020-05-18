
<!-- README.md is generated from README.Rmd. Please edit that file -->

# conogive <img src="man/figures/logo.png" align="right" width="140" height="70" />

[![Build
Status](https://travis-ci.com/JonasMoss/conogive.svg?branch=master)](https://travis-ci.com/JonasMoss/conogive)
[![AppVeyor Build
Status](https://ci.appveyor.com/api/projects/status/github/JonasMoss/conogive?branch=master&svg=true)](https://ci.appveyor.com/project/JonasMoss/conogive)

[![Codecov test
coverage](https://codecov.io/gh/JonasMoss/conogive/branch/master/graph/badge.svg)](https://codecov.io/gh/JonasMoss/conogive?branch=master)
[![DOI](https://zenodo.org/badge/244635248.svg)](https://zenodo.org/badge/latestdoi/244635248)
[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)

An `R` package for the congeneric normal-ogive model.

## Overview

The congeneric normal-ogive model (McDonald, R. P., 1997) is a
psychometric model for Likert data with one latent factor. This package
has functions to estimate such models, calculate their ordinal
reliabilities, and make predictions.

## Installation

From inside `R`, use one of the following commands:

``` r
devtools::install_github("JonasMoss/conogive")
```

## Usage

Estimate a congeneric normal-ogive model with `congive`, predict the
value of the latent factor with `predict`, and calculate the reliability
with `ordinal_omega`.

``` r
library("conogive")
extraversion = psychTools::bfi[c("E1", "E2", "E3", "E4", "E5")]
extraversion[, "E1"] = 7 - extraversion[, "E1"] # Reverse-coded item.
extraversion[, "E2"] = 7 - extraversion[, "E2"] # Reverse-coded item.

object = conogive(extraversion)

ordinal_omega(object) # Observed reliability
#> [1] 0.7046056
omega(object) # Theoretical reliability
#> [1] 0.8122607

hist(predict(object, extraversion)) # Plot distribution of predictions.
```

<img src="man/figures/README-estimate-1.png" width="750px" />

## References

McDonald, R. P. (1997). Normal-ogive multidimensional model. In W. J.
van der Linden & R. K. Hamble-ton (Eds.), Handbook of modern item
response theory (pp. 257–269). Springer.
<https://doi.org/10.1007/978-1-4757-2691-6_15>
