
<!-- README.md is generated from README.Rmd. Please edit that file -->

# conogive <img src="man/figures/logo.png" align="right" width="140" height="70" />

[![Build
Status](https://travis-ci.com/JonasMoss/conogive.svg?branch=master)](https://travis-ci.com/JonasMoss/conogive)
[![AppVeyor Build
Status](https://ci.appveyor.com/api/projects/status/github/JonasMoss/conogive?branch=master&svg=true)](https://ci.appveyor.com/project/JonasMoss/conogive)
[![Project Status: WIP – Initial development is in progress, but there
has not yet been a stable, usable release suitable for the
public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
[![Codecov test
coverage](https://codecov.io/gh/JonasMoss/conogive/branch/master/graph/badge.svg)](https://codecov.io/gh/JonasMoss/conogive?branch=master)

<!--[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)-->

An `R` package for the congeneric ogive model.

## Overview

The congeneric ogive model is a psychometric model for Likert scale data
with one latent factor. This package has functions to estimate such
models, calculate their ordinal reliabilities, and make predictions.

## Installation

From inside `R`, use one of the following commands:

``` r
devtools::install_github("JonasMoss/conogive")
```

## Usage

Estimate a congeneric ogive model with `congive`, predict the value of
the latent factor with `predict`, and calculate the reliability with
`ordinal_omega`.

``` r
library("conogive")
extraversion = psychTools::bfi[c("E1", "E2", "E3", "E4", "E5")]
extraversion[, "E1"] = 7 - extraversion[, "E1"] # Reverse-coded item.
extraversion[, "E2"] = 7 - extraversion[, "E2"] # Reverse-coded item.

object = conogive(extraversion)

ordinal_omega(object) # Observed reliability
#> [1] 0.7046056
omega(object) # Theoretical reliability
#> [1] 0.8122608

hist(predict(object, extraversion)) # Plot distribution of predictions.
```

<img src="man/figures/README-estimate-1.png" width="750px" />
