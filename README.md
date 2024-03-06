
<!-- README.md is generated from README.Rmd. Please edit that file -->

# nhmgrid: An R package for visualization of non-homogeneous Markov model probabilities

<!-- badges: start -->

[![Project Status: Inactive – The project has reached a stable, usable
state but is no longer being actively developed; support/maintenance
will be provided as time
allows.](https://www.repostatus.org/badges/latest/inactive.svg)](https://www.repostatus.org/#inactive)
<!-- badges: end -->

The `nhmgrid` R package provides an easy-to-use interface for estimation
and visualization of non-homogeneous Markov model transition
probabilities. The main features in the package are:

- Supports a variety of model types in transition probability estimation
  using marginaleffects.
- Is able to calculate state transition proportions without a model.
- Produces highly customizable plots using ggplot2.

## Installation

You can install the most recent version of `nhmgrid` by running the
following lines:

``` r
# install.packages("devtools")
devtools::install_github("mirojantti/nhmgrid")
```

## Example

The package contains a simulated panel data set `health`. The data set
consists of 100 individuals whose health state has been measured every
year for 10 years.

``` r
library(nhmgrid)

head(health, 12)
#>    id    sex age  state lagstate
#> 1   1 female  15  terve     <NA>
#> 2   1 female  16 sairas    terve
#> 3   1 female  17 sairas   sairas
#> 4   1 female  18  terve   sairas
#> 5   1 female  19  terve    terve
#> 6   1 female  20  terve    terve
#> 7   1 female  21 sairas    terve
#> 8   1 female  22 sairas   sairas
#> 9   1 female  23  terve   sairas
#> 10  1 female  24 sairas    terve
#> 11  2 female  20  terve     <NA>
#> 12  2 female  21  terve    terve
nrow(health)
#> [1] 1000
```

One can calculate the transition proportions between the health states
using `stprops` and plot them. Because the plot is constructed using
ggplot2, it is highly customizable.

``` r
props <- stprops(health, x = "age", state = "state", id = "id")
plot(props) +
  ggplot2::geom_smooth(se = FALSE)
```

<img src="man/figures/README-props-1.png" style="display: block; margin: auto;" />

The transition probabilities between the states can be estimated with a
Markov model using `stprobs`. In this example we fit a multinomial
logistic regression model and estimate the probabilities separately for
men and women.

``` r
fit <- nnet::multinom(state ~ lagstate + age + sex, data = health)
probs <- stprobs(fit, x = "age", group = "sex")
plot(probs)
```

<img src="man/figures/README-probs-1.png" style="display: block; margin: auto;" />

For more examples, see the package documentation.
