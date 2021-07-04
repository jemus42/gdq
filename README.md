
<!-- README.md is generated from README.Rmd. Please edit that file -->

# gdqdonations

<!-- badges: start -->

[![R-CMD-check](https://github.com/jemus42/gdqdonations/workflows/R-CMD-check/badge.svg)](https://github.com/jemus42/gdqdonations/actions)
<!-- badges: end -->

The goal of `gdqdonations` is to to gather data from the GDQ tracker,
and also gdqvods.com

## Installation

Install from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("jemus42/gdqdonations")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(gdqdonations)

# Included event dates for convenience
event_dates
#> # A tibble: 22 x 4
#>    event    start               end                 event_duration
#>    <chr>    <dttm>              <dttm>                       <dbl>
#>  1 AGDQ2011 2011-01-06 00:00:00 2011-01-11 00:00:00              5
#>  2 AGDQ2012 2012-01-04 00:00:00 2012-01-09 00:00:00              5
#>  3 AGDQ2013 2013-01-06 00:00:00 2013-01-12 00:00:00              6
#>  4 AGDQ2014 2014-01-05 00:00:00 2014-01-11 00:00:00              6
#>  5 AGDQ2015 2015-01-04 00:00:00 2015-01-10 00:00:00              6
#>  6 AGDQ2016 2016-01-03 00:00:00 2016-01-10 00:00:00              7
#>  7 AGDQ2017 2017-01-08 00:00:00 2017-01-15 00:00:00              7
#>  8 AGDQ2018 2018-01-07 00:00:00 2018-01-14 00:00:00              7
#>  9 AGDQ2019 2019-01-06 00:00:00 2019-01-12 00:00:00              6
#> 10 AGDQ2020 2020-01-05 00:00:00 2020-01-12 00:00:00              7
#> # … with 12 more rows
```
