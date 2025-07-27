
<!-- README.md is generated from README.Rmd. Please edit that file -->

# gdqdonations

<!-- badges: start -->

[![R-CMD-check](https://github.com/jemus42/gdqdonations/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/jemus42/gdqdonations/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of `gdqdonations` is to to gather data from the GDQ tracker,
and also gdqvods.com

## Installation

Install from [GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("jemus42/gdqdonations")
```

## Example

This is a basic example:

``` r
library(gdqdonations)

# Included event dates for convenience
tibble::glimpse(event_index)
#> Rows: 30
#> Columns: 8
#> $ event                <chr> "AGDQ2011", "SGDQ2011", "AGDQ2012", "SGDQ2012", "…
#> $ start                <dttm> 2011-01-06 00:00:00, 2011-08-04 19:00:00, 2012-0…
#> $ end                  <dttm> 2011-01-11 00:00:00, 2011-08-06 23:07:31, 2012-0…
#> $ event_duration       <dbl> 5.000000, 2.171887, 2.802083, 3.884028, 6.401921,…
#> $ event_name           <chr> "Awesome Games Done Quick 2011", "Summer Games Do…
#> $ tracker_run_url      <chr> "/tracker/runs/agdq2011", "/tracker/runs/sgdq2011…
#> $ tracker_donation_url <chr> "/tracker/donations/agdq2011", "/tracker/donation…
#> $ event_slug           <chr> "agdq2011", "sgdq2011", "agdq2012", "sgdq2012", "…
```
