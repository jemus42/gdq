
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
# install.packages("remotes")
remotes::install_github("jemus42/gdqdonations")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(gdqdonations)

# Included event dates for convenience
tibble::glimpse(event_index)
#> Rows: 22
#> Columns: 8
#> $ event                <chr> "AGDQ2011", "AGDQ2012", "AGDQ2013", "AGDQ2014", "…
#> $ start                <dttm> 2011-01-06, 2012-01-04, 2013-01-06, 2014-01-05, …
#> $ end                  <dttm> 2011-01-11, 2012-01-09, 2013-01-12, 2014-01-11, …
#> $ event_duration       <dbl> 5, 5, 6, 6, 6, 7, 7, 7, 6, 7, 7, 2, 4, 5, 6, 7, 6…
#> $ event_name           <chr> "Awesome Games Done Quick 2011", "Awesome Games D…
#> $ tracker_run_url      <chr> "/tracker/runs/agdq2011", "/tracker/runs/agdq2012…
#> $ tracker_donation_url <chr> "/tracker/donations/agdq2011", "/tracker/donation…
#> $ event_slug           <chr> "agdq2011", "agdq2012", "agdq2013", "agdq2014", "…
```
