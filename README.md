
<!-- README.md is generated from README.Rmd. Please edit that file -->
[![Travis-CI Build Status](https://travis-ci.org/datacamp/funneljoin.svg?branch=master)](https://travis-ci.org/datacamp/funneljoin)

**Disclaimer**: funneljoin is still in active development. While we are frequently using it at DataCamp, there are still some bugs, especially with remote tables. If you can, we recommend pulling your data to work with locally. And **do not** do multiple funnel joins in a row remotely (it's fine locally).

Overview
========

The goal of funneljoin is to make it easy to analyze behavior funnels. For example, maybe you're interested in finding the people who visit a page and then register. Or you want all the times people click on an item and add it to their cart within 2 days.

You can do this with funneljoin's `after_join()` function. The arguments are:

-   `x`: a dataset with the first set of behaviors.
-   `y`: a dataset with the second set of behaviors.
-   `by_time`: a character vector to specify the time columns in x and y. Must be a single column in each tbl. Note that this column is used to filter for time y &gt;= time x.
-   `by_user`: a character vector to specify the user or identity columns in x and y. Must be a single column in each tbl.
-   `mode`: the method used to join: "inner", "full", "anti", "semi", "right", "left".
-   `type`: the type of funnel used to distinguish between event pairs, such as "first-first", "last-first", "any-firstafter". See types of funnels.
-   `max_gap` (optional): the maximum gap between events. Can be a integer representing the number of seconds or a difftime object
-   `gap_col` (optional): whether to return a numeric column, `.gap`, with the time difference in seconds between the events. Defaults to `FALSE`.

As funneljoin uses dplyr, it can also work with remote tables, **but has only been tried on postgres**. Some of the functionality, especially the gap joins, use SQL code that may not work with different versions.

Installation
------------

You can install the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("datacamp/funneljoin")
```

Types of funnels
----------------

`type` can be any combination of `first`, `last`, `any`, and `lastbefore` with with `first`, `last`, `any`, and `firstafter`. Some common ones you may use include:

-   **first-first**: Take the earliest x and y for each user **before** joining. For example, you want the first time someone entered an experiment, followed by the first time someone **ever** registered. If they registered, entered the experiment, and registered again, you do not want to include that person.
-   **first-firstafter**: Take the first x, then the first y after that. For example, you want when someone first entered an experiment and the first course they started afterwards. You don't care if they started courses before entering the experiment.
-   **lastbefore-firstafter**: First x that's followed by a y before the next x. For example, in last click paid ad attribution, you want the last time someone clicked an ad before the first subscription they did afterward.
-   **any-firstafter**: Take all Xs followed by the first Y after it. For example, you want all the times someone visited a homepage and their first product page they visited afterwards.
-   **any-any**: Take all Xs followed by all Ys. For example, you want all the times someone visited a homepage and **all** the product pages they saw afterward.

See the vignette for more examples.

### Example

``` r
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
library(funneljoin)
```

``` r
landed <- tibble::tribble(
  ~user_id, ~timestamp,
  1, "2018-07-01",
  2, "2018-07-01",
  3, "2018-07-02",
  4, "2018-07-01",
  4, "2018-07-04",
  5, "2018-07-10",
  5, "2018-07-12",
  6, "2018-07-07",
  6, "2018-07-08"
) %>%
  mutate(timestamp = as.Date(timestamp))

registered <- tibble::tribble(
  ~user_id, ~timestamp,
  1, "2018-07-02",
  3, "2018-07-02",
  4, "2018-06-10",
  4, "2018-07-02",
  5, "2018-07-11",
  6, "2018-07-10",
  6, "2018-07-11",
  7, "2018-07-07"
) %>%
  mutate(timestamp = as.Date(timestamp))
```

Let's say we wanted to get only the first time people landed and registered. We would use a `first-first inner` join:

``` r
landed %>%
  after_inner_join(registered, 
                   by_user = "user_id",
                   by_time = "timestamp",
                   type = "first-first")
#> # A tibble: 4 x 3
#>   user_id timestamp.x timestamp.y
#>     <dbl> <date>      <date>     
#> 1       1 2018-07-01  2018-07-02 
#> 2       3 2018-07-02  2018-07-02 
#> 3       6 2018-07-07  2018-07-10 
#> 4       5 2018-07-10  2018-07-11
```

If your time and user columns have different names, you can work with that too:

``` r
landed <- tibble::tribble(
  ~user_id_x, ~landed_at,
  1, "2018-07-01",
  2, "2018-07-01",
  3, "2018-07-02",
  4, "2018-07-01",
  4, "2018-07-04",
  5, "2018-07-10",
  5, "2018-07-12",
  6, "2018-07-07",
  6, "2018-07-08"
) %>%
  mutate(landed_at = as.Date(landed_at))

registered <- tibble::tribble(
  ~user_id_y, ~registered_at,
  1, "2018-07-02",
  3, "2018-07-02",
  4, "2018-06-10",
  4, "2018-07-02",
  5, "2018-07-11",
  6, "2018-07-10",
  6, "2018-07-11",
  7, "2018-07-07"
) %>%
  mutate(registered_at = as.Date(registered_at))
```

``` r
landed %>%
  after_inner_join(registered, 
                   by_user = c("user_id_x" = "user_id_y"),
                   by_time = c("landed_at" = "registered_at"),
                   type = "first-first")
#> # A tibble: 4 x 3
#>   user_id_x landed_at  registered_at
#>       <dbl> <date>     <date>       
#> 1         1 2018-07-01 2018-07-02   
#> 2         3 2018-07-02 2018-07-02   
#> 3         6 2018-07-07 2018-07-10   
#> 4         5 2018-07-10 2018-07-11
```

Summarizing funnels
-------------------

Funneljoin also contains to functions to summarize funnels: `summarize_conversions()` and `summarize_prop_tests()`- see the vignette for details.

Reporting bugs and adding features
----------------------------------

If you find any bugs or have a feature request or question, please [create an issue](https://github.com/datacamp/funneljoin/issues/new). If you'd like to add a feature, tests, or other functionality, please make an issue first and let's discuss!
