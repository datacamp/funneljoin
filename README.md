
<!-- README.md is generated from README.Rmd. Please edit that file -->
**Disclaimer**: funneljoin is still in active development. While we are using it at DataCamp, there are still some bugs, especially with remote tables. If you can, we recommend pulling your data to work with locally. And **do not** do multiple funnel joins in a row remotely (it's fine locally).

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

Paired funneljoins can be any combination of `first`, `last`, `any`, and `lastbefore`, combined with `first`, `last`, `any`, and `firstafter`. Some common ones you may use include:

-   **first-first**: Take the earliest x and y for each user **before** joining. For example, you want the first time someone entered an experiment, followed by the first time someone **ever** registered. If they registered, entered the experiment, and registered again, you do not want to include that person.
-   **first-firstafter**: Take the first x, then the first y after that. For example, you want when someone first entered an experiment and the first course they started afterwards. You don't care if they started courses before entering the experiment.
-   **lastbefore-firstafter**: First x that's followed by a y before the next x. For example, in last click paid ad attribution, you want the last time someone clicked an ad before the first subscription they did afterward.
-   **any-firstafter**: Take all Xs followed by the first Y after it. For example, you want all the times someone visited a homepage and their first product page they visited afterwards.
-   **any-any**: Take all Xs followed by all Ys. For example, you want all the times someone visited a homepage and **all** the product pages they saw afterward.

See the vignette for more.

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

Rules
-----

Some rules to keep in mind:

-   If type\_x is "last" or "first", then a right join has the same number of rows as y.
-   If type\_y is "last", "first", or "firstafter", then a left join has the same number of rows as x.

Summarizing funnels
-------------------

Funneljoin also contains to functions to summarize funnels: `summarize_conversions` (see the vignette) and `summarize_prop_tests()`.

`summarize_prop_tests()` takes in a dataset with at least three columns - `alternative.name`, `nb_starts`, and `nb_conversions`. It can also have an additional column that is the type of conversion - for example, you could have clicks and purchases. Each type of conversion can only have two rows, one `control` and one other group. If you have that additional column of type, you need to group by it first.

It returns a dataset with X columns:

-   `control`: the conversion rate of the control group
-   `treatment`: the conversion rate of the treatment group
-   `p_value` of the proportion test
-   `pct_change`: the percentage difference between the control and treatment group
-   `pct_change_low` and `pct_change_high`: the bayesian estimates for a 90% confidence interval.

If you had a type column, it will also be in the output.

``` r
tbl <- tibble::tribble(
  ~ alternative.name, ~nb_starts, ~nb_conversions, ~type,
  "control", 500, 200, "purchase",
  "treatment", 500, 100, "purchase", 
  "control", 500, 360, "click",
  "treatment", 500, 375, "click"
)

tbl %>%
  group_by(type) %>%
  summarize_prop_tests()
#> # A tibble: 2 x 7
#>   type  control treatment  p_value pct_change pct_change_low
#>   <chr>   <dbl>     <dbl>    <dbl>      <dbl>          <dbl>
#> 1 click    0.72      0.75 3.16e- 1     0.0417        -0.0248
#> 2 purc…    0.4       0.2  8.39e-12    -0.5           -0.621 
#> # ... with 1 more variable: pct_change_high <dbl>
```

Reporting bugs and adding features
----------------------------------

If you find any bugs or have a feature request or question, please [create an issue](https://github.com/datacamp/funneljoin/issues/new). If you'd like to add a feature, tests, or other functionality, please make an issue first and let's discuss!
