
<!-- README.md is generated from README.Rmd. Please edit that file -->
[![Travis-CI Build Status](https://travis-ci.org/robinsones/funneljoin.svg?branch=master)](https://travis-ci.org/robinsones/funneljoin)

The goal of funneljoin is to make it easy to analyze behavior funnels. For example, maybe you're interested in finding the people who visit a page and then register. Or you want all the times people click on an item and add it to their cart within 2 days. These can all be answered quickly with funneljoin's `after_join()` or `funnel_start()` and `funnel_step()`. As funneljoin uses dplyr, it can also work with remote tables, **but has only been tried on postgres**.

For more examples of how to use funneljoin, check out [the vignette](https://robinsones.github.io/funneljoin/articles/funneljoin.html), which shows different types of joins and the optional arguments, or this [blog post](https://hookedondata.org/introducing-the-funneljoin-package/), which showcases how to use funneljoin analyze questions and answers on StackOverflow.

Installation
------------

You can install the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("robinsones/funneljoin")
```

after\_join()
-------------

``` r
library(dplyr)
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

Let's say we wanted to get only the first time people landed and registered. We would `after_inner_join()` with a `first-first` type:

``` r
landed %>%
  after_inner_join(registered, 
                   by_user = "user_id",
                   by_time = "timestamp",
                   type = "first-first",
                   suffix = c("_landed", "_registered"))
#> # A tibble: 4 x 3
#>   user_id timestamp_landed timestamp_registered
#>     <dbl> <date>           <date>              
#> 1       1 2018-07-01       2018-07-02          
#> 2       3 2018-07-02       2018-07-02          
#> 3       6 2018-07-07       2018-07-10          
#> 4       5 2018-07-10       2018-07-11
```

The first two arguments are the tables we're joining, with the first table being the events that happen first. We then specify:

-   `by_time`: the time columns in each table. This would typically be a datetime or a date column. These columns are used to filter for time y being after time x.
-   `by_user`:the user or identity columns in each table. These must be identical for a pair of rows to match.
-   `type`: the type of funnel used to distinguish between event pairs, such as "first-first", "last-first", "any-firstafter".
-   `suffix`: just like dplyr’s join functions, this specifies what should be appended to the names of columns that are in both tables.

`type` can be any combination of `first`, `last`, `any`, and `lastbefore` with `first`, `last`, `any`, and `firstafter`. Some common ones you may use include:

-   **first-first**: Take the earliest x and y for each user **before** joining. For example, you want the first time someone entered an experiment, followed by the first time someone **ever** registered. If they registered, entered the experiment, and registered again, you do not want to include that person.
-   **first-firstafter**: Take the first x, then the first y after that. For example, you want when someone first entered an experiment and the first course they started afterwards. You don't care if they started courses before entering the experiment.
-   **lastbefore-firstafter**: First x that's followed by a y before the next x. For example, in last click paid ad attribution, you want the last ad someone clicked before the first subscription they did afterward.
-   **any-firstafter**: Take all Xs followed by the first Y after it. For example, you want all the times someone visited a homepage and their first product page they visited afterwards.
-   **any-any**: Take all Xs followed by all Ys. For example, you want all the times someone visited a homepage and **all** the product pages they saw afterward.

If your time and user columns have different names, you can work with that too:

``` r
landed <- landed %>%
  rename(landed_at = timestamp,
         user_id_x = user_id)

registered <- registered %>%
  rename(registered_at = timestamp,
         user_id_y = user_id)
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

funnel\_start() and funnel\_step()
----------------------------------

Sometimes you have all the data you need in one table. For example, let's look at this table of user activity on a website.

``` r
activity <- tibble::tribble(
  ~ "user_id", ~ "event", ~ "timestamp",
  1, "landing", "2019-07-01",
  1, "registration", "2019-07-02",
  1, "purchase", "2019-07-07",
  1, "purchase", "2019-07-10",
  2, "landing", "2019-08-01",
  2, "registration", "2019-08-15",
  3, "landing", "2019-05-01",
  3, "registration", "2019-06-01",
  3, "purchase", "2019-06-04",
  4, "landing", "2019-06-13"
)
```

We can use `funnel_start()` and `funnel_step()` to make an activity funnel. `funnel_start()` takes five arugments:

-   `tbl`: The table of events.
-   `moment_type`: The first moment, or event, in the funnel.
-   `moment`: The name of the column that indicates the `moment_type`.
-   `tstamp`: The name of the column with the timestamps of the moment.
-   `user`: The name of the column indicating the user who did the moment.

``` r
activity %>%
  funnel_start(moment_type = "landing", 
               moment = "event", 
               tstamp = "timestamp", 
               user = "user_id")
#> # A tibble: 4 x 2
#>   user_id timestamp_landing
#>     <dbl> <chr>            
#> 1       1 2019-07-01       
#> 2       2 2019-08-01       
#> 3       3 2019-05-01       
#> 4       4 2019-06-13
```

`funnel_start()` returns a table with the user\_ids and a column with the name of your timestamp column, `_`, and the moment type. This table also includes metadata.

To add more moments to the funnel, you use `funnel_step()`. Since you've indicated in `funnel_start()` what columns to use for each part, now you only need to have the `moment_type` and the `type` of `after_join()` (e.g. "first-first", "first-any").

``` r
activity %>%
  funnel_start(moment_type = "landing", 
               moment = "event", 
               tstamp = "timestamp", 
               user = "user_id") %>%
  funnel_step(moment_type = "registration",
              type = "first-firstafter")
#> # A tibble: 4 x 3
#>   user_id timestamp_landing timestamp_registration
#>     <dbl> <chr>             <chr>                 
#> 1       3 2019-05-01        2019-06-01            
#> 2       4 2019-06-13        <NA>                  
#> 3       1 2019-07-01        2019-07-02            
#> 4       2 2019-08-01        2019-08-15
```

You can continue stacking on `funnel_step()` with more moments.

``` r
activity %>%
  funnel_start(moment_type = "landing", 
               moment = "event", 
               tstamp = "timestamp", 
               user = "user_id") %>%
  funnel_steps(moment_type = "registration",
              type = "first-firstafter") %>%
  funnel_step(moment_type = "purchase",
              type = "first-any")
#> # A tibble: 5 x 4
#>   user_id timestamp_landing timestamp_registration timestamp_purchase
#>     <dbl> <chr>             <chr>                  <chr>             
#> 1       3 2019-05-01        2019-06-01             2019-06-04        
#> 2       1 2019-07-01        2019-07-02             2019-07-07        
#> 3       1 2019-07-01        2019-07-02             2019-07-10        
#> 4       2 2019-08-01        2019-08-15             <NA>              
#> 5       4 2019-06-13        <NA>                   <NA>
```

If you use a `type` that allows multiple moments of one type for a user, like "first-any", you will get more rows per user rather than more columns. For example, user 1 had two purchases, so she now has two rows. The `timestamp_landing` and `timestamp_registration` is the same for both rows, but they have a different `timestamp_purchase`.

Finally, you can use the `summarize_funnel()` to understand how many and what percentage of people make it through to each next step of the funnel. We can also switch to `funnel_steps()` to shorten our code a bit - we give it a character vector of `moment_types` in order and the `type` for each step.

``` r
activity %>%
  funnel_start(moment_type = "landing", 
               moment = "event", 
               tstamp = "timestamp", 
               user = "user_id") %>%
  funnel_steps(moment_types = c("registration", "purchase"),
              type = "first-firstafter") %>%
  summarize_funnel()
#> # A tibble: 3 x 4
#>   moment_type  nb_step pct_cumulative pct_step
#>   <fct>          <int>          <dbl>    <dbl>
#> 1 landing            4           1      NA    
#> 2 registration       3           0.75    0.75 
#> 3 purchase           2           0.5     0.667
```

`nb_step` is how many users made it to each step, `pct_cumulative` is what percent that is out of the original step, and `pct_step` is what percentage that is out of those who made it to the previous step. So in our case, 2 people had a purchase, which is 50% of the people who landed but 66% of those who registered.

Reporting bugs and adding features
----------------------------------

If you find any bugs or have a feature request or question, please [create an issue](https://github.com/datacamp/funneljoin/issues/new). If you'd like to add a feature, tests, or other functionality, please also make an issue first and let's discuss!
