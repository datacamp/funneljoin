
<!-- README.md is generated from README.Rmd. Please edit that file -->
funneljoin
==========

The goal of funneljoin is to ...

Installation
------------

You can install the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("datacamp/funneljoin")
```

Example
-------

Rules
-----

-   If type\_x is "last" or "first", then a right join has the same number of rows as y.
-   If type\_y is "last", "first", or "firstafter", then a left join has the same number of rows as x.

Types of joins
--------------

-   first-first: You can take earliest x and y for each user before the join (entered an experiment, then registered)
-   first-firstafter: Take the first x, then the first y after that (entered an experiment, then started a course)
-   lastbefore-firstafter: First x that's followed by a y before the next x (last click paid ad attribution)
-   last-firstafter: Take the last X followed by the first Y afterward
-   any-firstafter: All Xs followed by the first Y after it.
-   any-any: All Xs followed by all Ys.

Gaps: \* smallestgap: Smallest x-y gap (in tie, earliest). \* withingap: Gap is less than X (all ad clicks followed by a course start within an hour).
