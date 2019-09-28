
# aRlegislation

[![Build
Status](https://travis-ci.com/titaniumtroop/aRlegislation.svg?branch=master)](https://travis-ci.com/titaniumtroop/aRlegislation)

This package provides a dataset of Arkansas legislation from 2001-2019
for data exploration, natural language processing, and graph analysis.
The dataset is structured as a nested tibble containing distinct
legislative sessions, nested by cycle (every two years) and session
type.

Because the package uses nested tibbles, the `tidyr` package is required
to use the dataset:

``` r
library(aRlegislation)
library(tidyr) # needed for nest/unnest operations
```

The structure of the dataset is:

``` r
head(legislation)
```

    ## # A tibble: 6 x 5
    ##   cycle session acts                 lawmakers          sponsorship        
    ##   <dbl> <chr>   <list>               <list>             <list>             
    ## 1  2001 R       <tibble [1,843 × 6]> <tibble [136 × 11… <tibble [4,814 × 2…
    ## 2  2001 S1      <tibble [2 × 6]>     <tibble [86 × 11]> <tibble [86 × 2]>  
    ## 3  2003 R       <tibble [1,816 × 6]> <tibble [135 × 11… <tibble [5,683 × 2…
    ## 4  2003 S1      <tibble [63 × 6]>    <tibble [6 × 11]>  <tibble [66 × 2]>  
    ## 5  2003 S2      <tibble [110 × 6]>   <tibble [124 × 11… <tibble [473 × 2]> 
    ## 6  2005 R       <tibble [2,325 × 6]> <tibble [135 × 11… <tibble [6,128 × 2…

There are three different datasets for each legislative cycle and
session:

  - `acts`: contains act number, bill number, title, subtitle, text, and
    partisanship
  - `lawmakers`: contains demographic information about elected senators
    and representatives such as chamber, political party, and district
  - `sponsorship`: a simple table containing act number and sponsor name

The vignettes provide more detail about each type of nested tibble, and
also provide an example or two of analyses that can be performed using
each type of tibble.

# Future Plans

The legislature meets once a year, plus special sessions, so the dataset
can be updated as each additional session concludes.

This dataset is essentially an aggregation of distinct datasets across
each cycle/session of the legislature. This works well for seeing trends
across cycles/sessions, but it doesn’t give much visibility into how
individual lawmakers act as they get re-elected, gain experience, and
perhaps graduate from House to Senate as term limits force them out of
office. As such, entity resolution of lawmakers across cycles/sessions
will likely be the focus of the next major version of this dataset.
