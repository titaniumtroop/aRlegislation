
# aRlegislation

[![Build
Status](https://travis-ci.com/titaniumtroop/aRlegislation.svg?branch=master)](https://travis-ci.com/titaniumtroop/aRlegislation)

This package provides a dataset of Arkansas legislation from 2001-2019
for data exploration, natural language processing, and graph analysis.
Pre-calculated topic models are provided; however, these files are
large, so only a few are included in the package build.

The primary datasets is structured as a nested tibble containing
distinct legislative sessions, nested by cycle (every two years) and
session type. Because the package uses nested tibbles, the `tidyr`
package is required to use the dataset:

``` r
library(aRlegislation)
library(tidyr) # needed for nest/unnest operations
```

## `legislation` dataset

The structure of the `legislation` dataset is:

``` r
head(legislation)
```

    ## # A tibble: 6 x 5
    ##   cycle session acts                 lawmakers           sponsorship         
    ##   <dbl> <chr>   <list>               <list>              <list>              
    ## 1  2001 R       <tibble [1,843 × 6]> <tibble [136 × 11]> <tibble [4,814 × 2]>
    ## 2  2001 S1      <tibble [2 × 6]>     <tibble [86 × 11]>  <tibble [86 × 2]>   
    ## 3  2003 R       <tibble [1,816 × 6]> <tibble [135 × 11]> <tibble [5,683 × 2]>
    ## 4  2003 S1      <tibble [63 × 6]>    <tibble [6 × 11]>   <tibble [66 × 2]>   
    ## 5  2003 S2      <tibble [110 × 6]>   <tibble [124 × 11]> <tibble [473 × 2]>  
    ## 6  2005 R       <tibble [2,325 × 6]> <tibble [135 × 11]> <tibble [6,128 × 2]>

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

## `lawmakers` dataset

The structure of the `lawmakers` dataset is:

``` r
head(lawmakers)
```

    ## # A tibble: 6 x 4
    ##   sponsor_full_name count sessions          corruption
    ##   <chr>             <int> <list>            <lgl>     
    ## 1 Aaron Burkes          1 <tibble [1 × 12]> FALSE     
    ## 2 Aaron Pilkington      4 <tibble [4 × 12]> FALSE     
    ## 3 Alan Clark            7 <tibble [7 × 12]> FALSE     
    ## 4 Allen Kerr            5 <tibble [5 × 12]> FALSE     
    ## 5 Allen Maxwell         6 <tibble [6 × 12]> FALSE     
    ## 6 Alvin Simes           4 <tibble [4 × 12]> FALSE

This dataset provides entity resolution for the sponsors of legislation
across the cycles and sessions contained in the `legislation` dataset.
Per-cycle/session information is nested in the dataframe. A corruption
flag has been added for each legislator; the source code contains links
in the comments to the news articles and U.S. attorney press releases
announcing public corruption charges/convictions.

## `legislation_corpus` dataset

The structure of the `legislation_corpus` dataset is:

``` r
head(legislation_corpus)
```

    ## # A tibble: 6 x 5
    ## # Groups:   cycle, session [6]
    ##   cycle session title_corpus text_corpus text_stemmed
    ##   <dbl> <chr>   <list>       <list>      <list>      
    ## 1  2001 R       <VCorpus>    <VCorpus>   <VCorpus>   
    ## 2  2001 S1      <VCorpus>    <VCorpus>   <VCorpus>   
    ## 3  2003 R       <VCorpus>    <VCorpus>   <VCorpus>   
    ## 4  2003 S1      <VCorpus>    <VCorpus>   <VCorpus>   
    ## 5  2003 S2      <VCorpus>    <VCorpus>   <VCorpus>   
    ## 6  2005 R       <VCorpus>    <VCorpus>   <VCorpus>

This dataset provides various VCorpus objects based on the act titles
and text in the `legislation` dataset. There is also a stemmed version
that is useful for conducting topical analyses.

## Topic modeling

A large amount of topic modeling has been conducted on the dataset; this
is a time-consuming process, so various functions are provided to load
topic models and other models derived from them. Those functions are:

  - `load_topic_models`: loads the LDA Gibbs objects for the specified
    range of topics (currently 100-203)
  - `load_lawmaker_models`: loads the dot product of the sparse lawmaker
    sponsorship matrix and the LDA Gibbs gamma matrix for the specified
    range of topics; this represents a “career view” topic model for
    each lawmaker
  - `load_lawmaker_session_models`: loads the ot product of the sparse,
    by-session lawmaker sponsorship matrix and the LDA Gibbs gamma
    matrix for the specified range of topics; this represents a “session
    view” topic model for each lawmaker’s activity over time

Note: As noted above, only a few topic models are provided in the
package build in order to keep the size down. Additional topic models in
the data/\*\_model subdirectories can be downloaded if needed. In
particular, the topic models are large – on the order of 1.6GB. Use care
when downloading, loading, and processing.

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
