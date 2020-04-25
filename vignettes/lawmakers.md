Lawmakers
================

This vignette examines the structure of the `lawmakers` tibbles
contained within the `legislation` dataset and displays the political
makeup of the Arkansas Legislature over time as an example use case.

``` r
library(aRlegislation)
library(dplyr)
library(tidyr) # needed for nest/unnest operations
library(ggplot2)
```

For purposes of this dataset, the term “sponsor” refers to one or more
lawmakers or committees that propose an enacted bill during a
legislative session. This table contains demographic information about
lawmakers, so it excludes committees – hence the name. Here’s what the
structure of one of the lawmakers tibbles looks like:

``` r
head(legislation$lawmakers[[1]])
#> # A tibble: 6 x 11
#>   sponsor sponsor_full_na… chamber party committee seniority district occupation
#>   <chr>   <chr>            <chr>   <chr> <chr>     <chr>        <dbl> <chr>     
#> 1 Adams   Bob Adams        House   D     TASK FOR… 73              48 Grant Cou…
#> 2 Agee    Sarah Agee       House   R     ARKANSAS… 46               9 Cattle Pr…
#> 3 Allison Jerry Allison    House   D     PUBLIC T… 9               86 Owner, Al…
#> 4 Altes   Denny Altes      House   R     PUBLIC H… 66              14 Recycler …
#> 5 Argue   Jim Argue        Senate  D     ALC/JBC … 13              15 Foundatio…
#> 6 B. Joh… Bob Johnson      Senate  D     AGRICULT… 34              25 <NA>      
#> # … with 3 more variables: church <chr>, veteran <chr>, public_service <chr>
```

As an example, we can look at the political makeup of the lawmakers in
each regular-session legislative chamber over time pretty easily:

``` r
# Use party-related colors
party.colors <- c(
  "R" = "#990000", # dark red = Republicans
  "D" = "#668cff", # light blue = Democrats
  "G" = "#00cc00", # bright green = Green party
  "I" = "#444444", # dark grey = Independent
  "unk" = "#b3b300", # dark yellow = unknown
  "bipartisan" = "#8A2BE2", # purple
  "committee" = "#888888" # medium grey
)

legislation %>%
  filter(grepl("R", session)) %>% # regular sessions only
  unnest(lawmakers) %>%
  count(cycle, session, party, chamber) %>%
  ggplot(aes(y = n, x = cycle, color = party)) +
    geom_point(size = 2.5) +
    geom_line(size = 3) +
    facet_wrap(~ chamber) +
    scale_x_continuous(
      breaks = seq(from = 2001, to = 2019, by = 4), 
      minor_breaks = seq(from = 2001, to = 2019, by = 2)
    ) +
    scale_color_manual(values = party.colors) +
    labs(
      x = "",
      y = "",
      title = "Political Makeup of the Arkansas Legislature"
    ) +
    theme(legend.position = "none")
```

![](/Users/nathan/Dropbox/Programming/aRlegislation/vignettes/lawmakers_files/figure-gfm/cycle-chamber-party_plot-1.png)<!-- -->

This graph shows a very pronounced switch in the makeup of both chambers
of the Arkansas legislature between 2009 and 2017.
