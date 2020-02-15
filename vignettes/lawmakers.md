---
title: "Lawmakers"
output: 
  rmarkdown::html_vignette:
    keep_md: TRUE
vignette: >
  %\VignetteIndexEntry{Lawmakers}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---




```r
suppressPackageStartupMessages(library(aRlegislation))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(tidyr)) # needed for nest/unnest operations
suppressPackageStartupMessages(library(ggplot2))
```

For purposes of this dataset, the term "sponsor" refers to one or more lawmakers or committees that propose an enacted bill during a legislative session. This table contains demographic information about lawmakers, so it excludes committees -- hence the name. Here's what the structure of one of the lawmakers tibbles looks like: 


```r
head(legislation$lawmakers[[1]])
#> # A tibble: 6 x 11
#>   sponsor sponsor_full_na… chamber party committee seniority district
#>   <chr>   <chr>            <chr>   <chr> <chr>         <dbl>    <dbl>
#> 1 Adams   Bob Adams        House   D     TASK FOR…        73       48
#> 2 Agee    Sarah Agee       House   R     ARKANSAS…        46        9
#> 3 Allison Jerry Allison    House   D     PUBLIC T…         9       86
#> 4 Altes   Denny Altes      House   R     PUBLIC H…        66       14
#> 5 Argue   Jim Argue        Senate  D     ALC/JBC …        13       15
#> 6 B. Joh… Bob Johnson      Senate  D     AGRICULT…        34       25
#> # … with 4 more variables: occupation <chr>, church <chr>, veteran <chr>,
#> #   public_service <chr>
```

As an example, we can look at the political makeup of the lawmakers in each regular-session legislative chamber over time pretty easily:


```r
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

![](lawmakers_files/figure-html/cycle-chamber-party_plot-1.png)<!-- -->

This graph shows a very pronounced switch in the makeup of both chambers of the Arkansas legislature between 2009 and 2017. 
