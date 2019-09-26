packages <- c("dplyr", "usethis", "stringr", "tidyr")
#lapply(packages, install.packages)
lapply(packages, require, character.only = TRUE)
rm(packages)

# Load topic distributions from storage
dir.files <- list.files("./data-raw", "Rdata", full.names = TRUE, ignore.case = TRUE)
dir.file.info <- file.info(dir.files)
dir.file.info$name <- dir.files

newest.file <- dir.file.info %>%
  arrange(desc(mtime)) %>%
  filter(size > 0 & grepl("1e_munge_sponsor", name)) %>%
  top_n(1, mtime) %>%
  select(name)

load(newest.file$name)
rm(newest.file, dir.files, dir.file.info)

## ----shrink, clean up acts df--------------------------------------------
# At this point, we've pulled all the information we need from the raw text, so we'll remove it to save space
acts.df$raw.txt <- NULL
acts.df$text <- acts.df$munged.txt  # We'll rename munged text to text as well
acts.df$munged.txt <- NULL

# Also, the acts dataframe has the raw list of sponsors, and we've already parsed it, so we'll remove it as well
acts.df$sponsors <- NULL

# Change any non-tidy-compliant column names to tidy-compliant
names(acts.df) %<>%
  stringr::str_to_lower(.) %>%
  stringr::str_replace_all('\\.', '_')

names(sponsorship) %<>%
  stringr::str_to_lower(.) %>%
  stringr::str_replace_all('\\.', '_')

names(sponsors.detail) %<>%
  stringr::str_to_lower(.) %>%
  stringr::str_replace_all('\\.', '_')


acts.df <- sponsorship %>%
  mutate_if(is.factor, as.character) %>%
  left_join(sponsors.detail) %>%
  mutate(
    party = ifelse(
    grepl("Budget|Committee|Management|Senate|Efficiency|Insurance|Agencies|Taxation", sponsor),
    "committee",
    as.character(party))
  ) %>%
#  filter(!is.na(chamber)) %>%
#  mutate(party = factor(ifelse(party == "unk", "D", as.character(party)), ordered = T)) %>%
#  filter(party != "blank") %>%
  arrange(cycle, session, act) %>%
  count(cycle, session, party, act) %>% # counts all sponsors who signed onto acts
  arrange(cycle, session, act, desc(n)) %>%
  mutate(party = as.character(party)) %>%
  select(-n) %>%
  spread(., party, party, fill = "") %>%
  unite(parties, -cycle, -session, -act) %>%
  mutate(parties = gsub("_+", "_", parties)) %>%
  mutate(parties = gsub("^_+|_+$", "", parties)) %>%
  mutate(multiple = grepl("_", parties)) %>%
  mutate(partisan = ifelse(
    grepl("committee", parties),
    "committee",
    ifelse(
      multiple == TRUE,
      "bipartisan",
      parties
    )
  )) %>%
  select(-parties, -multiple) %>%
  arrange(cycle, session, act) %>%
  left_join(acts.df, by = c("cycle", "session", "act"))



## ----nest acts sponsors and sponsorship----------------------------------
legislation <- acts.df %>%
  as_tibble() %>%
  group_by(cycle, session) %>%
  arrange(cycle, session) %>%
  mutate_if(is.factor, as.character) %>%
  nest(.key = "acts") %>%
  left_join(
    sponsors.detail %>%
      as_tibble() %>%
      group_by(cycle, session) %>%
      arrange(cycle, session) %>%
      mutate_if(is.factor, as.character) %>%
      nest(.key = "lawmakers"),
    by = c("cycle" = "cycle", "session" = "session")
  ) %>%
  left_join(
    sponsorship %>%
      as_tibble() %>%
      group_by(cycle, session) %>%
      arrange(cycle, session) %>%
      mutate_if(is.factor, as.character) %>%
      nest(.key = "sponsorship"),
    by = c("cycle" = "cycle", "session" = "session")
  )

legislation

# # A tibble: 27 x 5
# cycle session acts                 sponsors            sponsorship
# <dbl> <chr>   <list>               <list>              <list>
#   1  2001 R       <tibble [1,843 × 6]> <tibble [136 × 11]> <tibble [4,814 × 2]>
#   2  2001 S1      <tibble [2 × 6]>     <tibble [86 × 11]>  <tibble [86 × 2]>
#   3  2003 R       <tibble [1,816 × 6]> <tibble [135 × 11]> <tibble [5,683 × 2]>
#   4  2003 S1      <tibble [63 × 6]>    <tibble [6 × 11]>   <tibble [66 × 2]>
#   5  2003 S2      <tibble [110 × 6]>   <tibble [124 × 11]> <tibble [473 × 2]>
#   6  2005 R       <tibble [2,325 × 6]> <tibble [135 × 11]> <tibble [6,128 × 2]>
#   7  2005 S1      <tibble [39 × 6]>    <tibble [133 × 11]> <tibble [802 × 2]>
#   8  2007 R       <tibble [1,755 × 6]> <tibble [136 × 11]> <tibble [7,110 × 2]>
#   9  2007 S1      <tibble [5 × 6]>     <tibble [70 × 11]>  <tibble [78 × 2]>
#   10  2009 2010F   <tibble [298 × 6]>   <tibble [118 × 11]> <tibble [495 × 2]>
#   # … with 17 more rows


## ----print sponsor table lengths to compare to nested tibbles------------

legislation %>%
  unnest(lawmakers) %>%
  group_by(cycle, session) %>%
  count() %>%
  ungroup() %>%
  arrange(cycle, session) %>%
  head()

# # A tibble: 6 x 3
#   cycle session     n
#   <dbl> <chr>   <int>
# 1  2001 R         136
# 2  2001 S1         55
# 3  2003 R         135
# 4  2003 S1          6
# 5  2003 S2        107
# 6  2005 R         134

legislation %>%
  unnest(sponsorship) %>%
  group_by(cycle, session) %>%
  count() %>%
  ungroup() %>%
  arrange(cycle, session) %>%
  head()

# # A tibble: 6 x 3
#   cycle session     n
#   <dbl> <chr>   <int>
# 1  2001 R        3218
# 2  2001 S1         56
# 3  2003 R        3398
# 4  2003 S1         66
# 5  2003 S2        277
# 6  2005 R        3798


rm(acts.df, sponsors.detail, sponsorship)

# save(
#   legislation,
#   file = "./data/legislation.RData",
#   compress = "bzip2"
# )

usethis::use_data(legislation, compress = "bzip2", overwrite = TRUE)
