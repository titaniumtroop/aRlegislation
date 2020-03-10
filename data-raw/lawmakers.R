packages <- c("tidyverse", "aRlegislation")
lapply(packages, require, character.only = TRUE)
rm(packages)


## Format of lawmaker table is unique(cycle, session, sponsor) + lawmaker_id
## lawmaker_id is non-unique for table such that unique(cycle, sponsor) provides an accurate count of legislative service
## Add lawmaker_id to other tables to disambiguate (or just use joins?)

## ----- Disambiguate sponsor_full_names that refer to different people -------

# Senator Bob Johnson from 2001-2009 is Robert W. Johnson from district 18
# Rep. Bob Johnson from 2015-2017 is J.P. Bob Johnson from district 42
# They do not appear to be the same person
# We'll change the latter one to how he's listed on Ballotpedia

lawmakers <- legislation %>%
  unnest_legacy(lawmakers)

lawmakers[
  grepl("Bob\\s*Johnson", lawmakers$sponsor_full_name) &
  lawmakers$cycle >= 2015,
  c("sponsor_full_name")
] <- c("J.P. Bob Johnson")



## ----- Combine records that refer to same person ----------------------------

## There are other lawmakers who have different versions of full names listed
## Those need to be merged
## Try a grid expand to identify using Levenstein edit distance

temp <- expand_grid(n1 = unique(lawmakers$sponsor_full_name), n2 = unique(lawmakers$sponsor_full_name))

temp <- temp %>%
  # mutate(n1 = trimws(gsub("\\s+", " ", n1))) %>% # Made this change in the dataset itself
  # mutate(n2 = trimws(gsub("\\s+", " ", n2))) %>% # Made this change in the dataset itself
  filter(n1 != n2) %>% # Delete same
  filter(n1 < n2) %>% # Remove reversed order
  mutate(dist = mapply(adist, n1, n2, ignore.case = T)) %>%
  mutate(diff = ifelse( # Use longer of two names as denominator
    nchar(n1) > nchar(n2),
    round(dist / nchar(n1), 3),
    round(dist / nchar(n2), 3)
    )
  ) %>%
  mutate(use_name = ifelse( # Use longer of two names
    nchar(n1) > nchar(n2),
    n1,
    n2
  )) %>%
  arrange(diff) %>%
  filter (diff < 0.35) # Based on visual inspection, 0.333 is the highest we have to deal with

# temp now contains possible targets for entity resolution
# Only a few below 0.231, so we'll start with the default break there
# and manually set values afterwards

temp <- temp %>%
  mutate(resolve = ifelse(
    diff <= 0.231,
    TRUE,
    FALSE
  ))

# Resolve the few above 0.231 that should be TRUE

temp$resolve[
  temp$n1 == "Hank Wilkins, IV"
] <- TRUE

temp$resolve[
  temp$n2 == "Jimmy Hickey, Jr"
] <- TRUE

# Resolve several below 0.231 that should be FALSE

# temp$resolve[
#   temp$n1 == "" &
#   temp$n2 == ""
# ] <- FALSE

temp$resolve[
  temp$n1 == "Jim Hendren" &
  temp$n2 == "Kim Hendren"
] <- FALSE

temp$resolve[
  temp$n1 == "Calvin Johnson" &
  temp$n2 == "David Johnson"
] <- FALSE

temp$resolve[
  temp$n1 == "David Rackley" &
  temp$n2 == "David Rainey"
] <- FALSE

temp$resolve[
  temp$n1 == "Blake Johnson" &
  temp$n2 == "Lee Johnson"
] <- FALSE

temp <- temp[temp$resolve == TRUE,]

for (i in 1:NROW(temp)) {
  lawmakers[
    lawmakers$sponsor_full_name == temp$n1[i] |
    lawmakers$sponsor_full_name == temp$n2[i],
    c("sponsor_full_name")
  ] <- temp$use_name[i]
}



## ------------------------------------------------------------------------

lawmakers <- lawmakers %>%
#  distinct(cycle, session, sponsor, sponsor_full_name) %>%
  group_by(sponsor_full_name) %>%
  mutate(count = n()) %>%
  ungroup() %>%
  nest(sessions = -c(sponsor_full_name, count)) %>%
  arrange(sponsor_full_name)

usethis::use_data(lawmakers, compress = "bzip2", overwrite = TRUE)

