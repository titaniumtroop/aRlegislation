packages <- c("aRlegislation", "tidyverse")
lapply(packages, require, , character.only = TRUE)
rm(packages)

data("lawmakers")

# unlist the act numbers in the same way we combined all the documents in the
# corpus
# that way the ordering of of the 17215 columns/rows are the same
ordered_acts <- legislation %>%
  unnest(acts) %>%
  select(cycle, session, act) %>%
  unite(act_full_context, cycle, session, act)

# Get 1 entry for each lawmaker, resolved across all sessions
resolved_sponsorships <- lawmakers %>%
  as_tibble() %>%
  unnest(sessions) %>%
  select(sponsor_full_name, cycle, session, sponsor, corruption) %>%
  left_join(
    legislation %>%
      unnest_legacy(sponsorship),
    by = c("cycle", "session", "sponsor")
  )  %>%
  unite(act_full_context, cycle, session, act) %>%
  select(-sponsor) %>%
  right_join(ordered_acts, by = c("act_full_context")) %>%
  mutate(bool_value = 1) %>%
  pivot_wider(names_from = act_full_context, values_from = bool_value, values_fill = list(bool_value = 0), values_fn = list(bool_value = length)) %>%
  filter(!is.na(sponsor_full_name)) %>%
  arrange(sponsor_full_name)

#' In order to do the matrix algebra, the number of acts (ordered here) should
#' be equal to the number of columns in the resolved_sponsorship (less 2 --
#' one column for the sponsor name and one for the target corruption variable)
if (
  NROW(ordered_acts) == dim(resolved_sponsorships)[2] - 2
) {
  save(
    resolved_sponsorships,
    file = paste("./data-raw/resolved_sponsorships ",
                 gsub(":", "", Sys.time()),
                 ".RData",
                 sep = ""),
    envir = .GlobalEnv,
    compress = "bzip2"
  )
  # usethis::use_data(resolved_sponsorships, compress = "bzip2", overwrite = TRUE)
} else {
  stop("The dimensions between the number of acts and the resolved sponsorships don't match. Please check the input and try again.")
}
