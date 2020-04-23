## ----setup, include=FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------packages <- c("aRlegislation", "tidyverse", "furrr", "parallel", "scales")
lapply(packages, require, character.only = TRUE)
rm(packages)

options( scipen = 10 ) # print full numbers, not scientific notation
options(future.globals.maxSize = 3*1024*1024^2) # 3GB max, 500MB caused errors


## ----load topic models ---------------------------------

# Load topic distributions from storage
topic_models <- load_topic_models(datadir = "./inst/extdata/", topics = 75:205)


## ---- Resolve sponsorships _____________________________
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


# ---- Create parallel cluster ---------------------
## Single System
# my.cores <- parallel::detectCores() - 1
# parallelCluster <- parallel::makeCluster(parallel::detectCores())
# or
plan(multiprocess)

## HPC
# my.cores <- as.numeric(Sys.getenv("PBS_NP")) - 1
# snow::setDefaultClusterOptions(outfile = "./output/slave.out")
# parallelCluster <- snow::makeMPIcluster(my.cores, verbose = TRUE)



## ----Dimensionality check ----------------------------------------------------
# Check dimensions of resolved sponsorships (less 2 for sponsor name and target
# variable) vs. number of documents in the LDA model
dim(resolved_sponsorships)[[2]] - 2 == dim(topic_models$LDA_model[[1]]@gamma)[1]



## ----calculate dot products---------------------------------------------------

# We are rescaling data between 0 and 1 in order to allow for predictions based on a
# session-by-session basis
# Also converting the target variable to a factor here, and making it more descriptive
dot_product <- function(sp, LDA) {
  # Removing name column and target variable column
  temp_sp <- data.matrix(sp[! names(sp) %in% c("sponsor_full_name", "corruption")])

  if (class(LDA) =="LDA_Gibbs") {
    temp_LDA <- LDA@gamma
  } else {
    temp_LDA <- LDA
  }


  temp <- temp_sp %*% temp_LDA  %>%
    as_tibble(.name_repair = function(x) {paste0("V", seq_along(x))})

  # We will preserve scales in the event needed in the future, i.e., for rescaling of comparable input vectors
  # temp_scales <- data.frame(
  #   direction = c("from", "to"),
  #   min = c(min(temp), -1),
  #   max = c(max(temp), 1)
  # )

  # rescale over dataset to feature space of [-1, 1]
  temp <- temp %>%
    mutate_if(is.double, scales::rescale, to = c(-1, 1))

  # Bind sponsor names and corruption flag
  temp_tb <- cbind(sp[,c("sponsor_full_name", "corruption")], temp) %>%
    mutate(corruption = ifelse(corruption, "corrupt", "not corrupt")) %>%
    mutate(corruption = factor(corruption, levels = c("corrupt", "not corrupt"), ordered = TRUE)) %>%
    tibble::as_tibble()

  # attr(temp_tb, "scales") <- temp_scales
  temp_tb
}

topic_models_gamma <- topic_models %>%
  mutate(LDA_model = map(LDA_model, function(x) { x@gamma }))

lawmaker_models <- topic_models_gamma %>%
#  filter(topics >= 125 & topics <= 200) %>% # Internal area bounded by 4 LDA scoring algorithms
  as_tibble() %>%
  mutate(lawmaker_model = future_map(LDA_model, dot_product, sp = resolved_sponsorships, .progress = TRUE)) %>%
  select(topics, lawmaker_model)

#' Save as an attribute the resolved_sponsorship dataframe that was used to
#' generated the lawmaker_models
attr(lawmaker_models, "resolved_sponsorships") <- resolved_sponsorships

# save(
#   lawmaker_models,
#   file = paste("../data-raw/lawmaker_models_short ",
#                gsub(":", "", Sys.time()),
#                ".RData",
#                sep = ""),
#   envir = .GlobalEnv,
#   compress = "bzip2"
# )

for (i in 1:NROW(lawmaker_models)) {
  # for (i in 1:1) {
  j <- lawmaker_models[i,]$topics
  assign(paste("lawmaker_model", j, sep = "_"), lawmaker_models[i,])
  saveRDS(
    get(paste("lawmaker_model", j, sep = "_")),
    file = paste0("./inst/extdata/lawmaker_model_", j, ".rds"),
    compress = "bzip2"
  )
}

