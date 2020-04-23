## ----setup, include=FALSE-----------------------------------------------------

packages <- c("aRlegislation", "tidyverse", "furrr", "parallel", "scales")
lapply(packages, require, character.only = TRUE)
rm(packages)

options(future.globals.maxSize = 3*1024*1024^2) # 3GB max, 500MB caused errors


## ----load topic models ---------------------------------

# Load topic distributions from storage
topic_models <- load_topic_models(25:325)

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



## ---- lawmaker long model creation--------------------------------------------

act_list <- legislation %>%
  unnest(acts) %>%
  select(cycle, session, act)

sparse_lawmakers <- legislation %>%
  select(-c(acts, lawmakers)) %>%
  unnest(sponsorship) %>%
  left_join(
    lawmakers %>%
      select(-count) %>%
      unnest(sessions) %>%
      select(sponsor_full_name, sponsor, cycle, session, corruption),
    by = c("sponsor", "cycle", "session")
  ) %>%
  select(-sponsor) %>%
  unite(act_full_context, cycle, session, act, remove = FALSE) %>%
  mutate(bool_value = 1) %>%
  nest(sparse_lawmakers = -c(cycle, session)) %>%
  mutate(sparse_lawmakers = map(sparse_lawmakers, pivot_wider, id_cols = c(sponsor_full_name, corruption), names_from = act_full_context, values_from = bool_value, values_fill = list(bool_value = 0), values_fn = list(bool_value = length))) %>%
  mutate(sparse_lawmakers = map(sparse_lawmakers, drop_na, sponsor_full_name))

# sparse_list <- sparse_lawmakers$sparse_lawmakers[[1]]
# lda_gibbs <- topic_models_gamma$LDA_model[[1]]

dot_product <- function(sparse_list, lda_model) {

  temp <- data.matrix(sparse_list[,! names(sparse_list) %in% c("sponsor_full_name", "corruption")]) %*% data.matrix(lda_model[,! names(lda_model) %in% c("act")])
  temp <- temp %>% as_tibble()
  temp$corruption <- sparse_list$corruption
  temp$sponsor_full_name <- sparse_list$sponsor_full_name
  temp
}

compute_dot_product <- function(lda_gibbs, act_list, sparse_lawmakers) {

  if (class(lda_gibbs) =="LDA_Gibbs") {
    temp_LDA <- lda_gibbs@gamma %>% as_tibble(.name_repair = function(x) {paste0("V", seq_along(x))})
  } else {
    temp_LDA <- lda_gibbs %>% as_tibble(.name_repair = function(x) {paste0("V", seq_along(x))})
  }

  # Need to rescale entire gamma matrix after taking all dot products, not here -- values get wonky on dot product
  # Won't rescale properly if we do so on cycle-by-cycle basis

  models_acts <- act_list %>%
    bind_cols(., temp_LDA) %>%
    select(-act) %>%
    nest(model = -c(cycle, session))

  models_acts <- models_acts %>%
    left_join(sparse_lawmakers, by = c("cycle", "session"))

  sparse_list <- models_acts$sparse_lawmakers[[1]]
  lda_model <- models_acts$model[[1]]


  models_acts <- models_acts %>%
    mutate(lawmaker_models = map2(sparse_lawmakers, model, dot_product))

  lawmaker_model <- models_acts %>%
    select(-c(model, sparse_lawmakers)) %>%
    unnest(lawmaker_models) %>%
    select(cycle, session, sponsor_full_name, corruption, everything()) %>%
    mutate(cycle = as.integer(cycle)) %>%
    mutate(corruption = ifelse(corruption, "corrupt", "not corrupt")) %>%
    mutate(corruption = factor(corruption, levels = c("corrupt", "not corrupt"), ordered = TRUE))

  # Scale by column
  lawmaker_model <- lawmaker_model %>%
    mutate_if(grepl("^V\\d+", names(lawmaker_model)), scales::rescale, to = c(-1, 1)) %>%
    as_tibble()

  lawmaker_model

}

topic_models_gamma <- topic_models %>%
  mutate(LDA_model = map(LDA_model, function(x) { x@gamma }))

lawmaker_session_models <- topic_models_gamma %>%
  select(topics, LDA_model) %>%
  mutate(lawmaker_model = future_map(LDA_model, compute_dot_product, act_list = act_list, sparse_lawmakers = sparse_lawmakers, .progress = TRUE)) %>%
  select(-LDA_model) %>%
  as_tibble()


## ----Save---------------------------------------------------------------------

# save(
#   lawmaker_models,
#   file = paste("../data-raw/lawmaker_models_long ",
#                gsub(":", "", Sys.time()),
#                ".RData",
#                sep = ""),
#   envir = .GlobalEnv,
#   compress = "bzip2"
# )

# usethis::use_data(lawmaker_session_models, compress = "bzip2", overwrite = TRUE)

for (i in 1:NROW(lawmaker_session_models)) {
  # for (i in 1:1) {
  j <- lawmaker_session_models[i,]$topics
  assign(paste("lawmaker_session_model", j, sep = "_"), lawmaker_session_models[i,])
  saveRDS(
    get(paste("lawmaker_session_model", j, sep = "_")),
    file = paste0("./data/lawmaker_session_models/lawmaker_session_model_", j, ".RData"),
    compress = "bzip2"
  )
}

