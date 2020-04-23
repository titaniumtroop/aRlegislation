packages <- c("aRlegislation", "tidyr", "dplyr", "purrr", "furrr", "tm", "parallel", "ldatuning")
# packages <-  c(packages, "snow", "Rmpi") #HPC
lapply(packages, require, character.only = TRUE)
rm(packages)

options( scipen = 10 ) # print full numbers, not scientific notation
options(future.globals.maxSize = 2*1024*1024^2) # 2GB max, 500MB caused errors

## ----create full corpus from session corpora-----------------------------

full_corpus <- do.call(function(...) c(..., recursive = TRUE), legislation_corpus$text_stemmed)
# or
# Test data
# data("crude")
# full_corpus <-crude
# rm(crude)

stemmed.DTM <- DocumentTermMatrix(full_corpus)

# --- Set topic range ----------------------------------------------------------
# Strongly advise DO NOT RUN on full topic range of legislation
# This takes a long time, even when run in parallel or on HPC
# Advise to break up into individual chunks and combine later

topic_range <- 50:325 # Full topic range
# calculated_topic_range <- 117:201
# topic_range <- 5:8 # For test

# ---- Create parallel cluster ---------------------
## Single System
my.cores <- parallel::detectCores() - 1
parallelCluster <- parallel::makeCluster(parallel::detectCores())

## HPC
# my.cores <- as.numeric(Sys.getenv("PBS_NP")) - 1
# snow::setDefaultClusterOptions(outfile = "./output/slave.out")
# parallelCluster <- snow::makeMPIcluster(my.cores, verbose = TRUE)

time.full.topics <- system.time({
  topic_models <- ldatuning::FindTopicsNumber(
    stemmed.DTM,
    topics = topic_range,
    metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
    method = "Gibbs",
    mc.cores = parallelCluster,
    control = list(seed = 77),
    verbose = TRUE,
    return_models = TRUE
  )
})

time.full.topics

# save(
#   topic_models,
#   file = paste("../data-raw/topic_models ",
#                min(topic_range), "-", max(topic_range), " ",
#                gsub(":", "", Sys.time()),
#                ".RData",
#                sep = ""),
#   envir = .GlobalEnv,
#   compress = "bzip2"
# )

# Run after combining multiple outputs
# Result is ~1.5GB, takes forever to load
# usethis::use_data(topic_models, compress = "bzip2", overwrite = TRUE)

# Github requires files < 100 MB
# So, will save these individually
# Will need a combiner function to fetch and combine

for (i in 1:NROW(topic_models)) {
# for (i in 1:1) {
  j <- topic_models[i,]$topics
  assign(paste("topic_model", j, sep = "_"), topic_models[i,])
  saveRDS(
    get(paste("topic_model", j, sep = "_")),
    file = paste0("./data/topic_models/topic_model_", j, ".RData"),
    compress = "bzip2"
  )
}

