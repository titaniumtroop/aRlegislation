packages <- c("aRlegislation", "tidyr", "dplyr", "purrr", "furrr", "tm", "parallel")
# packages <-  c(packages, "snow", "Rmpi") #HPC
lapply(packages, require, character.only = TRUE)
rm(packages)

options( scipen = 10 ) # print full numbers, not scientific notation
options(future.globals.maxSize = 2*1024*1024^2) # 2GB max, 500MB caused errors

## ----create full corpus from session corpora-----------------------------

#full_corpus <- do.call(function(...) c(..., recursive = TRUE), legislation_corpus$text_stemmed)

# Test data
data("crude")
full_corpus <-crude
rm(crude)

# calculated_topic_range <- 117:201
calculated_topic_range <- 5:8

LDA_models <- tibble::tibble(topics = calculated_topic_range)

# ---- Create parallel cluster ---------------------
## Single System
my.cores <- parallel::detectCores()
parallelCluster <- parallel::makeCluster(parallel::detectCores())

## HPC
# my.cores <- as.numeric(Sys.getenv("PBS_NP")) - 1
# snow::setDefaultClusterOptions(outfile = "./output/slave.out")
# parallelCluster <- snow::makeMPIcluster(my.cores, verbose = TRUE)


generate_LDA_model <- function(stemmed.corpus, ideal_topics) {

  stemmed.DTM <- DocumentTermMatrix(stemmed.corpus)

  control = list(seed = 77)

  message(paste0("Begin processing LDA for ", ideal_topics, " topics."))

  LDA(
    stemmed.DTM,
    k = ideal_topics,
    method = "Gibbs",
    control = control
  )
}

time.LDA.generation <- system.time({
  LDA_models <- LDA_models %>%
    mutate(LDA_model = future_map(topics, generate_LDA_model, stemmed.corpus = full_corpus))
})
paste("LDA processing time:", round(time.LDA.generation[3] / 3600, 2), "hours", sep = " ")

save(
  LDA_models,
  file = paste("./data-raw/topic_models ",
               gsub(":", "", Sys.time()),
               ".RData",
               sep = ""),
  envir = .GlobalEnv,
  compress = "bzip2"
)

