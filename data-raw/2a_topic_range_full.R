# packages <- c("lubridate", "tidyverse", "ggplot2", "ggridges", "plotly", "tidyr", "dplyr", "scales", "pdftools", "tm", "wordcloud", "wordcloud2", "tidytext", "topicmodels", "doParallel", "purrr", "furrr", "ldatuning", "rvest", "class", "NbClust", "knitr", "rticles", "stringr", "bookdown")
#install.packages(packages[! packages %in% installed.packages()])

packages <- c("aRlegislation", "tidyr", "dplyr", "furrr", "tm")
lapply(packages, require, character.only = TRUE)
rm(packages)

options( scipen = 10 ) # print full numbers, not scientific notation

options(future.globals.maxSize = 2*1024*1024^2) # 2GB max, 500MB caused errors

## ----create full corpus from session corpora-----------------------------

full_corpus <- do.call(function(...) c(..., recursive = TRUE), legislation_corpus$text_stemmed)

stemmed.DTM.full <- DocumentTermMatrix(full_corpus)
# stemmed.DTM.full <- DocumentTermMatrix(full_corpus, control = list(weighting = weightTfIdf))
dim(stemmed.DTM.full)


## ---- Set topic range for processing; from previous reports, range will be 100-300
max <- 103
min <- 100
more.topics <- seq(from = min, to = max)

parallelCluster <- parallel::makeCluster(parallel::detectCores())
parallelCluster

time.full.topics <- system.time({
  full_topics <- ldatuning::FindTopicsNumber(
    stemmed.DTM,
    topics = more.topics,
    metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
    method = "Gibbs",
    mc.cores = parallelCluster,
    control = list(seed = 77),
    verbose = TRUE
  )
})

stopCluster(parallelCluster)

write_csv(
  full_topics,
  paste0(working.dir, "text_topics_full_corpus.csv"),
  append = TRUE
)

save(
  list = ls(all.names = T)[!c("params") %in% ls(all.names = T)],
  file = paste("./RData/2a_topic_range_full ",
               gsub(":", "", Sys.time()),
               ".RData",
               sep = ""),
  envir = .GlobalEnv
)
