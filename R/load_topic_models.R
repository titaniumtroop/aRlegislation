#' Loads pre-generated LDA topic models of aRlegislation
#'
#' Returns Gibbs-type LDA models generated using the topicmodels::LDA function.
#' These topic models were generated on the entire legislative dataset from
#' 2001-2019. Because the generation is time-consuming, the individuals models
#' have been saved for ease of reloading. The models take up ~15MB of space
#' each, so they are saved individually. This function permits topics within a
#' specified range to be loaded.
#'
#'
#' @param datadir A directory containing topic models in nested dataframe format. If NULL, the inst/extdata and extdata directories are searched in the installation directory. The inst/extdata directory should be explicitly declared to use all files, as only a few models are provided in the installation.
#' @param topics An integer, a vector of integers, or named vector in c(to = x, from = y) format. Defaults to first 10.
#' @return A nested dataframe with a column named topic_models for each of the topics specified, along with scores calculated by the ldatuning package.
#'
#' @export

load_topic_models <- function(
  datadir = NULL,
  topics = NULL
) {

  if (! is.null(datadir)) {
    if (! dir.exists(datadir)) {
      stop("The directory you provided does not exist.")
    }
  }

  # Using 200 here because that's what's left in the directory from .Rbuildignore
  if (is.null(datadir)) {
    location <- try({
      system.file("extdata", "topic_model_200.rds", package="aRlegislation", mustWork = TRUE)
    })
    if (class(location) == "try-error") {
      stop("No models found.")
    }
    datadir <- sub("(.+/).*?$", "\\1", location)
  }

  # Get directory listing
  dir.files <- list.files(datadir, "rds", full.names = TRUE, ignore.case = TRUE)
  dir.files <- dir.files[grepl("topic_model", dir.files)] # Ignore other files placed here
  dir.files <- data.frame(filename = dir.files, stringsAsFactors = F)
  dir.files$topics <- as.numeric(sub(".*?_(\\d+).*", "\\1", dir.files$filename))

  if (class(topics) == "NULL") {
    dir.files.filtered <- utils::head(dir.files, 10)
  } else if (class(topics) == "numeric" | class(topics) == "integer") {
    if (! any(topics %in% dir.files$topics)) {
      stop(paste("None of the topic numbers you specified are available. Topics available include: ", paste(dir.files$topics, collapse = ", ")))
    }
    else if (! all(topics %in% dir.files$topics)) {
      message(paste("Some topics numbers are not available: ", paste(topics[! topics %in% dir.files$topics], collapse = ", ")))
    }
    dir.files.filtered <- dir.files[dir.files$topics %in% topics, ]
  } else {
    stop("Please enter a valid value for the topics you wish to retrieve.")
  }

  topic_models <- tibble::tibble()

  for (i in 1:NROW(dir.files.filtered)) {
    temp <- readRDS(dir.files.filtered$filename[[i]])
    topic_models <- dplyr::bind_rows(topic_models, temp)
    rm(temp)
  }

  topic_models
}
