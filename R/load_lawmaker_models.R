#' Loads pre-generated lawmaker models based on LDA model topic distributions
#'
#' Returns nested data frames containing the dot product of a sparse sponsorship
#' matrix and the Gibbs-type LDA models generated using the topicmodels::LDA
#' function. These models were generated on the entire legislative dataset from
#' 2001-2019. Because the generation is time-consuming, the individuals models
#' have been saved for ease of reloading. The models take up > 50 MB of space
#' in the aggregate, so they are saved individually. This function permits
#' topics within a specified range to be loaded.
#'#'
#' @param datadir A directory containing lawmaker models in nested dataframe format.
#' @param topics An integer or a vector of integers. Defaults to first 10.
#' @return A nested dataframe with a column named lawmaker_models for each of the topics specified.
#'
#' @export

load_lawmaker_models <- function(
  datadir = "./data/lawmaker_models/",
  topics = NULL
) {

  if (! dir.exists(datadir)) {
    stop("The directory you provided does not exist.")
  }

  # Get directory listing
  dir.files <- list.files(datadir, "Rdata", full.names = TRUE, ignore.case = TRUE)
  dir.files <- dir.files[grepl("lawmaker_model", dir.files)] # Ignore other files placed here
  dir.files <- data.frame(filename = dir.files, stringsAsFactors = F)
  dir.files$topics <- as.numeric(sub(".*?_(\\d+)\\.RData", "\\1", dir.files$filename))

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

  lawmaker_models <- tibble::tibble()

  for (i in 1:NROW(dir.files.filtered)) {
    temp <- readRDS(dir.files.filtered$filename[[i]])
    lawmaker_models <- dplyr::bind_rows(lawmaker_models, temp)
    rm(temp)
  }

  lawmaker_models
}
