# packages <- c("lubridate", "tidyverse", "ggplot2", "ggridges", "plotly", "tidyr", "dplyr", "scales", "pdftools", "tm", "wordcloud", "wordcloud2", "tidytext", "topicmodels", "doParallel", "furrr", "ldatuning", "rvest", "class", "NbClust", "knitr", "rticles", "stringr", "bookdown")
#lapply(packages, install.packages)

packages <- c("aRlegislation", "tidyr", "dplyr", "furrr", "tm")
lapply(packages, require, character.only = TRUE)
rm(packages)

options( scipen = 10 ) # print full numbers, not scientific notation

plan(multiprocess) # furrr multicore setup


#'
#' ## Corpus Creation {#corpuscreation}
#'
#' We've already done a fair amount of cleaning of the act, title, and subtitle text. That step was to help make the PDF text more human and machine-friendly to read. Next, we'll be further cleaning the text to create a corpus of tokens for processing. "Tokens" refers to unique terms in the text and is a rough proxy for words; "corpus" simply means a collection of tokens.
#'
#' Earlier we identified that there are still newlines, numbers, and punctuation in the munged text. However, we're going to do some additional cleaning of the text before analyzing it that includes the following:
#'
#' * removing whitespace, punctuation, and numbers
#' * making all text lowercase
#' * removing common "stop words" such as a, an, the, and and
#'
#' In order to make the title/text comparison, we'll need to separate act titles and subtitles from the remainder of the act text. We'll combine titles and subtitles for purposes of the analysis.
#'
#' To perform these tasks, we'll be using the tm package [@JSSv025i05], which provides a robust set of tools for creating and manipulating text corpora and document-term matrices.
#'
## ----create corpus functions---------------------------------------------

#data <- legislation$acts[[1]] %>% head(10)

create_corpus <- function(
  data = NULL,
  stopwords = NULL,
  text_columns = NULL
) {

  if (is.null(data)) stop("No data supplied")
  if (any(is.na(text_columns %in% names(data)))) stop("Column names do not exist in data")
  #if (any(! is.character(data[, names(data) %in% text_columns]))) stop("Specified column(s) do not contain text data")

  text.raw <- tidyr::unite(data[, names(data) %in% text_columns], text, sep = " ")

  corpus.raw <- VCorpus(VectorSource(text.raw$text))

  # We aren't stemming here, but will stem for LDA analysis
  # Sentiment analysis looks for word matches, which could be problematic with stemmed versions that aren't real words
  corpus.raw <- tm_map(corpus.raw, removeNumbers)
  corpus.raw <- tm_map(corpus.raw, removePunctuation,
         preserve_intra_word_contractions = TRUE,
         preserve_intra_word_dashes = TRUE,
         ucp = TRUE)
  corpus.raw <- tm_map(corpus.raw, content_transformer(tolower))
  corpus.raw <- tm_map(corpus.raw, removeWords, stopwords("english"))
  corpus.raw <- tm_map(corpus.raw, stripWhitespace)

  if (! is.null(stopwords)) {
    corpus.raw <- tm_map(corpus.raw, removeWords, stopwords.domain)
  }

  corpus.raw

}

create_frequency_matrix <- function(
  corpus,
  min_word_length = 0,
  max_word_length = Inf
) {

  if (all(class(corpus) != "VCorpus")) stop("Corpus is not of class VCorpus")

  DTM <- DocumentTermMatrix(
    corpus,
    control = list(
      wordLengths = c(min_word_length, max_word_length)#,
      #weighting = weightTfIdf
    )
  )
  m <- as.matrix(DTM)

  word_freqs = sort(colSums(m), decreasing = TRUE)
  mdf <- data.frame(word = names(word_freqs), freq = word_freqs)
  # mdf <- mdf[!mdf$freq == 0,] # Need to supply full list so it matches act length?
  # mdf <- mdf[!mdf$word == "",] # Same
  rownames(mdf) <- NULL

  mdf
}


## ----create title corpus in parallel-------------------------------------

time.corpus.title <- system.time({

  legislation <- legislation %>%
  mutate(
    title_corpus = future_map(acts, create_corpus, text_columns = c("title", "subtitle"))
  )
})

time.corpus.title

#'
#' However, since act text is much longer than the text of titles/subtitles, we do expect a significant time savings by processing in parallel:
#'
## ----create text corpus in parallel--------------------------------------

time.corpus.text <- system.time({

  legislation <- legislation %>%
  mutate(
    text_corpus = future_map(acts, create_corpus, text_columns = c("text"))
  )
})

time.corpus.text


# Print before of words that start with govern*
temp.mdf <- create_frequency_matrix(legislation$text_corpus[[27]])
temp.mdf %>%
  filter(grepl("govern.*", word)) %>%
  as_tibble()

## ----stemming------------------------------------------------------------

stem_corpus <- function(corpus) {
  # tm_map(corpus,
  #        removePunctuation,
  #        preserve_intra_word_contractions = TRUE,
  #        preserve_intra_word_dashes = TRUE,
  #        ucp = TRUE)
  #  tm_map(corpus, scan_tokenizer)
  tm_map(corpus, stemDocument)
}

# time.stem.title <- system.time({
#   legislation <- legislation %>%
#     mutate(title_stemmed = future_map(title_corpus, stem_corpus))
# })
#time.stem.title

time.stem.text <- system.time({
  legislation <- legislation_corpus %>%
    mutate(text_stemmed = future_map(text_corpus, stem_corpus))
})

temp.mdf <- create_frequency_matrix(legislation$text_stemmed[[27]])

temp.mdf %>%
  filter(grepl("govern.*", word)) %>%
  as_tibble()


#'
#' Stemming is also performed in parallel and takes a few moments:
#'
## ------------------------------------------------------------------------
time.stem.text

legislation_corpus <- legislation %>%
  select(cycle, session, title_corpus, text_corpus, text_stemmed)

## ---- eval = T-----------------------------------------------------------

#rm(time.corpus.text, time.corpus.title, legislation)
# save(
#   list = ls(all.names = T)[!c("params") %in% ls(all.names = T)],
#   file = paste("./data-raw/3a_corpus_creation ",
#                gsub(":", "", Sys.time()),
#                ".RData",
#                sep = ""),
#   envir = .GlobalEnv
# )

usethis::use_data(legislation_corpus, compress = "bzip2", overwrite = TRUE)
