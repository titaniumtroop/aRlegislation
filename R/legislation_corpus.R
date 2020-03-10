#' Arkansas legislation from 2001 through the 2019 regular session.
#'
#' A dataset for Arkansas legislation. The format is a nested tibble; the
#' nesting variables are the beginning year of each two-year legislative cycle
#' and the type (and number, if applicable) of the session. There are three
#' VCorpus within each cycle/session for act titles, act text, and stemmed act
#' text.
#'
#'
#' @format A nested tibble with 27 rows and 5 variables:
#' \describe{
#'   \item{cycle}{year of the legislative cycle}
#'   \item{session}{session type and number}
#'   \item{title_corpus}{a VCorpus of act titles}
#'   \item{text_corpus}{a VCorpus of act text}
#'   \item{text_stemmed}{a VCorpus of stemmed act text}
#' }
#'
#' @source \url{https://github.com/titaniumtroop/aRlegislation/}
"legislation_corpus"
