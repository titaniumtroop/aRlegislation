
#packages <- c("lubridate", "tidyverse", "ggplot2", "plotly", "tidyr", "dplyr", "scales", "pdftools", "tm", "wordcloud", "wordcloud2", "tidytext", "topicmodels", "doParallel", "ldatuning", "rvest", "class", "NbClust", "knitr", "rticles", "stringr", "bookdown")

packages <- c("dplyr", "lubridate", "doParallel", "pdftools")

#lapply(packages[! packages %in% installed.packages()], install.packages)
lapply(packages, require, character.only = TRUE)
rm(packages)

data.dir <- "./data-raw/"

# Url example: http://www.arkleg.state.ar.us/assembly/2017/2017R/Acts/Act545.pdf
#
# According to search page at http://www.arkleg.state.ar.us/SearchCenter/Pages/historicalact.aspx
# Regular and extraordinary sessions through 2009, file path of YEAR/SESSION_TYPE/Act#.pdf, e.g., 2001/R/Act1.pdf
# No odd-number years in file path through 2009
# In 2010 with fiscal sessions every even year, the calendar year gets prepended to the SESSION_TYPE, e.g., 2011/2012F/Act1.pdf

# Uncomment first line to download all
# After past acts already downloaded, not necessary to include previous cycles/sessions
sessions <- expand.grid(session_type = c("R","S1","S2","F","S3","S4"), session = c(2001:year(Sys.Date())), cycle = rep(seq(from = 2001, to = year(Sys.Date()), by = 2), 2), stringsAsFactors = F) %>%
# sessions <- expand.grid(session_type = c("R","S1","S2","F","S3","S4"), session = c(2019:year(Sys.Date())), cycle = rep(seq(from = 2019, to = year(Sys.Date()), by = 2), 2), stringsAsFactors = F) %>%
  filter(session - cycle <= 1, session - cycle >= 0) %>%
  filter(ifelse(session < 2009, session %% 2 != 0, TRUE)) # Eliminate even years below 2010 from search grid, as cycles only have odd numbers

sessions <- data.frame(
  cycle = sessions$cycle,
  session = paste0(sessions$session, sessions$session_type),
  session_type = sessions$session_type,
  year = sessions$session,
  stringsAsFactors = F
)

# Sessions before 2010 don't have year prepended to the session type
sessions$dir_ref <- ifelse(sessions$year <= 2009, sessions$session_type, sessions$session)

session.dirs <- sessions %>%
  select(year, dir_ref) %>%
  distinct()

session.dirs <- session.dirs %>%
  left_join(sessions) %>%
  distinct()

# Loop through all sessions
# Download acts until we get three consecutive errors
for (i in 1:dim(session.dirs)[1]) {
  #print(sessions[i, ])
  act.no <- 1
  still.downloading <- TRUE
  consecutive.misses <- 0 # Some files aren't available, so we'll check a couple of additional numbers to make sure a missing file doesn't stop the downloads

  dir.err <- try(dir.create(paste0(data.dir, "/", session.dirs$cycle[i])))

  while (! is.na(still.downloading & "try-error" != class(dir.err))) {

    if (session.dirs$cycle[i] < 1999) {
      filename <- paste0(act.no, ".pdf")
    } else {
      filename <- paste0("Act", act.no, ".pdf")
    }
    local.filename  <- paste0("Act", act.no, ".pdf")

    destfile <- paste0(data.dir, "/", session.dirs$cycle[i], "/", session.dirs$dir_ref[i], "-", local.filename)

    url <- paste("http://www.arkleg.state.ar.us/assembly/", session.dirs$cycle[i], "/", session.dirs$dir_ref[i], "/Acts/", filename, sep="")

    if (! file.exists(destfile)) { # Don't download existing files


      err <- try(html.result <- download.file(url, destfile, mode = "wb", quiet = TRUE))
      Sys.sleep(sample(seq(0.2,1,0.1),1)) # sleep to avoid IP blocking
      if (class(err) == "try-error" & consecutive.misses >= 2) {
        still.downloading <- NA
      } else if (class(err) == "try-error") {
        consecutive.misses <- consecutive.misses + 1
        act.no <- act.no + 1
      } else {
        act.no <- act.no + 1
        consecutive.misses <- 0
      }
    } else {
      act.no <- act.no + 1
      consecutive.misses <- 0
    }

  }
}
rm(act.no, filename, destfile, url, html.result, i, still.downloading, err, consecutive.misses, sessions, session.dirs, local.filename)



## ----create data frame with files----------------------------------------
acts.df <- data.frame()
act.files <- list.files(path = data.dir, recursive = TRUE, pattern = "*.pdf", full.names = FALSE)

act.files <- act.files[! grepl("^1995|^1997|^1999", act.files)] # 1997 and earlier don't follow same format

# Run file read with all processors
registerDoParallel()
#getDoParWorkers()

time.file.read <- system.time({
  acts.df <- foreach (act.file = act.files, .combine = rbind) %dopar% {
    cycle <- as.numeric(sub("(\\d+)\\/.+$", "\\1", act.file))
    act <- as.numeric(sub(".+-Act(\\d+).pdf", "\\1", act.file))
    session <- sub(".+?(\\w+)-Act\\d+.pdf", "\\1", act.file, perl = TRUE)

    # Some PDFs corrupt
    read.pdf.attempt <- try({
      raw.txt <- pdf_text(paste(data.dir, act.file, sep = ""))
    })
    if (class(read.pdf.attempt) != 'try-error') {
      raw.txt <- paste(raw.txt, collapse = "")
      data.frame(cycle, session, act, raw.txt, stringsAsFactors = FALSE)
    } else {
      message(paste0("Error reading Act ", act, " from the ", cycle, " cycle, ", session, " session."))
    }
  }
})

time.file.read

#time.file.read, single session
# 4 Cores, Mac Pro mid-2010
#    user  system elapsed
# 166.193   1.491  77.215
# 2 cores, Macbook Pro 2015 i7
 #   user  system elapsed
 # 85.606   1.218 105.194
#    user  system elapsed
# 156.567   1.329  89.083

#time.file.read, 2001-2019 sessions (partial 2019)
# 2 cores, Macbook Pro 2015 i7
#    user  system elapsed
# 290.311  15.244 172.945
#    user  system elapsed
# 299.626  12.842 172.574

acts.df <- acts.df %>%
  mutate(act = as.numeric(act)) %>%
  arrange(cycle, session, act) %>%
  as_tibble()



## ------------------------------------------------------------------------

rm(act.files, time.file.read, act, act.file, cycle, dir.err, raw.txt, read.pdf.attempt, session)

# save.image(
save(
  acts.df,
  file = paste(data.dir, "/1a_download_acts ",
               gsub(":", "", Sys.time()),
               ".RData",
               sep = ""),
  envir = .GlobalEnv,
  compress = "bzip2"
)
