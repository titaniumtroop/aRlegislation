packages <- c("dplyr", "tidyr", "furrr")
#lapply(packages, install.packages)
lapply(packages, require, character.only = TRUE)
rm(packages)

# Load topic distributions from storage
dir.files <- list.files("./data-raw", "Rdata", full.names = TRUE, ignore.case = TRUE)
dir.file.info <- file.info(dir.files)
dir.file.info$name <- dir.files

newest.file <- dir.file.info %>%
  arrange(desc(mtime)) %>%
  filter(size > 0 & grepl("1a_download_acts", name)) %>%
  top_n(1, mtime) %>%
  select(name)

load(newest.file$name)
rm(newest.file, dir.files, dir.file.info)



## ----cleaning act text regexes-------------------------------------------

clean_act_text <- function(acts.df) {

  acts.df$munged.txt <- gsub("Stricken language.+?[SENATE|HOUSE] BILL|Act [[:digit:]]+ of the Regular Session.+?[SENATE|HOUSE] BILL", "", acts.df$raw.txt) # Drop header
  #acts.df$munged.txt <- gsub("-|/", " ", acts.df$munged.txt)
  acts.df$munged.txt <- gsub("/", " ", acts.df$munged.txt)

  # Remove footer, junk at end of text
  acts.df$munged.txt <- gsub("\\d{1,2}.?\\d{1,2}.?\\d{4} \\d{1,2}:\\d{1,2}:\\d{1,2} \\w{2} \\w{3}\\d{3}", " ", acts.df$munged.txt, ignore.case = TRUE) # remove footer tag
  acts.df$munged.txt <- gsub("\\d{1,2}.?\\d{1,2}.?\\d{4} \\d{1,2}:\\d{1,2}:\\d{1,2} \\w{3}\\d{3}\\s*", " ", acts.df$munged.txt, ignore.case = TRUE) # remove footer tag
  acts.df$munged.txt <- gsub("\\s*APPROVED: [[:digit:]]{1,2}.?[[:digit:]]{1,2}.?[[:digit:]]{2,4}.+?$", " ", acts.df$munged.txt) # Remove approval stamp at end of text
  acts.df$munged.txt <- gsub("\\w{3}\\d{3}\\s*$", " ", acts.df$munged.txt, perl = TRUE, ignore.case = TRUE) # remove initials and number at end of footer tag not removed in previous step
  acts.df$munged.txt <- gsub("\\s*APPROVED:[\\s\\d\\.]+$", " ", acts.df$munged.txt, perl = TRUE, ignore.case = TRUE) # remove dates and other digits in footer tag not removed in previous step
  acts.df$munged.txt <- gsub("\\*[[:alpha:]]{3}[[:digit:]]{3}\\*", " ", acts.df$munged.txt, perl = TRUE, ignore.case = TRUE) # remove 1st page footer tag
  #head(substr(acts.df$munged.txt, nchar(acts.df$munged.txt) - 250, nchar(acts.df$munged.txt))) # check to make sure footer removed

  # Remove Act number in header
  # substring(acts.df[31, ]$munged.txt, 600, 2000)
  # substring(acts.df[7, ]$munged.txt, 600, 2000)
  acts.df$munged.txt <- gsub("(SB[[:digit:]]+\\b)", " ", acts.df$munged.txt, perl = TRUE, ignore.case = TRUE)
  acts.df$munged.txt <- gsub("(HB[[:digit:]]+\\b)", " ", acts.df$munged.txt, perl = TRUE, ignore.case = TRUE)

  # Remove line or page numbers
  acts.df$munged.txt <- gsub("\n\\s*\\d+ *", "\n", acts.df$munged.txt, perl = TRUE) # Remove line or page numbers
  #acts.df$munged.txt <- gsub("\\s+", " ", acts.df$munged.txt, perl = TRUE) # Remove extra spaces
  acts.df$munged.txt <- gsub("\n\\s*\\d{1,2}\\s*\n", "\n", acts.df$munged.txt, perl = TRUE) # Remove blank lines (line or page numbers)
  acts.df$munged.txt <- gsub("\n\\s*\\d{1,2}\\s+(.+?)\n", "\\1\n", acts.df$munged.txt, perl = TRUE) # Remove line numbers
  #acts.df$munged.txt <- gsub("^\n\\s*\\d{1,2}\\s+(.+?)\n", "\\1\n", acts.df$munged.txt, perl = TRUE)
  acts.df$munged.txt <- gsub("^\\s+|\\s+$", "", acts.df$munged.txt)


  # Title
  acts.df$bill.no <- sub("^.+?[SENATE|HOUSE]{5,6}\\s*BILL\\s*(\\d+).+?$", "\\1", acts.df$raw.txt)
  acts.df$title <- sub("^.+?For An Act To Be Entitled(.+?)Subtitle.+", "\\1", acts.df$munged.txt)
  acts.df$subtitle <- sub("^.+?Subtitle(.+)BE IT ENACTED BY THE GENERAL ASSEMBLY OF THE STATE OF ARKANSAS:.+", "\\1", acts.df$munged.txt)


  # For future versions, should leave in
  # Then process hyphenated words with tm_map, removePunctuation options
  # Separate hyphenated words before removing punctuation
  # acts.df$title <- gsub("-|/", " ", acts.df$title)
  # acts.df$subtitle <- gsub("-|/", " ", acts.df$subtitle)

  # Drop newlines in titles/subtitles
  acts.df$title <- gsub("\n", " ", acts.df$title, perl = TRUE)
  acts.df$subtitle <- gsub("\n", " ", acts.df$subtitle, perl = TRUE)

  # Drop extra spaces in titles/subtitles
  acts.df$title <- gsub("^\\s+", " ", acts.df$title)
  acts.df$subtitle <- gsub("\\s+", " ", acts.df$subtitle)

  # Drop leading/trailing spaces
  acts.df$title <- gsub("^\\s+|\\s+$", "", acts.df$title)
  acts.df$subtitle <- gsub("^\\s+|\\s+$", "", acts.df$subtitle)


  #write.csv(acts.df[15, c("act", "title", "subtitle") ], "~/Desktop/titles.tsv", sep="\t")

  # Drop remaining header, now that we've extracted sponsors and title information
  acts.df$munged.txt <- sub("^.+?BE IT ENACTED BY THE GENERAL ASSEMBLY OF THE STATE OF ARKANSAS:(.+)", "\\1", acts.df$munged.txt, ignore.case = TRUE)
  # grep("stricken", substring(acts.df$munged.txt, 1, 100), value = TRUE, ignore.case = TRUE) # Check to make sure instructions are removed

  #writeLines(acts.df[31, ]$munged.txt, "~/Desktop/out.txt")
  #writeLines(acts.df[7, ]$raw.txt, "~/Desktop/out.raw.txt")

  acts.df
}



## ----act text regexes parallel processing--------------------------------

plan(multiprocess) # furrr multicore setup

acts.nested <- acts.df %>%
  as_tibble() %>%
  group_by(cycle, session) %>%
  arrange(cycle, session) %>%
  nest(.key = "acts")

time.clean_act_text <- system.time({
  acts.nested <- acts.nested %>%
    mutate(
      acts.df = future_map(acts, clean_act_text)
    )
})

acts.df <- acts.nested %>%
  unnest(acts.df)

time.clean_act_text
rm(time.clean_act_text, acts.nested, packages, clean_act_text)

save(
  acts.df,
  file = paste("./data-raw/1b_clean_up_your_acts ",
               gsub(":", "", Sys.time()),
               ".RData",
               sep = ""),
  envir = .GlobalEnv,
  compress = "bzip2"
)
