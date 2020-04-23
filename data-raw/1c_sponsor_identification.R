packages <- c("dplyr")
#lapply(packages, install.packages)
lapply(packages, require, character.only = TRUE)
rm(packages)

# Load topic distributions from storage
dir.files <- list.files("./data-raw", "Rdata", full.names = TRUE, ignore.case = TRUE)
dir.file.info <- file.info(dir.files)
dir.file.info$name <- dir.files

newest.file <- dir.file.info %>%
  arrange(desc(mtime)) %>%
  filter(size > 0 & grepl("1b_clean_up_your_acts", name)) %>%
  top_n(1, mtime) %>%
  select(name)

load(newest.file$name)
rm(newest.file, dir.files, dir.file.info)



## ----separate sponsors---------------------------------------------------
# Bill sponsors
#acts.df$sponsors <- gsub("^.+?By:\\s*(.*)For An Act To Be Entitled.+", "\\1", acts.df$raw.txt) # Doesn't pick up multi-chamber
acts.df$sponsors <- sub("^\\s*.+?(By:\\s*.+)For An Act To Be Entitled.+", "\\1", acts.df$raw.txt)
acts.df$sponsors <- gsub("By: |\\d+", "", acts.df$sponsors)
acts.df$sponsors <- gsub("\\d+", " ", acts.df$sponsors, perl = TRUE)
acts.df$sponsors <- gsub("\\s+", " ", acts.df$sponsors, perl = TRUE)
acts.df$sponsors <- trimws(acts.df$sponsors)

acts.df %>%
  arrange(desc(nchar(sponsors))) %>%
  top_n(1, nchar(sponsors)) %>%
  select(cycle, session, act, sponsors) %>%
  head(1)

## ----munge sponsors------------------------------------------------------
# sponsor pattern is comma- or title-separated
sponsor_processor <- function(sponsor.string) {
  # Or, replace word Senator(s)/Representative(s) with comma, trim leading/trailing comma, then split on comma?
  sponsor.string <- gsub(" and ", " ", sponsor.string)
  sponsor.string <- gsub("Senators|Senator", ",", sponsor.string)
  sponsor.string <- gsub("[`']", ",", sponsor.string)
  sponsor.string <- gsub("- ", "-", sponsor.string)
  sponsor.string <- gsub("Representatives|Representative", ",", sponsor.string)
  this.act.sponsors <- strsplit(sponsor.string, ",", perl = TRUE)
  this.act.sponsors <- lapply(this.act.sponsors, trimws)
  unlist(this.act.sponsors)
  # return data.frame(character vector) with each sponsor
}

# Now need a way to create separate dataframe matching each act to each sponsor on 1-to-1 basis for join later
sponsorship  <- tibble()

for (i in 1:NROW(acts.df)) {
  x <- acts.df[i, ]
  sponsor <- sponsor_processor(x$sponsors)
  cycle <- rep(x$cycle, length(sponsor))
  session <- rep(x$session, length(sponsor))
  act <- rep(x$act, length(sponsor))
  sponsorship <- rbind(sponsorship, data.frame(cycle, session, act, sponsor, stringsAsFactors = FALSE), stringsAsFactors = FALSE)
}

sponsorship <- sponsorship %>%
  filter(sponsor != "") %>%
  arrange(cycle, session, act) %>%
  as_tibble()

acts.df %>%
  arrange(desc(nchar(sponsors))) %>%
  top_n(1, nchar(sponsors)) %>%
  select(cycle, session, act) %>%
  head(1) %>%
  left_join(sponsorship)

rm(i, x, sponsor, cycle, session, act) # Remove temp and control variables
rm(sponsor_processor)

save(
  acts.df, sponsorship,
  file = paste("./data-raw/1c_sponsor_identification ",
               gsub(":", "", Sys.time()),
               ".RData",
               sep = ""),
  envir = .GlobalEnv,
  compress = "bzip2"
)
