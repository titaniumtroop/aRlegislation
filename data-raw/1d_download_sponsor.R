packages <- c("dplyr", "doParallel", "rvest", "purrr", "httr")
#lapply(packages, install.packages)
lapply(packages, require, character.only = TRUE)
rm(packages)

# Load topic distributions from storage

dir.files <- list.files("./data-raw", "Rdata", full.names = TRUE, ignore.case = TRUE)
dir.file.info <- file.info(dir.files)
dir.file.info$name <- dir.files

newest.file <- dir.file.info %>%
  arrange(desc(mtime)) %>%
  filter(size > 0 & grepl("1c_sponsor_ident", name)) %>%
  top_n(1, mtime) %>%
  select(name)

load(newest.file$name)
rm(newest.file, dir.files, dir.file.info)

## ----wayback machine link generator--------------------------------------

# To do for party affiliation scraper
# If member dies, takes a federal job, gets indicted, etc., he will be removed from the legislature's page
# The response will be: "No profile available for this member."
# As such, we need to be able to check the wayback machine at archive.org for the original information
# The format for the request is: http://archive.org/wayback/available?url=example.com&timestamp=20060101

wayback.url <- function(cycle = NULL, session = NULL, sponsor = NULL) {

  replacement.url <- FALSE
  #wayback.url <- "http://archive.org/wayback/available?url=http://www.arkleg.state.ar.us/assembly/2017/2017R/Pages/MemberProfile.aspx?member=Branscum&timestamp=20170101"

  if (! anyNA(c(cycle, session, sponsor))) {

    url <- paste0(
    "http://archive.org/wayback/available?url=http://www.arkleg.state.ar.us/assembly/", cycle, "/", session, "/Pages/MemberProfile.aspx?member=", sponsor, "&timestamp=", cycle, "0101"
    )
    url <- gsub(" ", "%20", url)

    wayback.json <- httr::GET(url)
    jsonRespParsed <- httr::content(wayback.json, as="parsed")

    if (length(jsonRespParsed$archived_snapshots) > 0) {
      replacement.url <- jsonRespParsed$archived_snapshots$closest$url
      # ...Should be able to call same function using the replacement.url
    }
  }

  replacement.url
}

## ----party affiliation scraper setup-------------------------------------
# http://www.arkleg.state.ar.us/assembly/2017/2017R/Pages/MemberProfile.aspx?member=Branscum
# http://www.arkleg.state.ar.us/assembly/2017/2017R/Pages/MemberProfile.aspx?member=F.%20Allen
# sponsor.url <- "http://www.arkleg.state.ar.us/assembly/2017/2017R/Pages/MemberProfile.aspx?member=Collins-Smith"
# sponsor.url <- "http://www.arkleg.state.ar.us/assembly/2017/2017R/Pages/MemberProfile.aspx?member=Collins-Smith"
# sponsor.url <- "http://www.arkleg.state.ar.us/assembly/2017/2017R/Pages/MemberProfile.aspx?member=J.%20Hutchinson"
# sponsor.url <- "http://www.arkleg.state.ar.us/assembly/2017/2017R/Pages/MemberProfile.aspx?member=Standridge"
# temp.html <- read_html(sponsor.url) %>% html_text()

sponsor.lookup <- function(sponsor, cycle, session) {
  sponsor.url <- paste0(
    "http://www.arkleg.state.ar.us/assembly/",
    cycle, "/", session, "/Pages/MemberProfile.aspx?member=", sponsor
  )
  sponsor.url <- gsub(" ", "%20", sponsor.url)
  temp.html <- read_html(sponsor.url)
  use.wayback <- grepl("No profile available for this member.", html_text(temp.html))
  if (use.wayback) {
    sponsor.url <- wayback.url(cycle = cycle, session = session, sponsor = sponsor)
    if (sponsor.url != FALSE) {
      temp.html <- read_html(sponsor.url)
    } else {
      temp.html <- NULL
    }
  }

  if (! is_empty(temp.html)) {
    temp.sponsor <- temp.html %>%
      html_node(".SiteNames") %>%
      html_text()
    temp.party <- sub(".+?\\((\\w+)\\)", "\\1", temp.sponsor, perl = TRUE)

    temp.committees <- temp.html %>%
      html_nodes("td")

    temp.committees <- temp.committees[ grepl("CommitteeDetail", temp.committees) ]
    temp.committees <- temp.committees %>% html_text()
    temp.committees <- grep("^[^\r]", temp.committees, value = TRUE)
    temp.committees <- sub(".*:(.+?)", "\\1", temp.committees)
    temp.committees <- ifelse(
      length(temp.committees) == 0,
      temp.committees <- NA,
      temp.committees
    )

    temp.info <- temp.html %>%
      html_nodes(".InfoTable tr") %>%
      html_text(trim = T)

    if (grepl("^Rep", temp.sponsor)) {
      temp.chamber <- "House"
    } else if (grepl("^Sen", temp.sponsor)) {
      temp.chamber <- "Senate"
    } else {
      temp.chamber <- "Unknown"
    }

    temp.phone <- sub(
      "Phone([\\d-]+).*?",
      "\\1",
      temp.info[grepl("^Phone", temp.info)], # gives only the rows matching Phone
      perl = TRUE
    )[-1] #Phone matches twice, as all td elements are put into one list for some reason
    if (is_empty(temp.phone)) temp.phone <- NA

    temp.district <- sub(
      "District([\\d-]+).*?",
      "\\1",
      temp.info[grepl("^District", temp.info)], # gives only the rows matching District
      perl = TRUE
    )
    if (is_empty(temp.district)) temp.district <- NA

    temp.seniority <- sub(
      "Seniority Number([\\d-]+).*?",
      "\\1",
      temp.info[grepl("^Seniority Number|^Seniority", temp.info)], # gives only the rows matching Seniority
      perl = TRUE
    )
    if (is_empty(temp.seniority)) temp.seniority <- NA

    temp.occupation <- sub(
      "Occupation(.*?)",
      "\\1",
      temp.info[grepl("^Occupation", temp.info)], # gives only the rows matching Occupation
      perl = TRUE
    )
    if (is_empty(temp.occupation)) temp.occupation <- NA

    temp.church <- sub(
      "Church Affiliation(.*?)|Church(.*?)",
      "\\1",
      temp.info[grepl("^Church Affiliation|^Church", temp.info)], # gives only the rows matching Church
      perl = TRUE
    )
    if (is_empty(temp.church)) temp.church <- NA

    temp.veteran <- sub(
      "Veteran Status(.*?)|Veteran(.*?)",
      "\\1",
      temp.info[grepl("^Veteran Status|^Veteran", temp.info)], # gives only the rows matching Veteran
      perl = TRUE
    )
    if (is_empty(temp.veteran)) temp.veteran <- NA

    temp.public.service <- sub(
      "Public Service(.*?)",
      "\\1",
      temp.info[grepl("^Public Service", temp.info)], # gives only the rows matching Public
      perl = TRUE
    )
    if (is_empty(temp.public.service)) temp.public.service <- NA

    # temp.seniority <- as.numeric(temp.info[13])
    # temp.district <- as.numeric(temp.info[11])
    # temp.occupation <- temp.info[15]
    # temp.church <- temp.info[17]
    # temp.veteran <- ifelse(temp.info[19] == "Yes", TRUE, FALSE)
    # temp.public.service <- temp.info[21]

    # if (temp.chamber == "Senate") {
      # temp.seniority <- as.numeric(temp.seniority) * -1  # 1 is highest seniority
      temp.seniority <- as.numeric(temp.seniority)  # Change to preserve officially-reported seniority
      # } else {
      # temp.seniority <- -35 - as.numeric(temp.seniority) # Hosue seniority is generally lower than Senate, starts at -36
    # }

    temp.df <- data.frame(cycle, session, sponsor, temp.sponsor, temp.chamber, temp.party, temp.committees, temp.seniority, temp.district, temp.occupation, temp.church, temp.veteran, temp.public.service, stringsAsFactors = F)
    colnames(temp.df) <- c("cycle", "session", "sponsor", "sponsor.full.name", "chamber", "party", "committee", "seniority", "district", "occupation", "church", "veteran", "public_service")

    temp.df

  } else {
    temp.df <- data.frame(cycle, session, sponsor, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, stringsAsFactors = F)
    colnames(temp.df) <- c("cycle", "session", "sponsor", "sponsor.full.name", "chamber", "party", "committee", "seniority", "district", "occupation", "church", "veteran", "public_service")
  }
    temp.df
}

## ----distinct sponsors for scraping--------------------------------------

unique.sponsors <- unique(
  sponsorship %>%
    filter(! grepl("Budget|Committee|Management|Senate|Efficiency|Insurance & Commerce-House|State Agencies & Govt|Revenue & Taxation-House", sponsor)) %>%
    # filter(cycle >= 2001) %>%
    # filter(! grepl("Error.+pdf", cycle, ignore.case = TRUE, perl = TRUE)) %>%
    distinct(cycle, session, sponsor) #%>%
    #mutate(sponsor = as.factor(sponsor))
)

head(unique.sponsors)

## ----scrape sponsors-----------------------------------------------------

# Serial
sponsors.detail <- data.frame()
time.scrape.sponsor <- system.time({
  for (i in 1:NROW(unique.sponsors)) {
    message(unique.sponsors$sponsor[i])
    temp.df <- sponsor.lookup(unique.sponsors$sponsor[i], unique.sponsors$cycle[i], unique.sponsors$session[i])
    sponsors.detail <- rbind(sponsors.detail, temp.df)
    Sys.sleep(sample(seq(0.5, 2, by = 0.1),1)) # pause to avoid IP blocking
  }
})

# # Parallel
# registerDoParallel()
# time.scrape.sponsor <- system.time({
#   sponsors.detail <- foreach (sponsor = unique.sponsors$sponsor, cycle = unique.sponsors$cycle, session = unique.sponsors$session, .combine = rbind) %dopar% {
#     Sys.sleep(sample(seq(0.5, 2, by = 0.1), 1)) # pause to avoid IP blocking
#     sponsor.lookup(sponsor = sponsor, cycle = cycle, session = session)
#   }
# })

sponsors.detail <- as_tibble(sponsors.detail)

time.scrape.sponsor
rm(unique.sponsors, wayback.url, sponsor.lookup, time.scrape.sponsor)

save(
  acts.df, sponsors.detail, sponsorship,
  file = paste("./data-raw/1d_download_sponsor ",
               gsub(":", "", Sys.time()),
               ".RData",
               sep = ""),
  envir = .GlobalEnv,
  compress = "bzip2"
)
