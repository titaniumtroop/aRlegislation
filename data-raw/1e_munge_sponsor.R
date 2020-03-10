packages <- c("dplyr", "stringr", "tidyr", "xml2", "rvest")
#lapply(packages, install.packages)
lapply(packages, require, character.only = TRUE)
rm(packages)

# Load topic distributions from storage
dir.files <- list.files("./data-raw", "Rdata", full.names = TRUE, ignore.case = TRUE)
dir.file.info <- file.info(dir.files)
dir.file.info$name <- dir.files

newest.file <- dir.file.info %>%
  arrange(desc(mtime)) %>%
  filter(size > 0 & grepl("1d_download_sponsor", name)) %>%
  top_n(1, mtime) %>%
  select(name)

load(newest.file$name)
rm(newest.file, dir.files, dir.file.info)

## ----inspect missing sponsor details-------------------------------------
sponsors.detail %>%
  as_tibble() %>%
  filter(is.na(sponsor.full.name)) %>%
  select(cycle, session, sponsor, sponsor.full.name)

## ----fix missing sponsor details-----------------------------------------
# ~ 21 sponsors don't pull automatically, which is probably ok
# It looks like the errors come from: mispellings (Lowrey), or using initials in the acts where none was needed (e.g., T. Roebuck to Roebuck) or vice versa (e.g., Dobbins to S. Dobbins)
# We'll manually correct these errors

sponsors.detail <- sponsors.detail %>%
  filter(! (cycle == 2001 & session == "R" & sponsor == "T. Roebuck") )
sponsorship$sponsor[
  sponsorship$cycle == 2001 & sponsorship$session == "R" & sponsorship$sponsor == "T. Roebuck"
] <- "Roebuck"

sponsors.detail <- sponsors.detail %>%
  filter(! (cycle == 2001 & session == "R" & sponsor == "C.Taylor") )
sponsorship$sponsor[
  sponsorship$cycle == 2001 & sponsorship$session == "R" & sponsorship$sponsor == "C.Taylor"
] <- "C. Taylor"

sponsors.detail <- sponsors.detail %>%
  filter(! (cycle == 2001 & session == "R" & sponsor == "Lowrey") )
sponsorship$sponsor[
  sponsorship$cycle == 2001 & sponsorship$session == "R" & sponsorship$sponsor == "Lowrey"
] <- "Lowery"

sponsors.detail <- sponsors.detail %>%
  filter(! (cycle == 2001 & session == "R" & sponsor == "J. Lewellen") )
sponsorship$sponsor[
  sponsorship$cycle == 2001 & sponsorship$session == "R" & sponsorship$sponsor == "J. Lewellen"
] <- "Lewellen"

sponsors.detail <- sponsors.detail %>%
  filter(! (cycle == 2001 & session == "R" & sponsor == "Johnson") )
sponsorship$sponsor[
  sponsorship$cycle == 2001 & sponsorship$session == "R" & sponsorship$sponsor == "Johnson" & sponsorship$act == 1777
] <- "B. Johnson"

sponsors.detail <- sponsors.detail %>%
  filter(! (cycle == 2001 & session == "R" & sponsor == "Johnson") )
sponsorship$sponsor[
  sponsorship$cycle == 2001 & sponsorship$session == "R" & sponsorship$sponsor == "Johnson" & sponsorship$act == 724
] <- "C. Johnson"

sponsors.detail <- sponsors.detail %>%
  filter(! (cycle == 2001 & session == "R" & sponsor == "L. Thomas") )
sponsorship$sponsor[
  sponsorship$cycle == 2001 & sponsorship$session == "R" & sponsorship$sponsor == "L. Thomas"
] <- "Thomas"

sponsors.detail <- sponsors.detail %>%
  filter(! (cycle == 2001 & session == "R" & sponsor == "M Steele") )
sponsorship$sponsor[
  sponsorship$cycle == 2001 & sponsorship$session == "R" & sponsorship$sponsor == "M Steele"
] <- "M. Steele"

sponsors.detail <- sponsors.detail %>%
  filter(! (cycle == 2003 & session == "R" & sponsor == "J. Wood") )
sponsorship$sponsor[
  sponsorship$cycle == 2003 & sponsorship$session == "R" & sponsorship$sponsor == "J. Wood"
] <- "Wood"

# Two Praters as Reps this session, confirmed on webpage that Larry Prater is this sponsor
sponsors.detail <- sponsors.detail %>%
  filter(! (cycle == 2003 & session == "R" & sponsor == "Prater") )
sponsorship$sponsor[
  sponsorship$cycle == 2003 & sponsorship$session == "R" & sponsorship$sponsor == "Prater"
] <- "L. Prater"

sponsors.detail <- sponsors.detail %>%
  filter(! (cycle == 2005 & session == "R" & sponsor == "Creekmore") )
sponsorship$sponsor[
  sponsorship$cycle == 2005 & sponsorship$session == "R" & sponsorship$sponsor == "Creekmore"
] <- "D. Creekmore"

sponsors.detail <- sponsors.detail %>%
  filter(! (cycle == 2009 & session == "R" & sponsor == "Baker") )
sponsorship$sponsor[
  sponsorship$cycle == 2009 & sponsorship$session == "R" & sponsorship$sponsor == "Baker" & sponsorship$act == 211
] <- "G. Baker"

sponsors.detail <- sponsors.detail %>%
  filter(! (cycle == 2009 & session == "R" & sponsor == "Baker") )
sponsorship$sponsor[
  sponsorship$cycle == 2009 & sponsorship$session == "R" & sponsorship$sponsor == "Baker" & (sponsorship$act == 211 | sponsorship$act == 9)
] <- "G. Baker"

sponsors.detail <- sponsors.detail %>%
  filter(! (cycle == 2009 & session == "R" & sponsor == "Wyatt") )
sponsorship$sponsor[
  sponsorship$cycle == 2009 & sponsorship$session == "R" & sponsorship$sponsor == "Wyatt"
] <- "D. Wyatt"

sponsors.detail <- sponsors.detail %>%
  filter(! (cycle == 2013 & session == "2013R" & sponsor == "G. Leding") )
sponsorship$sponsor[
  sponsorship$cycle == 2013 & sponsorship$session == "2013R" & sponsorship$sponsor == "G. Leding"
] <- "Leding"

sponsors.detail <- sponsors.detail %>%
  filter(! (cycle == 2013 & session == "2013R" & sponsor == "G. McGill") )
sponsorship$sponsor[
  sponsorship$cycle == 2013 & sponsorship$session == "2013R" & sponsorship$sponsor == "G. McGill"
] <- "McGill"

sponsors.detail <- sponsors.detail %>%
  filter(! (cycle == 2013 & session == "2013R" & sponsor == "M. Broadaway") )
sponsorship$sponsor[
  sponsorship$cycle == 2013 & sponsorship$session == "2013R" & sponsorship$sponsor == "M. Broadaway"
] <- "Broadaway"

sponsors.detail <- sponsors.detail %>%
  filter(! (cycle == 2013 & session == "2013R" & sponsor == "M. Hodges") )
sponsorship$sponsor[
  sponsorship$cycle == 2013 & sponsorship$session == "2013R" & sponsorship$sponsor == "M. Hodges"
] <- "Hodges"

# One of the special sessions is blank for this sponsor, so we'll copy the information from the next special session
sponsors.detail[
  sponsors.detail$cycle == 2015 & sponsors.detail$session == "2015S1" & sponsors.detail$sponsor == "Gossage",
] <- sponsors.detail[
  sponsors.detail$cycle == 2015 & sponsors.detail$session == "2016S3" & sponsors.detail$sponsor == "Gossage",
]
sponsors.detail$session[
  sponsors.detail$cycle == 2015 & sponsors.detail$session == "2016S3" & sponsors.detail$sponsor == "Gossage"
][1] <- "2015S1"



## ----inspect incomplete sponsor details----------------------------------
# What's left
sponsors.detail[is.na(sponsors.detail$sponsor.full.name), c("cycle", "session", "sponsor", "chamber")]
# # A tibble: 6 x 3
#   cycle session sponsor
#   <dbl> <chr>   <fct>
# 1  2001 R       Hunt
# 2  2001 R       Webb
# 3  2005 R       J. Bookout # died in office, page not available, but was in office 2003
# 4  2009 2010F   Cole
# 5  2009 R       Cole
# 6  2009 R       Trusty


## ----fix remaining incomplete sponsor details--------------------------------------

sponsors.detail[
  sponsors.detail$cycle == 2005 & sponsors.detail$session == "R" & sponsors.detail$sponsor == "J. Bookout",
  ! names(sponsors.detail) %in% c("cycle", "seniority")
] <- sponsors.detail[
  sponsors.detail$cycle == 2003 & sponsors.detail$session == "R" & sponsors.detail$sponsor == "J. Bookout",
  ! names(sponsors.detail) %in% c("cycle", "seniority")
]

sponsors.detail[
  sponsors.detail$cycle == 2005 & sponsors.detail$session == "R" & sponsors.detail$sponsor == "Dobbins",
  ! names(sponsors.detail) %in% c("cycle", "seniority")
] <- sponsors.detail[
  sponsors.detail$cycle == 2003 & sponsors.detail$session == "R" & sponsors.detail$sponsor == "Dobbins",
  ! names(sponsors.detail) %in% c("cycle", "seniority")
]

sponsors.detail %>% filter(sponsor == "J. Bookout" | sponsor == "Dobbins")


## ----inspect missing sponsor chamber-------------------------------------
sponsors.detail %>%
  as_tibble() %>%
  filter(chamber != "House" & chamber != "Senate") %>%
  select(cycle, session, sponsor) #%>%
  #left_join(sponsorship)


## ----fix missing sponsor chamber-----------------------------------------
# Looked up in PDF
# 2009 Trusty is Senator
# 2009 Cole is Representative

# temp[temp$V1 == 2 & temp$V2 == 12, ] <- c(1:10)

# Detail from ballotpedia, https://ballotpedia.org/Arkansas_State_Senate_elections,_2008
sponsors.detail[
  sponsors.detail$cycle == 2009 &
  sponsors.detail$session == "R" &
  sponsors.detail$sponsor == "Trusty",
  c("chamber", "district", "sponsor.full.name", "party")
] <- c("Senate", 4, "Sharon Trusty", "R")

# Detail from ballotpedia, https://ballotpedia.org/Arkansas_House_of_Representatives_elections,_2008
sponsors.detail[
  sponsors.detail$cycle == 2009 &
  sponsors.detail$session == "2010F" &
  sponsors.detail$sponsor == "Cole",
  c("sponsor.full.name", "chamber", "party", "district")
] <- c("Steve Cole", "House", "D", 21)

sponsors.detail[
  sponsors.detail$cycle == 2009 &
  sponsors.detail$session == "R" &
  sponsors.detail$sponsor == "Cole",
  c("sponsor.full.name", "chamber", "party", "district")
  ] <- c("Steve Cole", "House", "D", 21)

# Detail from ballotpedia, https://ballotpedia.org/Arkansas_House_of_Representatives_elections,_2004
# There's an earlier Creekmore, later Dawn Creekmores use her initial, so we'll change that here
sponsors.detail[
  sponsors.detail$session == "R" &
  (sponsors.detail$cycle == 2005 | sponsors.detail$cycle == 2007) &
  sponsors.detail$sponsor == "Creekmore",
  c("chamber", "district", "sponsor.full.name", "sponsor", "party")
] <- c("House", 27, "Dawn Creekmore", "D. Creekmore", "D")

# Detail from Ballotpedia, https://ballotpedia.org/Arkansas_House_of_Representatives_elections,_2000
sponsors.detail[
  sponsors.detail$cycle == 2001 &
  sponsors.detail$session == "R" &
  sponsors.detail$sponsor == "Hunt",
  c("chamber", "sponsor.full.name", "district")
] <- c("House", "Russ Hunt", 68)

#Detail from Old legislature site, https://www.arkleg.state.ar.us/Legislators/Detail?ddBienniumSession=1999%2FR&member=Webb
sponsors.detail[
  sponsors.detail$cycle == 2001 &
  sponsors.detail$session == "R" &
  sponsors.detail$sponsor == "Webb",
  c("chamber", "sponsor.full.name", "district", "seniority", "occupation", "church", "public_service", "committee")
] <- c("Senate", "Doyle Webb", 14, 24, "Attorney", "Presbyterian", "Saline County Quorum Court: Justice of the Peace", "ALC-JBC BUDGET HEARINGS, ALC-REVIEW, ALC-PEER, ALC-LITIGATION REPORTS SUBCOMMITTEE, JUDICIARY COMMITTEE - SENATE, CITY, COUNTY & LOCAL AFFAIRS COMMITTEE - SENATE, CHILDREN AND YOUTH COMMITTEE - SENATE, RURAL FIRE DEPARTMENTS STUDY COMMITTEE, LEGISLATIVE AUDIT, LEGISLATIVE AUDIT-State Agencies, SENATE RULES, LEGISLATIVE COUNCIL")

# Detail from Ballotpedia, https://ballotpedia.org/Arkansas_House_of_Representatives_elections,_2010
sponsors.detail[
  sponsors.detail$cycle == 2011 &
  sponsors.detail$session == "2011R" &
  sponsors.detail$sponsor == "F. Smith",
  c("sponsor.full.name", "chamber", "district", "occupation")
] <- c("Fred Smith", "House", 54, "Harlem Globetrotter")


## ----inspect remaining missing profiles----------------------------------
sponsors.detail %>%
  filter(is.na(sponsor.full.name)) %>%
  left_join(sponsorship) %>%
  group_by(cycle, session, sponsor) %>%
  count()
# # A tibble: 6 x 4
# # Groups:   cycle, session, sponsor [6]
#   cycle session sponsor        n
#   <dbl> <chr>   <chr>      <int>
# 1  2001 R       Hunt          37
# 2  2001 R       Webb          19
# 3  2005 R       J. Bookout    58
# 4  2009 2010F   Cole           1
# 5  2009 R       Cole          32
# 6  2009 R       Trusty        19
#
# This represents 166 acts, some of which will have multiple sponsors


## ----ghost sponsors------------------------------------------------------

solo.ghost.sponsors <- sponsors.detail %>%
  filter(is.na(sponsor.full.name)) %>%
  left_join(sponsorship) %>%
  select(cycle, session, act) %>%
  left_join(sponsorship) %>%
  group_by(cycle, session, act) %>%
  count() %>%
  ungroup() %>%
  filter(n == 1) %>%
  summarize(sum = sum(n))
# 70 acts have just the one sponsor for which we don't have details
# This is about 0.5% of all acts, which I can live with for now



## ----clean party affiliations--------------------------------------------
sponsors.detail %>%
#  filter(is.na(party)) %>%
  distinct(cycle, chamber, sponsor, party, public_service) %>%
  group_by(cycle, party) %>%
  xtabs(~ cycle + party, data = .) %>%
  addmargins(.)


## ----inspect missing party info------------------------------------------
# No party information provided before 2007
sponsors.detail$party[
  sponsors.detail$party == "Representative" |
    sponsors.detail$party == "Senator"
] <- NA

# unique(sponsors.detail$party)

party.lookups <- sponsors.detail %>%
  filter(is.na(party)) %>%
  distinct(cycle, chamber, sponsor, sponsor.full.name, public_service) %>%
  as_tibble()

party.lookups$party <- NA
party.lookups$matches <- NA

party.lookups %>%
  filter( is.na(party)) %>%
  arrange(cycle, chamber, sponsor) %>%
  xtabs(~ cycle + chamber, data = .) %>%
  addmargins(.)



## ----download and parse ballotpedia--------------------------------------
# make ballotpedia request by cycle and chamber
# match sponsor name against ballotpedia list

candidate.list <- data.frame()

search.parameters <- expand.grid(chamber = c("House", "Senate"), year = seq(from = 2000, to = 2004, by = 2))

for (i in 1:NROW(search.parameters)) {

  if (search.parameters$chamber[i] == "House") {
    url <- paste0("https://ballotpedia.org/Arkansas_House_of_Representatives_elections,_", search.parameters$year[i])
  } else {
    url <- paste0("https://ballotpedia.org/Arkansas_State_Senate_elections,_", search.parameters$year[i])
  }

  ballot.results.html <- read_html(url) %>%
    html_nodes("dd") %>%
    html_text()

  parsed.ballot.results <- sub("\\s*\\w+ \\d+ (\\w)\\w+ .+?:\\s*", "\\1\n", ballot.results.html, perl = TRUE)

  split.ballot.results <- str_split_fixed(parsed.ballot.results, "\n", 5)

  temp.candidate.list <- data.frame(split.ballot.results) %>%
    filter(X1 == "R" | X1 == "D") %>%
    rename(party = X1) %>%
    gather(., key = "candidate", value = "name", -party) %>%
    select(-candidate) %>%
    filter(name != "" & name != "No candidates filed.") %>%
    mutate(
      name = sub("^(.+):.+$", "\\1", name)
    ) %>%
    mutate(
      winner = ifelse(
        grepl(" a$", name),
        TRUE,
        FALSE
      )
    ) %>%
    mutate(
      name = sub("^(.+?) a$", "\\1", name)
    )

  temp.candidate.list$cycle <- search.parameters$year[i] + 1
  temp.candidate.list$chamber <- as.character(search.parameters$chamber[i])

  candidate.list <- rbind(candidate.list, temp.candidate.list)

}

candidate.list <- candidate.list %>% distinct()

rm(search.parameters, temp.candidate.list, split.ballot.results, parsed.ballot.results, ballot.results.html, url)



## ----fix missing party with ballotpedia----------------------------------

map_grep <- function(x, y) {
  grepl(x, y)

  # mutate(
  #   chamber_cycle = paste0(chamber.x, ".+", cycle.x),
  #   chamber.match = as.logical(map2(chamber_cycle, public_service, map_grep))
  # ) %>%
  # filter(
  #   chamber.match == TRUE
  # ) %>%
}

for (i in 1:NROW(party.lookups)) {

  temp.name <- sub("\\. ", ".+?", party.lookups$sponsor[i])

  if (! is.na(party.lookups$chamber[i])) {

    if (party.lookups$chamber[i] == "Senate") {
      party.match <- candidate.list %>%
        filter(
          chamber == party.lookups$chamber[i] &
          (
            # Senate terms are 6 years, so election could've been in two previous cycles
            cycle == party.lookups$cycle[i] |
            cycle == party.lookups$cycle[i] - 2 |
            cycle == party.lookups$cycle[i] - 4
          ) &
          grepl(temp.name, name, perl = TRUE, ignore.case = TRUE)
        )
    } else {
      party.match <- candidate.list %>%
        filter(
          chamber == party.lookups$chamber[i] &
          cycle == party.lookups$cycle[i] &
          grepl(temp.name, name, perl = TRUE, ignore.case = TRUE)
        )
    }
  }

  if (NROW(party.match) == 0) {
    party.match <- candidate.list %>%
      mutate(
        chamber0.match = as.logical(map2(paste0(chamber, ".+", cycle - 0), party.lookups$public_service[i], map_grep)),
        chamber2.match = as.logical(map2(paste0(chamber, ".+", cycle - 2), party.lookups$public_service[i], map_grep)),
        chamber4.match = as.logical(map2(paste0(chamber, ".+", cycle - 4), party.lookups$public_service[i], map_grep))
      ) %>%
      filter(
          chamber2.match == TRUE | # Year of service
          chamber4.match == TRUE # appears in previous cycle
      ) %>%
      filter (
        chamber0.match == TRUE & # Current chamber appears in previous cycle
        grepl(temp.name, name, perl = TRUE, ignore.case = TRUE)
      )
  }

  # We only want results we're sure about, so we need one result, only
  # If we still have more than one result, choose someone listed as a winner
  if (NROW(party.match) > 1) {
    party.match <- party.match %>%
      filter(winner == TRUE)
  }

  # If all results are the same party, then it doesn't matter that we've have more than one result
  if (NROW(party.match) > 1) {
    party.match <- party.match %>%
      group_by(party) %>%
      count()
  }

  # If we've only returned one result, match that party
  if (NROW(party.match) == 1) {
    party.lookups$party[i] = as.character(party.match$party[1])
  } else {
    party.lookups$matches[i] = NROW(party.match)
  }
}


## ----show ballotpedia improvement----------------------------------------
party.lookups %>%
  filter( is.na(party)) %>%
  arrange(cycle, chamber, sponsor) %>%
  xtabs(~ cycle + chamber, data = .) %>%
  addmargins(.)

# Before implementing party_service matching
#       chamber
# cycle  House Senate Sum
#   2001     5     17  22
#   2003    49     10  59
#   2005     1      5   6
#   2009     1      0   1
#   Sum     56     32  88
# It appears many of the remaining candidates ran unopposed

# read_html("https://ballotpedia.org/Arkansas_House_of_Representatives_elections,_2002") %>%
#     html_nodes("h3") %>%
#     html_text() %>%
#     grepl("District", .) %>%
#     sum()
# [1] 49
# Yep, only 49 House districts had elections in 2002

# After implementing prior public_service matching
#       chamber
# cycle  House Senate Sum
#   2001     5     17  22
#   2003    33      8  41
#   2005     1      5   6
#   2009     1      0   1
#   Sum     40     30  70



## ----fix missing party with SOS 2000 election data-----------------------
sos.results.2000 <- readxl::read_xls("./data-raw/2k_primarysum2.xls")

names(sos.results.2000)[1] <- "candidate"
names(sos.results.2000)[2] <- "party"

sos.results.2000 <- sos.results.2000 %>%
  filter(! is.na(candidate)) %>%
  filter(! grepl("^Total|^Candidates", candidate)) %>%
  filter(! grepl("District \\d", candidate)) %>%
  mutate(
    party = ifelse(party == "Democrat", "D", party),
    party = ifelse(party == "Republican", "R", party)
  ) %>%
  filter()

party.lookups$SOS.matches <- NA

party.lookups <- party.lookups %>%
  mutate(sponsor.full.name = gsub("\\(|\\)", "", sponsor.full.name)) %>%
  mutate(sponsor.full.name = gsub("Senator|Representative", "", sponsor.full.name)) %>%
  mutate(sponsor.full.name = gsub("^\\s+|\\s+$", "", sponsor.full.name))

# party.lookups %>%
#   filter(cycle == 2001) %>%
#   filter(is.na(party))
#

for (i in 1:NROW(party.lookups)) {

  temp.full.name <- ifelse(
    is.na(party.lookups$sponsor.full.name[i]),
    "NeverGonnaMatchThisStringInASponsorName",
    gsub("\\s", ".+?", party.lookups$sponsor.full.name[i])
  )

  if (party.lookups$cycle[i] == 2001 & is.na(party.lookups$party[i])) {
    # see if this person in the lookup table is in the SOS results
    # print(party.lookups$sponsor[i])

    temp.name <- sub("\\. ", ".+?", party.lookups$sponsor[i])


    # if (party.lookups$sponsor[i] == "Broadway") {
    #   print(temp.full.name)
    # }
    #
    party.match <- sos.results.2000 %>%
      filter(
        grepl(temp.full.name, candidate, perl = TRUE, ignore.case = TRUE)
      )
    party.lookups$SOS.matches[i] <- NROW(party.match)

    if (NROW(party.match) == 1) {
      party.lookups$party[i] = party.match$party[1]
    } else {
      party.match <- sos.results.2000 %>%
        filter(
          grepl(temp.name, candidate, perl = TRUE, ignore.case = TRUE) |
          grepl(temp.full.name, candidate, perl = TRUE, ignore.case = TRUE)
        )
      party.lookups$SOS.matches[i] <- NROW(party.match)
      if (NROW(party.match) == 1) {
        party.lookups$party[i] = party.match$party[1]
      }
    }
  } else if (is.na(party.lookups$party[i])) {

    if(grepl("2001", party.lookups$public_service[i])) {

      # This gives members with public service in 2001
      # Need to match against election list, then assign to

      party.match <- sos.results.2000 %>%
        filter(
          grepl(temp.full.name, candidate, perl = TRUE, ignore.case = TRUE)
        )
      party.lookups$SOS.matches[i] <- NROW(party.match)
      if (NROW(party.match) == 1) {
        party.lookups$party[i] = party.match$party[1]
      } else {
        party.match <- sos.results.2000 %>%
          filter(
            grepl(temp.name, candidate, perl = TRUE, ignore.case = TRUE)
          )
        party.lookups$SOS.matches[i] <- NROW(party.match)
        if (NROW(party.match) == 1) {
          party.lookups$party[i] = party.match$party[1]
        }

      }

    }
  }
}

# party.lookups %>%
#   filter( is.na(party)) %>%
#   filter(grepl("2001", public_service))

party.lookups %>%
  filter( is.na(party)) %>%
  arrange(cycle, chamber, sponsor) %>%
  xtabs(~ cycle + chamber, data = .) %>%
  addmargins(.)



## ----fix missing party with info from other cycles-----------------------

replacements <- party.lookups %>%
  filter(is.na(party)) %>%
  select(cycle, chamber, sponsor, party) %>%
  left_join(
    sponsors.detail %>%
      filter(! is.na(party) & party != "unk"),
    by = "sponsor"
  ) %>%
  arrange(sponsor) %>%
  mutate(
    chamber_cycle = paste0(chamber.x, ".+", cycle.x),
    chamber.match = as.logical(map2(chamber_cycle, public_service, map_grep))
  ) %>%
  filter(
    chamber.match == TRUE
  ) %>%
  select(-cycle.y, -session, -chamber.y, -committee, -seniority, -district, -public_service, -chamber.match) %>%
  distinct()

for (i in 1:NROW(replacements)) {

  party.lookups$party[
    party.lookups$cycle == replacements$cycle.x[i] &
    party.lookups$chamber == replacements$chamber.x[i] &
    party.lookups$sponsor == replacements$sponsor[i]
  ] <- replacements$party.y[i]

}

party.lookups %>%
  filter( is.na(party)) %>%
  arrange(cycle, chamber, sponsor) %>%
  xtabs(~ cycle + chamber, data = .) %>%
  addmargins(.)



## ------------------------------------------------------------------------

manual.party.lookups <- party.lookups %>%
  filter(is.na(party)) %>%
  left_join(sponsorship, by = c("cycle", "sponsor")) %>%
  count(cycle, sponsor, sponsor.full.name, chamber, party) %>%
  arrange(desc(n))

# write.csv(manual.party.lookups, "manual_party_lookups.csv", row.names = FALSE)

manual.party.lookups.in <- read.csv("./data-raw/manual_party_lookups.csv", stringsAsFactors = FALSE)

# If manual file has been changed, update party.lookups
if (any(! is.na(manual.party.lookups.in$party))) {

  party.lookups <- party.lookups %>%
    filter(! is.na(party)) %>%
    bind_rows(manual.party.lookups.in)

}



## ----copy party.lookups into sponsors.detail-----------------------------

# sponsors.detail %>%
#   filter(is.na(party)) %>%
#   group_by(cycle, chamber) %>%
#   xtabs(~ cycle + chamber, data = .) %>%
#   addmargins(.)
# cycle
# 2001 2003 2005 2009  Sum
#  191  248  233    3  675

# party.lookups %>%
#   filter(is.na(party)) %>%
#   group_by(cycle, chamber) %>%
#   xtabs(~ cycle + chamber, data = .) %>%
#   addmargins(.)
#       chamber
# cycle  House Senate Sum
#   2001     5     15  20
#   2003    31      6  37
#   2005     1      4   5
#   2009     1      0   1
#   Sum     38     25  63

sponsors.detail <- sponsors.detail %>%
  left_join(
    party.lookups %>%
      rename(ballotpedia.party = party) %>%
      select(-matches, -public_service, -sponsor.full.name, -SOS.matches)
  ) %>%
  mutate(
    party = ifelse(is.na(party), ballotpedia.party, party)
  ) %>%
  select(-ballotpedia.party, -n)

#NROW(sponsors.detail %>% filter(is.na(party)))
# 104 -- not 64 because party.lookups doesn't include session info

#round(NROW(sponsors.detail %>% filter(is.na(party))) / NROW(sponsors.detail) * 100, 1)

## --- clean sponsor full name --------------------------------------------

sponsors.detail$sponsor.full.name <- gsub("^(Representative|Senator)\\s+", "", sponsors.detail$sponsor.full.name) # Remove Title at beginning
sponsors.detail$sponsor.full.name <- gsub("\\s+\\(\\w\\)\\s*$", "", sponsors.detail$sponsor.full.name) # Remove party designation
sponsors.detail$sponsor.full.name <- gsub("\\s+\\((Representative|Senator)\\)\\s*$", "", sponsors.detail$sponsor.full.name) # Remove chamber designation
sponsors.detail$sponsor.full.name <- trimws(gsub("\\s+", " ", sponsors.detail$sponsor.full.name)) # Standardize whitespace

# sponsors.detail %>%
#   distinct(sponsor.full.name, sponsor) %>%
#   arrange(sponsor)

## ---- clean committee membership of JS ------------------------------------

sponsors.detail$committee <- gsub(".+?General Assembly > .+? Session.+?Committees\\s*(.*)\\s*Bill\\s*Bill\\s*Status.*", "\\1", sponsors.detail$committee)

 ## ---- factor sponsor detail-----------------------------------------------

sponsors.detail$party[is.na(sponsors.detail$party)] <- "unk"

sponsors.detail <- sponsors.detail %>%
  mutate(
    party = factor(party, levels = c(unique(party)), ordered = T),
    chamber = factor(chamber, ordered = T),
    sponsor = factor(sponsor),
    sponsor.full.name = factor(sponsor.full.name),
    district = as.numeric(district)
  ) %>%
  arrange(cycle, session, sponsor) %>%
  as_tibble()

# sponsors.detail %>%
#   as_tibble() %>%
#   filter(is.na(chamber)) %>%
#   select(cycle, session, sponsor, chamber)



## ------------------------------------------------------------------------

rm(party.lookups, party.match, candidate.list, i, temp.name, solo.ghost.sponsors, map_grep, temp.full.name, local.filename, sos.results.2000, replacements, manual.party.lookups, manual.party.lookups.in)

save(
  acts.df, sponsors.detail, sponsorship,
  file = paste("./data-raw/1e_munge_sponsor ",
               gsub(":", "", Sys.time()),
               ".RData",
               sep = ""),
  envir = .GlobalEnv,
  compress = "bzip2"
)


