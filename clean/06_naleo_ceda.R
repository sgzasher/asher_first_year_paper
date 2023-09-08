# Script Target: Cleaning candidate-level CEDA, inferring hispanicity.
# Note: from A&M. Proportion of Elected Board Members Who Were Latino
# I will add: proportion of those who ran who were latino. 
# Proportion of open seats with at least one latino candidate. 
# Also interesting for theory: ratio of candidates to open seats. 
# Note: broadly following the replication file for Fischer, who cleaned the 
# pre-2007 data in Stata. Just doing a bunch of work to copy that over to here. 

# Packages ---------------------------------------------------------------------
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
set.seed(1966)
library(dplyr)
library(data.table)
library(haven)
library(purrr)
library(readxl)
library(lubridate)
library(DescTools)
library(wru)
library(parallel)
library(stringr)
library(stringdist)

# Import Data ------------------------------------------------------------------
df.ceda = fread("../../data/output/built/ceda_candidates.csv")
df.naleo = fread("../../data/output/built/naleo_officials.csv")
df.school = fread("../../data/output/analysis/cdd_analysis.csv")

# Date Clean + SY --------------------------------------------------------------

# To lubridate
df.ceda$ELEC.Date = lubridate::as_date(df.ceda$ELEC.Date)

# List of school years
sy.list = unique(df.school$SY)
sy.topyear = str_sub(sy.list, 1, 4)
sy.botyear = str_sub(sy.list, -2, -1)


# Getting the school years covering the relevant year
index.base = rep(NA, nrow(df.ceda))
index.bot = rep(NA, nrow(df.ceda))

for(i in 1:nrow(df.ceda)){
  
  year = as.character(lubridate::year(df.ceda[i,]$ELEC.Date))
  
  out.base = which(sy.topyear %in% year)
  out.bot = which(sy.botyear %in% str_sub(year, -2, -1))
  
  if(length(out.base) == 0){
    index.base[i] = NA
  }
  else(
    index.base[i] = out.base
  )
  
  if(length(out.bot) == 0){
    index.bot[i] = NA
  }
  else(
    index.bot[i] = out.bot
  )
  
}

# And assigning to dataframe the appropriate school year (summers are the year
# before... i guess we could do that the other way too)
df.ceda$SY = 
  sy.list[
    ifelse(
      lubridate::month(df.ceda$ELEC.Date) >= 9,
      index.base,
      index.bot
    )
  ]

# Multiple Incumbency Flags ----------------------------------------------------

# Basic clearing out some garbage
df.ceda = df.ceda[!(duplicated(df.ceda[,-"V1"]))]
df.ceda = df.ceda[!(df.ceda$NCESDist == ""),]

# Individuals with multiple incumbency flags... #
incumb <-
  summarize(
    df.ceda,
    .by = "CandID",
    CAND.Inc = max(ifelse(CAND.Incumb == "Y", 1, 0), na.rm = T)
  ) 

incumb <- incumb %>%
  dplyr::mutate(
    CAND.Inc = ifelse(is.infinite(CAND.Inc), NA, CAND.Inc)
  )

df.ceda <- 
  left_join(
    df.ceda,
    incumb,
    by = "CandID"
  )

df.ceda <- df.ceda[,-"CAND.Incumb"]

# Now remove some more garbage
df.ceda = df.ceda[!(duplicated(df.ceda[,-c("V1", "Cand.BalDes")]))]

# Multiple Districts -----------------------------------------------------------

# Small set of cases with ambiguous district. We're gonna ignore them, naturally
numdists <- 
  summarize(
    df.ceda,
    .by = "RaceID",
    numdist = length(unique(NCESDist))
  )

df.ceda <- 
  left_join(
    df.ceda,
    numdists,
    by = "RaceID"
  )

# More garbage
df.ceda = df.ceda[df.ceda$numdist == 1,]

# Remaining Issues -------------------------------------------------------------

# Just want to check we have a consistent winning variable before dropping the 
# remaining cases...

winners <- 
  summarize(
    df.ceda,
    .by = "CandID",
    winner = min(CAND.Elected)
  )

df.ceda <- 
  left_join(
    df.ceda,
    winners,
    by = "CandID"
  )

df.ceda$CAND.Elected = df.ceda$winner

# Very small number of remaining problems; let's just get rid of them
df.ceda <- dplyr::select(df.ceda, -numdist, -winner)
sum(duplicated(df.ceda$CandID))
56/22372
df.ceda <- df.ceda[!(duplicated(df.ceda$CandID))]

# Tag for Ward Elections -------------------------------------------------------
numelec <- 
  summarize(
    df.ceda,
    .by = c("NCESDist", "ELEC.Date"),
    ELEC.Ward = ifelse(length(unique(RaceID)) > 1, 1, 0)
  )

df.ceda <- 
  left_join(
    df.ceda,
    numelec,
    by = c("NCESDist", "ELEC.Date")
  )

# Vote Share & K-Ratio ---------------------------------------------------------
df.kratio <- 
  summarise(
    df.ceda,
    .by = "RaceID",
    ELEC.KRatio = max(ELEC.CandNum/ELEC.K)
  )

#### WILL CITE THE FOLLOWING FIGURE IN TEXT #####
summary(df.kratio$ELEC.KRatio)

df.ceda <-
  df.ceda %>%
  dplyr::mutate(
    CAND.Share = CAND.Votes / ELEC.VoteTotal,
  )

df.ceda <- 
  left_join(
    df.ceda,
    df.kratio,
    by = "RaceID"
  )

# Names: Cleaning --------------------------------------------------------------

# only interested in final elections
df.ceda = df.ceda[df.ceda$ELEC.Ranoff == 0,]

# Name cleaning
df.ceda <-
  df.ceda %>%
  dplyr::mutate(
    name.full = paste0(Cand.FirstName, CAND.LastName)
  )

df.naleo <- 
  df.naleo %>%
  dplyr::mutate(
    name.full = paste0(FIRST_NAME, MI, LAST_NAME)
  )

df.ceda$name.full <- 
  df.ceda$name.full %>%
  str_replace_all("[:punct:]", "") %>%
  str_replace_all("\"", "") %>%
  str_replace_all(",", "") %>%
  str_replace_all("^c", "") %>%
  str_replace_all(" ", "") %>%
  toupper()

df.naleo$name.full <- 
  df.naleo$name.full %>%
  str_replace_all("[:punct:]", "") %>%
  str_replace_all("\"", "") %>%
  str_replace_all(",", "") %>%
  str_replace_all("^c", "") %>%
  str_replace_all(" ", "") %>%
  toupper()

df.naleo.ca <- 
  dplyr::filter(
    df.naleo,
    STATE == "CA"
  )

# Names: Merge Functions -------------------------------------------------------

# Function for fuzzy string matching
fzzy.fst <- function(choice.function, choice.max, df1, df2, choice.variables){
  
  v1 = getElement(df1, choice.variables[1])
  v2 = getElement(df2, choice.variables[2])
  
  returnme <-
    mclapply(
      v1,
      FUN = function(x){
        stringdist(x, v2, method = choice.function) < choice.max
      }
    )
  
  return(returnme)
}

# Names: Fuzzy Merge & BISG ----------------------------------------------------

# First way: do you ever match someone in any of the NALEO records?
join.naleo = 
fzzy.fst(
  "jw",
  0.1,
  df.ceda,
  df.naleo,
  c("name.full", "name.full")
)
df.ceda$HISP.NALEO = ifelse(unlist(lapply(join.naleo, sum)) > 0, 1, 0)

# Second way: do you ever match a californian representative in the records?
join.naleo.ca = 
  fzzy.fst(
    "jw",
    0.1,
    df.ceda,
    df.naleo.ca,
    c("name.full", "name.full")
  )
df.ceda$HISP.NALEO.CA = ifelse(unlist(lapply(join.naleo.ca, sum)) > 0, 1, 0)

# Third way: "BISG" Surnames
df.ceda$surname = df.ceda$CAND.LastName %>%
  str_replace_all("[:punct:]", "") %>%
  str_replace_all("\"", "") %>%
  str_replace_all(",", "") %>%
  str_replace_all("^c", "") %>%
  str_replace_all(" ", "") %>%
  toupper()

df.ceda =
wru::predict_race(voter.file = df.ceda, surname.only = T)

# Output -----------------------------------------------------------------------
write.csv(
  df.ceda,
  "../../data/output/analysis/ceda_candidate_level.csv"
)
