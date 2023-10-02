# Script Target: Constructing election/district-level outcome variables from
# inferred candidate hispanicity in CEDA. 

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
df.ceda = fread("../../data/output/analysis/ceda_candidate_level.csv")
df.naleo = fread("../../data/output/built/naleo_officials.csv")
df.school = fread("../../data/output/analysis/cdd_analysis.csv")

# Quick Name Fix ---------------------------------------------------------------
colnames(df.ceda)[2] = "V1.New"

# Individual Hispanicity Variables ---------------------------------------------

### Individual-level hispnicity codes:
# A: Only NALEO-CA (Binary)
# B: Only NALEO (Binary)
# C: NALEO-CA | BISG (Scaler)
# D: NALEO | BISG (Scaler)
# E: NALEO-CA | BISG90 (Binary)
# F: NALEO | BISG90 (Binary)

df.ceda <- 
  df.ceda %>%
  dplyr::mutate(
    HISP.C = ifelse(HISP.NALEO.CA == 1, 1, pred.his),
    HISP.E = ifelse(HISP.NALEO.CA == 1 | pred.his >= 0.9, 1, 0),
  )

# Time Trends ------------------------------------------------------------------
trend <- 
  df.ceda %>%
  summarise(
    .by = "SY",
    HISP.C = mean(HISP.C, na.rm = T)
  )

trend.winner <-
  df.ceda[df.ceda$CAND.Elected == 1,] %>%
  summarise(
    .by = "SY",
    HISP.C = mean(HISP.C, na.rm = T)
  )


pdf("../../output/data_section/trend_hispan_cand.pdf", width = 8.6, height = 6)
barplot(trend$HISP.C, names.arg = trend$SY,
        main = "Time Trends in Candiate Hispanicity",
        xlab = "School Year",
        ylab = "Expected Proportion of Candidates Hispanic")
dev.off()

pdf("../../output/data_section/trend_hispan_elec.pdf", width = 8.6, height = 6)
barplot(trend.winner$HISP.C, names.arg = trend$SY,
        main = "Time Trends in Seat Winner Hispanicity",
        xlab = "School Year",
        ylab = "Expected Proportion of Seat Winners Hispanic")
dev.off()

# Outcome Variables ------------------------------------------------------------

# Function: calculate electoral margin
margin <- function(raceid, sy){
  
  if(sum(is.na(sy)) == 0){
    df = df.ceda[df.ceda$RaceID == raceid & df.ceda$SY == sy,]
  }
  else{
    df = df.ceda[df.ceda$RaceID == raceid,]
  }
  
  votes = sort(df$CAND.Votes, decreasing = T)
  k = as.integer(mean(df$ELEC.K, na.rm = T))
  
  if(length(votes) > 1){
    margin = (votes[k] - votes[k+1]) / sum(votes, na.rm = T)
  }
  
  else(
    margin = 1
  )
  
  return(margin)
}

# Race level
outcomes.race <- 
  df.ceda %>%
  summarise(
    .by = c("RaceID", "SY"),
    NCESDist = first(NCESDist),
    ELEC.Ward = first(ELEC.Ward),
    Num.Elected = sum(CAND.Elected == 1),
    Num.Candidates = sum(length(unique(CandID))),
    KRatio = mean(ELEC.KRatio, na.rm = T),
    Sum.Win.Hisp.B = sum(ifelse(CAND.Elected == 1, HISP.NALEO, 0)),
    Sum.Win.Hisp.C = sum(ifelse(CAND.Elected == 1, HISP.C, 0)),
    Sum.Win.Hisp.E = sum(ifelse(CAND.Elected == 1, HISP.E, 0)),
    Sum.Ran.Hisp.B = sum(HISP.NALEO),
    Sum.Ran.Hisp.C = sum(HISP.C),
    Sum.Ran.Hisp.E = sum(HISP.E),
    VS.Hisp.B = sum(CAND.Share * HISP.NALEO),
    VS.Hisp.C = sum(CAND.Share * HISP.C),
    VS.Hisp.E = sum(CAND.Share * HISP.E),
    Incumbents = sum(CAND.Inc, na.rm = T),
    VoteTotal = first(ELEC.VoteTotal),
    Margin = margin(RaceID, SY)
  )

# District level...
outcomes.district <- 
  df.ceda %>%
  summarise(
    .by = c("NCESDist", "SY"),
    ELEC.Ward = first(ELEC.Ward),
    Num.Elected = sum(CAND.Elected == 1),
    Num.Candidates = sum(length(unique(CandID))),
    Sum.Win.Hisp.B = sum(ifelse(CAND.Elected == 1, HISP.NALEO, 0)),
    Sum.Win.Hisp.C = sum(ifelse(CAND.Elected == 1, HISP.C, 0)),
    Sum.Win.Hisp.E = sum(ifelse(CAND.Elected == 1, HISP.E, 0)),
    Sum.Ran.Hisp.B = sum(HISP.NALEO),
    Sum.Ran.Hisp.C = sum(HISP.C),
    Sum.Ran.Hisp.E = sum(HISP.E),
    Votes.Total = sum(CAND.Votes, na.rm = T),
    Votes.Hisp.B = sum(CAND.Votes * HISP.NALEO, na.rm = T),
    Votes.Hisp.C = sum(CAND.Votes * HISP.C, na.rm = T),
    Votes.Hisp.E = sum(CAND.Votes * HISP.E, na.rm = T)
  )

# And finishing those outcomes
outcomes.district <- 
  outcomes.district %>%
  dplyr::mutate(
    KRatio = ifelse(
      is.infinite(Num.Candidates / Num.Elected),
      NA,
      Num.Candidates / Num.Elected
    ),
    VS.Hisp.B = Votes.Hisp.B / Votes.Total,
    VS.Hisp.C = Votes.Hisp.C / Votes.Total,
    VS.Hisp.E = Votes.Hisp.E / Votes.Total
  ) %>%
  dplyr::select(
    NCESDist, SY, ELEC.Ward, Num.Elected, Num.Candidates,
    KRatio, Sum.Win.Hisp.B, Sum.Win.Hisp.C, Sum.Win.Hisp.E,
    Sum.Ran.Hisp.B, Sum.Ran.Hisp.C, Sum.Ran.Hisp.E,
    VS.Hisp.B, VS.Hisp.C, VS.Hisp.E
  )
  
# Write ------------------------------------------------------------------------
write.csv(
  outcomes.district,
  "../../data/output/analysis/election_outcomes_district.csv"
)

write.csv(
  outcomes.race,
  "../../data/output/analysis/election_outcomes_race.csv"
)
