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

# Outcome Variables ------------------------------------------------------------

outcomes <- 
  df.ceda %>%
  summarise(
    .by = c("RaceID", "SY"),
    Num.Elected,
    Num.Candidates,
    KRatio,
    
  )