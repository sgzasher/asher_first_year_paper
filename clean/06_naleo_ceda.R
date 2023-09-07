# Script Target: Get the school-level outcome variables at the district level. 
# Broadly, gini-coefficients for each of the outcome variables. 
# Note: from A&M. Proportion of Elected Board Members Who Were Latino
# I will add: proportion of those who ran who were latino. 
# Proportion of open seats with at least one latino candidate. 
# Also interesting for theory: ratio of candidates to open seats

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

# Import Data ------------------------------------------------------------------
df.ceda = fread("../../data/output/built/ceda_candidates.csv")
df.naleo = fread("../../data/output/built/naleo_officials.csv")
df.l2.m = fread("../../data/raw/L2/transform_2_output.csv")
df.l2.f = fread("../../data/raw/L2/transform_3_output.csv")

# Subset CEDA ------------------------------------------------------------------

# only interested in final elections
df.ceda = df.ceda[df.ceda$ELEC.Ranoff == 0,]

# Winners check against NALEO
df.ceda.winners <-
  df.ceda %>%
  dplyr::filter(
    CAND.Elected == 1 
  )

# Nonwinners have to go to the voter file
df.ceda.nonwinner <-
  df.ceda %>%
  dplyr::filter(
    CAND.Elected == 2
  )

# Free up memory
rm(df.ceda)

# Cleaning Names ---------------------------------------------------------------