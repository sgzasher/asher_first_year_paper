setwd(dirname(rstudioapi::getSourceEditorContext()$path))
set.seed(1966)
library(dplyr)
library(ggplot2)
library(data.table)
library(fuzzyjoin)
library(tidyverse)
library(rdrobust)
library(osmdata)
library(sf)
library(tigris)
options(tigris_use_cache = TRUE)
library(stringr)
library(parallel)
library(doParallel)
library(stringdist)

# Data Read -----------
data.xwalk <- fread("../data/cde/pubschls.txt")
data.sfp <- fread("../data/sfp/school-facility-program-funding.csv")

# Data Subsets -----------

# IIrrelevant Crosswalk Data
data.xwalk <-
  data.xwalk %>%
  dplyr::filter(
    (NCESSchool != "No Data")
  )

# For now: focus on Modernization grants; new constructions have weird names
# Should note this actively in the talk!
data.sfp <-
  data.sfp %>%
  dplyr::filter(
    (Program != "New Construction")
  )

# Only want inside study period
data.sfp <- 
  data.sfp %>%
  dplyr::mutate(
    Year = as.numeric(
      str_sub(Last_SAB_Date, -4, -1)
    )
  ) %>%
  dplyr::filter(
    Year %in% 2001:2016
  )

# Unique SFP Identifiers-----------

# String Standardization Step -----------

# 90% Threshold Crosswalk ----------- 

# Pull Data, Prep for Next Round -----------

# 80% Threshold Crosswalk -----------

# Disambiguation Step -----------