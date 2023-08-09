# Script Target: Identify SPopulation/Universe of Relevant California Schools
# Restrictions: set of districts in Abott and Magazinnik

# Packages -------
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
data.xwalk <- fread("../../data/cde/pubschls.txt")
data.magazinnik <- fread("../../data/magazinnik/panel_agg.csv")

# District Population Choice ---------

choice.districts <- 
  c(0, 60, 52, 54, 56)

data.dists <- 
  data.xwalk %>%
  dplyr::mutate(
    disttag = ifelse(
      str_sub(CDSCode, -7, -1) == "0000000", 1, 0
    )
  ) %>% 
  dplyr::filter(
    disttag == 1,
    DOC %in% choice.districts,
    StatusType == "Active"
  )

# Magazinnik Data Coersion ------ 
data.magazinnik <- 
  data.magazinnik %>%
  dplyr::mutate(
    id = bit64::as.integer64(id)
  )  %>%
  dplyr::rename(
    CDSCode = id
  )

# Set of A&M Districts -------

# AT SOME POINT: FOLLOW UP AND FIGURE OUT WHY SOME DISTRICTS ARE MISSING
schools.mag <- 
  data.magazinnik %>%
  dplyr::select(
    CDSCode,
    switch.type
  ) %>%
  dplyr::mutate(
    inmag = 1
  ) %>%
  unique()

data.mag <-
  left_join(
    schools.mag,
    data.xwalk,
    by = "CDSCode"
  )

data.dists <-
  left_join(
    data.dists,
    schools.mag,
    by = "CDSCode"
  ) %>%
  dplyr::select(
    CDSCode,
    NCESDist,
    inmag
  ) %>%
  dplyr::filter(
    inmag == 1
  )

magcheck <-
  left_join(
    data.magazinnik,
    data.xwalk,
    by = "CDSCode"
  )

nonac = data.mag[data.mag$StatusType != "Active"]

