# Script Target: GDID Regressions 

# Packages ---------------------------------------------------------------------
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
set.seed(1966)
library(dplyr)
library(data.table)
library(stringr)
library(DescTools)
library(fixest)
library(stargazer)
lib

# Data -------------------------------------------------------------------------

# write.csv struggles with long files
csv.fix <- function(df){
  df = df[,-"V1"]
  df = df[!(duplicated(df))]
  return(df)
}

# Read and fix
cdd.school = 
cen.school = 
