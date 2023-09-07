# Script Target: NALEO data. Mostly just spread across many files. Notice that, 
# in contrast to the CEDA data, we don't need to spend a lot of time trying to
# stitch stuff to the 

# Packages ---------------------------------------------------------------------
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
set.seed(1966)
library(dplyr)
library(data.table)
library(haven)
library(purrr)
library(readxl)
library(acs)

