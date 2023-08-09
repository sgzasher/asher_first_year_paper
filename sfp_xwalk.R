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
data.fischer <- fread("../data/sfp/fischer/sfp_data.csv", header = T)

# Subset Data ---------------

# If active and merged, take just the active?
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

data.sfp <- 
  data.sfp %>%
  dplyr::mutate(
    Year = as.numeric(
      str_sub(Last_SAB_Date, -4, -1)
      )
  )

# String Standardization ---------------
clean_strings <- function(vector.string){
  vs = vector.string
  vs <- paste(str_extract_all(vs, '[:alnum:]*'))
  vs <- 
    vs %>%
    str_replace_all("[:punct:]", "") %>%
    str_replace_all("\"", "") %>%
    str_replace_all(",", "") %>%
    str_replace_all("^c", "") %>%
    str_replace_all(" ", "") %>%
    toupper()
  return(vs)
}

# Cleaning relevant
data.sfp$Stand_School = clean_strings(data.sfp$School_Name)
data.sfp$Stand_County = clean_strings(data.sfp$County)
data.sfp$Stand_District = clean_strings(data.sfp$District)

data.xwalk$Stand_School = clean_strings(data.xwalk$School)
data.xwalk$Stand_County = clean_strings(data.xwalk$County)
data.xwalk$Stand_District = clean_strings(data.xwalk$District)

# String Matching Script ---------------

# Fuzzy Join Code
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

# 'Global' Parameters
c.fun = "jw"
c.max = 0.1

# On Each Variab;e
out.school <- 
  fzzy.fst(
    choice.function = c.fun,
    choice.max = c.max, 
    df1 = data.sfp, 
    df2 = data.xwalk, 
    choice.variables = c("Stand_School", "Stand_School"))

out.dist <- 
  fzzy.fst(
    choice.function = c.fun,
    choice.max = c.max, 
    df1 = data.sfp, 
    df2 = data.xwalk, 
    choice.variables = c("Stand_District", "Stand_District"))

out.count <- 
  fzzy.fst(
    choice.function = c.fun,
    choice.max = c.max, 
    df1 = data.sfp, 
    df2 = data.xwalk, 
    choice.variables = c("Stand_County", "Stand_County"))

# Across Variables
out.total <- 
mclapply(
  X = 1:nrow(data.sfp),
  FUN = function(x){out.school[[x]] & out.dist[[x]] & out.count[[x]]}
)

## DISTRICT IS SOMETIMES WRONG; E.G. JACKSON JUNIOR HIGH IN AMADOR UNIFIED

# If first pass doesn't work, merge on JUST county and school name; consider match iff unique matcher...
out.nodist <- 
  mclapply(
    X = 1:nrow(data.sfp),
    FUN = function(x){out.school[[x]] & out.count[[x]]}
  )

# Stitch Data and Prepare For Second Round

# Get info from correct match...
data.sfp.crosswalk

print("Total: No Match/Unique/Many")
sum(as.vector(sapply(out.total, FUN = sum) == 0))
sum(as.vector(sapply(out.total, FUN = sum) == 1))
sum(as.vector(sapply(out.total, FUN = sum) > 1))

print("NoDist: No Match/Unique/Many")
sum(as.vector(sapply(out.nodist, FUN = sum) == 0))
sum(as.vector(sapply(out.nodist, FUN = sum) == 1))
sum(as.vector(sapply(out.nodist, FUN = sum) > 1))

print("Improvements?")
sum(as.vector(sapply(out.total, FUN = sum) == 0) & as.vector(sapply(out.nodist, FUN = sum) == 1))


##### Trying With 20% Threshold

c.max = 0.2

# On Each Variab;e
out.school2 <- 
  fzzy.fst(
    choice.function = c.fun,
    choice.max = c.max, 
    df1 = data.sfp, 
    df2 = data.xwalk, 
    choice.variables = c("Stand_School", "Stand_School"))

out.dist2 <- 
  fzzy.fst(
    choice.function = c.fun,
    choice.max = c.max, 
    df1 = data.sfp, 
    df2 = data.xwalk, 
    choice.variables = c("Stand_District", "Stand_District"))

out.count2 <- 
  fzzy.fst(
    choice.function = c.fun,
    choice.max = c.max, 
    df1 = data.sfp, 
    df2 = data.xwalk, 
    choice.variables = c("Stand_County", "Stand_County"))

# Across Variables
out.total2 <- 
  mclapply(
    X = 1:nrow(data.sfp),
    FUN = function(x){out.school2[[x]] & out.dist2[[x]] & out.count2[[x]]}
  )

## DISTRICT IS SOMETIMES WRONG; E.G. JACKSON JUNIOR HIGH IN AMADOR UNIFIED

# If first pass doesn't work, merge on JUST county and school name; consider match iff unique matcher...
out.nodist2 <- 
  mclapply(
    X = 1:nrow(data.sfp),
    FUN = function(x){out.school2[[x]] & out.count2[[x]]}
  )

print("Total: No Match/Unique/Many")
sum(as.vector(sapply(out.total, FUN = sum) == 0))
sum(as.vector(sapply(out.total, FUN = sum) == 1))
sum(as.vector(sapply(out.total, FUN = sum) > 1))

print("NoDist: No Match/Unique/Many")
sum(as.vector(sapply(out.nodist, FUN = sum) == 0))
sum(as.vector(sapply(out.nodist, FUN = sum) == 1))
sum(as.vector(sapply(out.nodist, FUN = sum) > 1))

print("Improvements?")
sum(as.vector(sapply(out.total, FUN = sum) == 0) & as.vector(sapply(out.nodist, FUN = sum) == 1))

# Literally Just Going Raw 
p <- 
left_join(
  data.sfp,
  data.xwalk,
  by = c("Stand_School", "Stand_District", "Stand_County"),
  relationship = "many-to-many"
)

sum(!is.na(p$NCESSchool))
