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

# Data Read -----------
data.magazinnik <- fread("../data/magazinnik/panel_agg.csv")
data.xwalk <- fread("../data/cde/pubschls.txt")
data.sfp <- fread("../data/sfp/school-facility-program-funding.csv")

data.sfp = data.sfp[1:1000,]

# Identifiers to SFP ------------------------

# Key problem is that SFP just has district/school names, no more
# So we need to string match across them... let's see how well
# We can do... 

# Function for cleaning strings
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
data.sfp$School = clean_strings(data.sfp$School_Name)
data.sfp$County = clean_strings(data.sfp$County)
data.sfp$District = clean_strings(data.sfp$District)

data.xwalk$School = clean_strings(data.xwalk$School)
data.xwalk$County = clean_strings(data.xwalk$County)
data.xwalk$District = clean_strings(data.xwalk$District)

# Subsetting Xwalk data to real observations

keep.districttype = c(
  "Unified School District",  "High School District", 
  "Elementary School District"
)

keep.schooltype = c(
  "K-12 Schools (Public)", "High Schools (Public)", 
  "Elementary Schools (Public)", "Intermediate/Middle Schools (Public)", 
  "Junior High Schools (Public)", "Continuation High Schools"  
)


data.xwalk <- 
  data.xwalk %>%
  dplyr::filter(
    (StatusType == "Active" & 
       SOCType %in% keep.schooltype & 
       DOCType %in% keep.districttype)
  )

#regex_left_join(data.sfp, data.xwalk, 
#                by = c("School", "District", "County"))


# Might Just Brute Force This 

row.col.check <- function(single.row, col, check.value, check.col){
  
  out = rep(NA, length(col))

  for(i in 1:length(col)){
    
    if(check.value == check.col[i]){
      out[i] = (agrep(single.row, col[i]) == 1 || (agrep(col[i], single.row) == 1))
    }

    else{
      out[i] = FALSE
    }
    
    
  }
  out = ifelse(is.na(out), FALSE, out)
  return(out)

}


fuzzyjoin.sam <- function(df1, df2, vector.cols, subset.col){
  
  # Loop over the rows of df1
  # For each row, loop over the columns getting the match indices
  # Combine the match indices and return
  
  
  idx.data = list()

    for(row in 1:nrow(df1)){
    
    idx.row = list()
    subset.vector = getElement(df1, subset.col)
    row.subset = subset.vector[row]
    check.col = getElement(df2, subset.col)
    
    # Run the bilateral regex check
    for(col in vector.cols){
      ref = getElement(df1[row], col)
      column = getElement(df2, col)
      idx.row[[col]] = row.col.check(ref, column, row.subset, check.col)
    }
    
    # Now combine
    row.result = rep(TRUE, nrow(df2))
    for(col in vector.cols){
      row.result = row.result & idx.row[[col]]
    }
    idx.data[[row]] = row.result
    
  }
  
  return(idx.data)
  
  
}

fuzzyjoin.sam(data.sfp, 
              data.xwalk,
              c("School", "District"), 
              "County")
)




# This is going pretty well realistically... most important thing from here is 
# probably to parallelise the index code... because the next bit should be
# Pretty easy... 


# THIS STUFF NOT REAL YET -------------------------
test.sfp$CDSCode = 


idx.school = row.col.check(test.sfp$School[1],test.xwalk$School)
idx.district = row.col.check(test.sfp$District[1],test.xwalk$District)
idx.county = row.col.check(test.sfp$County[1],test.xwalk$County)

idx.school == T & idx.district == T & idx.county == T

# Prep Xwalk
xwalk.relevant <- 
  data.xwalk

# Testing 
function.agrep <- function(x, y){
  out = list()
  for(i in 1:length(x)){
    out[[i]] = agrep(x[i], y)
  }
  return(out)
}

fuzzyjoin.sam <- function(df1, df2, list.cols){
  
  idx.list = list()
  
  for(col in list.cols){
    col1 = getElement(df1, col)
    col2 = getElement(df2, col)
    idx.list[[col]] = function.agrep(col1,col2)
  }
  
  return(idx.list)
  
  
}

fuzzyjoin.sam(test.sfp, test.xwalk, c("District", "School"))

function.agrep(test.sfp$District, test.xwalk$District)

test.sfp = dplyr::filter(data.sfp, District == "AMADORCOUNTYOFFICEOFEDUCATION")
test.xwalk = dplyr::filter(xwalk.relevant, District == "AMADORCOUNTYOFFIEOFEDUATION")

test.data <- 
fuzzy_join(
  test.sfp,
  test.xwalk,
  multi_by = c("School_Name" = "School", "County", "District"),
  match_fun = function.agrep
)


function.agrep("AMADORCOUNTYOFFICEOFEDUCATION", "AMADORCOUNTYOFFIEOFEDUATION")


fuzzy_join(
  data.sfp,
  xwalk.relevant,
  by = c("School_Name" = "School", "County", "District"),
  match_fun = function.agrep
)

# Join
df = left_join(data.sfp, xwalk.relevant, by = c("School_Name" = "School", "County", "District"))
