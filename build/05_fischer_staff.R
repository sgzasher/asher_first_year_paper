# Script Target: Staff Data. Lots of variance across years in how this is 
# stored and formatted, so will need some attention.

# Packages ---------------------------------------------------------------------
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
set.seed(1966)
library(dplyr)
library(data.table)
library(haven)
library(purrr)
library(stringr)

# Read School Frame ------------------------------------------------------------
df.frame = fread("../../data/output/frames/frame_school.csv")

# File Formats -----------------------------------------------------------------

# From Fischer: the file names don't map to school year very well here
base.path = "../../data/raw/cde/fischer_staff"
files.all = list.files(base.path)
files.all = files.all[str_detect(files.all, "paif")]

# Files in the original format
suffix.formatA = c("98", "99", "00",
                   "03", "04", "05", 
                   "06", "07", "08")

files.formatA = files.all[str_sub(files.all, -6, -5) %in% suffix.formatA]

# 2001, 2002 in a slightly different format
suffix.formatB = c("01", "02")
files.formatB = files.all[str_sub(files.all, -6, -5) %in% suffix.formatB]

# 10/11 and 11/12
suffix.formatC = c("10", "11")
files.formatC = files.all[str_sub(files.all, -6, -5) %in% suffix.formatC]

# 09/10 needs unique handling; remaining years in a totally different format
# that I will handle differently
suffix.formatD = c("09")
files.formatD = files.all[st_sub(files.all, -6, -5) %in% suffix.formatD]

# Read File In Any Format ------------------------------------------------------

# Function: read individual file; all columns as characters, any file type
read.any <- function(file.name){
  
  # Can only read these types
  type.file = str_sub(file.name, -3, -1)
  if(!(type.file %in% c("txt", "dta", "tab"))){
    stop("file type must be txt, dta, or tab")
    return(10)
  }
  
  if(type.file %in% c("txt", "tab")){
    output = fread(file.name, colClasses = "character")
  }
  
  if(type.file == "dta"){
    output = read_dta(file.name) %>%
      dplyr::mutate(across(everything(), as.character))
  }
  
  return(output)
}

# Read: Format A ---------------------------------------------------------------

# Accurate school years
sy.formatA = c("2000-01", "2001-02", "2002-03",
               "2005-06", "2006-07", "2007-08",
               "2008-09", "2009-10", "2010-11")

# Read in first file
df.formatA = read.any(paste0(base.path, "/", files.formatA[1])) %>%
  dplyr::mutate(SY = sy.fomatA[1])

# Bind remaining
for(i in 2:length(files.formatA)){
  
  # Read in current file
  temp = read.any(paste0(base.path, "/", files.formatA[i])) %>%
    dplyr::mutate(ST = sy.formatA[i])
  
  # Append to the main file
  df.formatA <- 
    bind_rows(
      df.formatA, 
      temp
    )
  
  # Remove current file and loop 
  rm(temp)
}

# Basic cleaning and prep

bit64::as.integer64(CDS_CODE)




