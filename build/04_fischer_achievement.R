# Script Target: Achievement data from Fischer. Will attempt standardization 
# later if I think of something...

# Packages ---------------------------------------------------------------------
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
set.seed(1966)
library(dplyr)
library(data.table)
library(haven)
library(purrr)
library(stringr)

# Get list for reading  --------------------------------------------------------
base.path = "../../data/raw/cde/fischer_achievement"
files.all = list.files(base.path)

# How do we need to read them in?
files.txt = files.all[str_sub(files.all, -3, -1) %in% c("txt", "TXT")]
files.tab = files.all[str_sub(files.all, -3, -1) %in% c("tab")]
files.dta = files.all[str_sub(files.all, -3, -1) %in% c("dta")]

# And are they the test files or the entity files?
files.test = files.all[str_detect(files.all, "test")]
files.entity = files.all[str_detect(files.all, "entity")]

# Read & Append Function -------------------------------------------------------

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

# Function; given list of files, read and append them all. 
read.files <- function(vector.names, directory.path){
  
  goto = getwd()
  setwd(directory.path)
  
  # Can only read vectors of names (otherwise, don't use this)
  n.read = length(vector.names)
  if(n.read == 1){
    stop("single file; just read it alone")
    return(11)
  }
  
  # Years for SY variable
  years.read = str_sub(vector.names, 1, 4)
  
  # Read in first
  output = read.any(vector.names[1])
  output$SY = years.read[1]
  
  # And read remaining
  for(i in 2:n.read){
    
    temp = read.any(vector.names[i])
    temp$SY = years.read[i]
    output <-
      bind_rows(
        output, 
        temp
      )
    rm(temp)
  }
  
  # Should all be neatly appended
  setwd(goto)
  return(output)
}

# Read: Test DTA Files ---------------------------------------------------------
files.read = files.test[files.test %in% files.dta]

df.dta = read.files(files.read, base.path)
