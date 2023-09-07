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
library(lubridate)

# Function: Standardize column names -------------------------------------------

stand.colnames <- function(name.vector){
  
  # Upper; no spaces
  output <-
    str_replace_all(
      toupper(name.vector),
      " ",
      "_"
    )
  
  # Party affil
  output <- 
    ifelse(
      grepl("PARTY", output), 
      "PARTY",
      output
    )
  
  # Fax number
  output <- 
    ifelse(
      grepl("FAX", output), 
      "FAX",
      output
    )
  
  # Level
  output <-
    ifelse(
      grepl("LEVEL", output),
      "LEVEL",
      output
    )
  
  return(output)

}


# Read and Append --------------------------------------------------------------
base.path = "../../data/raw/naleo"
list.files = list.files(base.path)
list.files = list.files[str_detect(list.files, ".xlsx")]

# Read first
df.naleo <- 
  read_excel(
    paste0(
      base.path, 
      "/", 
      list.files[grepl("2000", list.files)]
      ),
    col_types = "text"
    )

# Standardize...
colnames(df.naleo) = stand.colnames(colnames(df.naleo))
df.naleo$YEAR = "2000"

# Read remaining
for(i in as.character(2001:2021)){
  
  # Read current
  temp = read_excel(
    paste0(
      base.path,
      "/",
      list.files[grepl(i, list.files)]
    ),
    col_types = "text"
  )
  
  # Colnames
  colnames(temp) = stand.colnames(colnames(temp))
  temp$YEAR = i
  
  # Append
  df.naleo <- 
    bind_rows(
      df.naleo, 
      temp
    )
  
  # Remove
  rm(temp)
  
}

# Column Selection -------------------------------------------------------------
df.naleo <- 
  df.naleo %>%
  dplyr::select(
    STATE, FIRST_NAME, MI, M.I., LAST_NAME, 
    PARTY, GENDER, LEVEL, YEAR
  )

# Cleaning ---------------------------------------------------------------------

# Fix middle initials
df.naleo <- 
  df.naleo %>%
  dplyr::mutate(
    MI = ifelse(
      is.na(MI), 
      M.I., 
      MI
    )
  ) %>%
  dplyr::select(
    - M.I.
  )

# Refactor variables
df.naleo <- 
  df.naleo %>%
  dplyr::mutate(
    LEVEL = case_match(
      LEVEL,
      "s" ~ "U.S. Senator",
      "a" ~ "U.S. Representative",
      "b" ~ "Governor",
      "c" ~ "State Executive",
      "d" ~ "State Senator",
      "e" ~ "State Representative",
      "f" ~ "County",
      "g" ~ "Municipal",
      "h" ~ "Judicial or Law Enforcement",
      "i" ~ "School Board",
      "x" ~ "Special District",
    )
  ) %>%
  dplyr::mutate(
    PARTY = case_match(
      PARTY,
      "DEM" ~ "D",
      "GOP" ~ "R",
      "N-P" ~ "NP",
      "*" ~ "Not Reported",
      "IND" ~ "I"
    )
  ) %>%
  dplyr::mutate(
    GENDER = case_when(
      grepl("M", toupper(GENDER)) ~ "M",
      grepl("F", toupper(GENDER)) ~ "F"
    )
  )

# Write ------------------------------------------------------------------------
write.csv(
  df.naleo,
  "../../data/output/built/naleo_officials.csv"
)

