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
suffix.formatA = c("03", "04", "05", 
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
files.formatD = files.all[str_sub(files.all, -6, -5) %in% suffix.formatD]

# Remaining years all need some extent of joining across files
suffix.formatE = c("12", "13", "14", "15", "16")
files.all = list.files(base.path)
files.formatE = files.all[str_detect(files.all, "Demo") | 
                          str_detect(files.all, "SchoolFTE") | 
                          str_detect(files.all, "schoolcodes")]

# Read File In Any Format ------------------------------------------------------

# Function: read individual file; all columns as characters, any file type
read.any <- function(file.name){
  
  # Can only read these types
  type.file = str_sub(file.name, -3, -1)
  if(!(type.file %in% c("txt", "dta", "tab", "TXT"))){
    stop("file type must be txt, dta, or tab")
    return(10)
  }
  
  if(type.file %in% c("txt", "tab", "TXT")){
    output = fread(file.name, colClasses = "character")
  }
  
  if(type.file == "dta"){
    output = read_dta(file.name) %>%
      dplyr::mutate(across(everything(), as.character))
  }
  
  return(output)
}

# Format A ---------------------------------------------------------------------

# Accurate school years
sy.formatA = c("2003-04", "2004-05", "2005-06", 
               "2006-07", "2007-08", "2008-09")

# Read in first file
df.formatA = read.any(paste0(base.path, "/", files.formatA[1])) %>%
  dplyr::mutate(SY = sy.formatA[1])

# Bind remaining
for(i in 2:length(files.formatA)){
  
  # Read in current file
  temp = read.any(paste0(base.path, "/", files.formatA[i])) %>%
    dplyr::mutate(SY = sy.formatA[i])
  
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
df.formatA <- 
  df.formatA %>% 
  dplyr::mutate_if(is.character, list(~na_if(., ""))) %>%
  dplyr::mutate(
    CDSCode = bit64::as.integer64(CDS_CODE),
    TCH.Male = ifelse(gender == "M", 1, 0),
    TCH.Male = ifelse(is.na(gender), NA, TCH.Male),
    TCH.Edu = ED_LEVEL, # Coming back to this when needed
    TCH.AAPI = ifelse(ETHNIC_GP %in% c("2", "3", "4"), 1, 0),
    TCH.Hisp = ifelse(ETHNIC_GP %in% c("5"), 1, 0),
    TCH.Black = ifelse(ETHNIC_GP %in% c("6"), 1, 0),
    TCH.White = ifelse(ETHNIC_GP %in% c("7"), 1, 0),
    TCH.RaceNA = ifelse(ETHNIC_GP %in% c("1"), 1, 0),
    TCH.Other = ifelse(ETHNIC_GP %in% c("8"), 1, 0),
    TCH.FTE = ifelse(as.numeric(PERC_TIME) >= 100 & F_P_TIME == "F", 1, 0),
    TCH.FullCred = as.numeric(FULL_CRED),
    Auth.GenElem = as.numeric(elem),
    Auth.GenSec = pmax(as.numeric(sec), as.numeric(GEN_SEC)), 
    Auth.Math = as.numeric(math),
    Auth.English = as.numeric(english),
    Auth.ELD = pmax(as.numeric(eld), as.numeric(sdaie))
  ) %>%
  dplyr::select(
    REC_ID, CDSCode, TCH.Male, TCH.Edu, TCH.AAPI, TCH.Hisp, TCH.Black, 
    TCH.White, TCH.RaceNA, TCH.Other, TCH.FTE, TCH.FullCred, 
    Auth.GenElem, Auth.GenSec, Auth.Math, Auth.English, Auth.ELD
  )

# Format B ---------------------------------------------------------------------

# Accurate school years
sy.formatB = c("2001-02", "2002-03")

# Read in first file
df.formatB = read.any(paste0(base.path, "/", files.formatB[1])) %>%
  dplyr::mutate(SY = sy.formatB[1])

# Only 2 for this format
df.formatB = bind_rows(
  df.formatB,
  read.any(paste0(base.path, "/", files.formatB[2])) %>%
    dplyr::mutate(SY = sy.formatB[2])
)

# Numeric variables easier to handle 
df.formatB <- 
  df.formatB %>%
  dplyr::mutate(
    across(
      ind:wht,
      .fns = ~as.numeric(case_match(.x, "T" ~ "1", "F" ~ "0"))
    )
  ) %>%
  dplyr::mutate(
    across(
      FULL_CRED:adult,
      .fns = ~as.numeric(case_match(.x, "Y" ~ "1", "N" ~ "0"))
    )
  )

# Basic cleaning and prep
df.formatB <- 
  df.formatB %>% 
  dplyr::mutate_if(is.character, list(~na_if(., ""))) %>%
  dplyr::mutate(
    CDSCode = bit64::as.integer64(CDS_CODE),
    TCH.Male = ifelse(gender == "M", 1, 0),
    TCH.Male = ifelse(is.na(gender), NA, TCH.Male),
    TCH.Edu = ED_LEVEL, # Coming back to this when needed
    TCH.AAPI = pmax(ind, chin, japn, kor, viet, asiaind, lao, cam,
                    othasn, haw, gua, sam, othpac, fil),
    TCH.Hisp = hisp,
    TCH.Black = blk,
    TCH.White = wht,
    TCH.RaceNA = 1 - pmax(ind, chin, japn, kor, viet, asiaind, lao, cam,
                          othasn, haw, gua, sam, othpac, fil,
                          hisp, blk, wht),
    TCH.Other = 0,
    TCH.FTE = ifelse(as.numeric(FTE_T) >=1, 1, 0),
    TCH.FullCred = FULL_CRED,
    Auth.GenElem = elem,
    Auth.GenSec = pmax(sec, GEN_SEC), 
    Auth.Math = math,
    Auth.English = english,
    Auth.ELD = pmax(as.numeric(eld), as.numeric(sdaie))
  ) %>%
  dplyr::select(
    REC_ID, CDSCode, TCH.Male, TCH.Edu, TCH.AAPI, TCH.Hisp, TCH.Black, 
    TCH.White, TCH.RaceNA, TCH.Other, TCH.FTE, TCH.FullCred, 
    Auth.GenElem, Auth.GenSec, Auth.Math, Auth.English, Auth.ELD
  )

# Format C ---------------------------------------------------------------------

# Accurate school years
sy.formatC = c("2010-11", "2011-12")

# Read in first file
df.formatC = read.any(paste0(base.path, "/", files.formatC[1])) %>%
  dplyr::mutate(SY = sy.formatC[1])

# Only 2 for this format
df.formatC = bind_rows(
  df.formatC,
  read.any(paste0(base.path, "/", files.formatB[2])) %>%
    dplyr::mutate(SY = sy.formatC[2])
)

# Basic cleaning and prep
df.formatC <- 
  df.formatC %>% 
  dplyr::mutate_if(is.character, list(~na_if(., ""))) %>%
  dplyr::mutate(
    CDSCode = bit64::as.integer64(CDS_CODE),
    TCH.Male = ifelse(GENDER == "M", 1, 0),
    TCH.Male = ifelse(is.na(GENDER), NA, TCH.Male),
    TCH.Edu = ED_LEVEL, # Coming back to this when needed
    TCH.AAPI = ifelse(ETHNIC_GP %in% c("2", "3", "4"), 1, 0),
    TCH.Hisp = ifelse(ETHNIC_GP %in% c("5"), 1, 0),
    TCH.Black = ifelse(ETHNIC_GP %in% c("6"), 1, 0),
    TCH.White = ifelse(ETHNIC_GP %in% c("7"), 1, 0),
    TCH.RaceNA = ifelse(ETHNIC_GP %in% c("0"), 1, 0),
    TCH.Other = ifelse(ETHNIC_GP %in% c("9", "1"), 1 ,0),
    TCH.FTE = ifelse(as.numeric(PCTTEACH) >= 100 & as.numeric(TEACH) == 1, 
                     1, 0),
    TCH.FullCred = NA,
    Auth.GenElem = NA,
    Auth.GenSec = NA, 
    Auth.Math = NA,
    Auth.English = NA,
    Auth.ELD = NA
  ) %>%
  dplyr::select(
    REC_ID, CDSCode, TCH.Male, TCH.Edu, TCH.AAPI, TCH.Hisp, TCH.Black, 
    TCH.White, TCH.RaceNA, TCH.Other, TCH.FTE, TCH.FullCred, 
    Auth.GenElem, Auth.GenSec, Auth.Math, Auth.English, Auth.ELD
  )

# Format D ---------------------------------------------------------------------

# Single year of data
df.formatD = read.any(paste0(base.path, "/", files.formatD[1])) %>%
  dplyr::mutate(SY = "2009-10")

# Basic cleaning and prep
df.formatD <- 
  df.formatD %>% 
  dplyr::mutate_if(is.character, list(~na_if(., ""))) %>%
  dplyr::mutate(
    CDSCode = bit64::as.integer64(CDS_CODE),
    TCH.Male = ifelse(gender == "M", 1, 0),
    TCH.Male = ifelse(is.na(gender), NA, TCH.Male),
    TCH.Edu = ed_level, # Coming back to this when needed
    TCH.AAPI = ifelse(ethnic_gp %in% c("2", "3", "4"), 1, 0),
    TCH.Hisp = ifelse(ethnic_gp %in% c("5"), 1, 0),
    TCH.Black = ifelse(ethnic_gp %in% c("6"), 1, 0),
    TCH.White = ifelse(ethnic_gp %in% c("7"), 1, 0),
    TCH.RaceNA = ifelse(ethnic_gp %in% c("0"), 1, 0),
    TCH.Other = ifelse(ethnic_gp %in% c("9", "1"), 1 ,0),
    TCH.FTE = ifelse(as.numeric(TEACH) >= 1, 1, 0),
    TCH.FullCred = NA,
    Auth.GenElem = NA,
    Auth.GenSec = NA, 
    Auth.Math = NA,
    Auth.English = NA,
    Auth.ELD = NA
  ) %>%
  dplyr::select(
    REC_ID, CDSCode, TCH.Male, TCH.Edu, TCH.AAPI, TCH.Hisp, TCH.Black, 
    TCH.White, TCH.RaceNA, TCH.Other, TCH.FTE, TCH.FullCred, 
    Auth.GenElem, Auth.GenSec, Auth.Math, Auth.English, Auth.ELD
  )

# Format E ---------------------------------------------------------------------

# Function for reading in any given file... 
# Notice: requires cleaning the credenial data
read.formatE <- function(suffix.year, file.list){
  
  # Directories
  dir.fischer = "../../data/raw/cde/fischer_staff"
  dir.cde = "../../data/raw/cde/cde_staff"
  
  # Get the relevant three files
  files.read <- file.list[str_detect(file.list, suffix.year)]
  file.demo <- files.read[str_detect(files.read, "Demo")]
  file.codes <- files.read[str_detect(files.read, "schoolcodes")]
  file.cred <- paste0("StaffCred", suffix.year, ".txt")
  
  # Read all in
  df.demo = read.any(paste0(dir.fischer, "/", file.demo))
  df.codes = read.any(paste0(dir.fischer, "/", file.codes))
  df.cred = read.any(paste0(dir.cde, "/", file.cred)) %>% 
    dplyr::select(RecID, CredentialType, AuthorizationType) %>%
    dplyr::mutate(
      TCH.FullCred = ifelse(CredentialType == "10", 1, 0), 
      Auth.GenElem = ifelse(AuthorizationType == "100", 1, 0),
      Auth.GenSec = ifelse(AuthorizationType == "107", 1, 0),
      Auth.Math = ifelse(AuthorizationType == "280", 1, 0),
      Auth.English = ifelse(AuthorizationType == "180", 1, 0),
      Auth.ELD = ifelse(AuthorizationType %in% c("370", "375", "420"), 1, 0)
    ) %>%
    dplyr::select(RecID, TCH.FullCred, Auth.GenElem, Auth.GenSec,
                  Auth.Math, Auth.English, Auth.ELD) %>%
    dplyr::summarize(
      .by = "RecID",
      TCH.FullCred = max(TCH.FullCred), 
      Auth.GenElem = max(Auth.GenElem), 
      Auth.GenSec = max(Auth.GenSec), 
      Auth.Math = max(Auth.Math), 
      Auth.English = max(Auth.English), 
      Auth.ELD = max(Auth.ELD), 
    )
  
  # Get district codes in 
  df.demo <-
    left_join(
      df.demo, 
      df.codes,
      by = c("RecID" = "recid", "DistrictCode" = "districtcode")
    )
  
  # And credential data
  df.demo <- 
    left_join(
      df.demo, 
      df.cred,
      by = c("RecID")
    )
  
  # And return
  return(df.demo)
}

# Accurate school years
sy.formatE = c("2012-13", "2013-14", "2014-15", 
               "2015-16", "2016-17")

# Read in first file
df.formatE = read.formatE(suffix.formatE[1], files.formatE) %>%
  dplyr::mutate(SY = sy.formatE[1])

# Bind remaining
for(i in 2:length(suffix.formatE)){
  
  # Read in current file
  temp = read.formatE(suffix.formatE[i], files.formatE) %>%
    dplyr::mutate(SY = sy.formatE[i])
  
  # Append to the main file
  df.formatE <- 
    bind_rows(
      df.formatE, 
      temp
    )
  
  # Remove current file and loop 
  rm(temp)
}

# Remaining cleaning
colnames(df.formatE) = gsub(" ", "\\.", colnames(df.formatE))

df.formatE <-
  df.formatE %>%
  dplyr::mutate(
    CDSCode = bit64::as.integer64(paste0(DistrictCode, school_code)),
    TCH.Male = ifelse(GenderCode == "M", 1, 0),
    TCH.Male = ifelse(is.na(GenderCode), NA, TCH.Male),
    TCH.Edu = EducationLevel, # Coming back to this when needed
    TCH.AAPI = ifelse(EthnicGroup %in% c("2", "3", "4"), 1, 0),
    TCH.Hisp = ifelse(EthnicGroup %in% c("5"), 1, 0),
    TCH.Black = ifelse(EthnicGroup %in% c("6"), 1, 0),
    TCH.White = ifelse(EthnicGroup %in% c("7"), 1, 0),
    TCH.RaceNA = ifelse(EthnicGroup %in% c("0"), 1, 0),
    TCH.Other = ifelse(EthnicGroup %in% c("9", "1"), 1 ,0),
    TCH.FTE = ifelse(as.numeric(FTE.Teaching) >= 100, 1, 0)
  ) %>%
  dplyr::rename(REC_ID = RecID) %>%
  dplyr::select(
    REC_ID, CDSCode, TCH.Male, TCH.Edu, TCH.AAPI, TCH.Hisp, TCH.Black, 
    TCH.White, TCH.RaceNA, TCH.Other, TCH.FTE, TCH.FullCred, 
    Auth.GenElem, Auth.GenSec, Auth.Math, Auth.English, Auth.ELD
  )


# Append and Compile -----------------------------------------------------------
df.out = 
  bind_rows(
    df.formatA, 
    df.formatB, 
    df.formatC, 
    df.formatD,
    df.formatE
  )

write.table(
  df.out,
  "../../data/output/built/fischer_staff.csv",
  append = FALSE,
  row.names = FALSE,
  sep = ","
)

