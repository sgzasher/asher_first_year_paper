# Script Target: District-level financial data from the CCD.

# Packages ---------------------------------------------------------------------
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
set.seed(1966)
library(dplyr)
library(data.table)
library(haven)
library(purrr)
library(stringr)

# Function: Extract and Read Data ----------------------------------------------

# Function: checks for the files in a directory. If any zips; unzip them. Keep
# track of new files to remove them at the end if needed. Then fread all text
# files. Praying everything is a text file here....

read.data <- function(base.path){
  
  # Files in directory; note ZIPs for extraction
  files.orig <- list.files(base.path)
  files.orig.zip <- files.orig[str_sub(files.orig, -3, -1) == "zip"]
  files.orig.nozip <- files.orig[!(files.orig %in% files.orig.zip)]
  
  # Extract zips
  for(file in files.orig.zip){
    unzip(paste0(base.path, "/", file), 
          exdir = base.path)
  }
  
  # Note new files for removal
  files.generated = list.files(base.path)
  files.generated = files.generated[!(files.generated %in% files.orig)]

  # Should only need to read one file per directory
  for(file in c(files.orig.nozip, files.generated)){
    out.data = fread(paste0(base.path, "/", file),
                     colClasses = "character") %>%
      dplyr::filter(FIPST == "06")
  }
  
  # Remove
  for(file in files.generated){
    file.remove(paste0(base.path, "/", file))
  }
  
  # Return
  return(out.data)
  
}

# Gunning It -------------------------------------------------------------------
directory.top <- "../../data/raw/ccd/fiscal/"
directory.years <- list.files(directory.top)

df.fiscal <- read.data(paste0(directory.top, directory.years[1])) %>%
  dplyr::mutate(
    SY = directory.years[1]
  )

for(year in directory.years[2:17]){
  
  temp <- 
    read.data(
      paste0(directory.top, year)
    ) %>% 
    dplyr::mutate(
      SY = year
    )
  
  df.fiscal <- 
    bind_rows(
      df.fiscal,
      temp
    )
  
  rm(temp)
}

# Variable Selection -----------------------------------------------------------

# Repair variable names
colnames(df.fiscal) = gsub("_", "V", colnames(df.fiscal))

# Select columns and rename
df.fiscal <- 
  df.fiscal %>%
  dplyr::select(
    LEAID, SY, TOTALREV, TFEDREV, TSTREV, TLOCREV,
    C14, B11, C07, C01, T06, TOTALEXP, TCURELSC, 
    TCAPOUT, TNONELSE, Z32, Z33, 
    V19H, V21F, V31F, V41F, V61V, V66V, 
    W01, W31, W61
  )
  
names.variables <- c(
  "NCESDist", "SY", "Rev.Total", "Rev.Federal", "Rev.State", "Rev.Local",
  "Rev.Fed.TitleI", "Rev.Fed.Billingual", 
  "Rev.State.Billingual", "Rev.State.Basic",
  "Rev.Local.Property",
  "Exp.Total", "Exp.Instruction", "Exp.Capital", "Exp.NonEd", 
  "Exp.Salary.Total", "Exp.Salary.Instruction",
  "Debt.Long.Outstanding.Start", "Debt.Long.Issued", 
  "Debt.Long.Returned", "Debt.Long.Outstanding.End",
  "Debt.Short.Outstanding.Start", "Debt.Short.Outstanding.End",
  "Assets.Sinking", "Assets.Bond", "Assets.Other"
)

colnames(df.fiscal) = names.variables

# Export -----------------------------------------------------------------------
write.table(
  df.export,
  "../../data/output/built/cdd_controls_district.csv",
  append = FALSE,
  row.names = FALSE,
  sep = ","
)

