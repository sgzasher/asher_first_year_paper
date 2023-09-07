# Script Target: Get the SFP data at the school level ready for a first-pass 
# on regressions! Just want to start with "is there anything here at all"
# The real problems here: getting from date to school year (actually seems
# pretty hard) and inflating to 2016 dollars, then normalizing by enrollment
# and taking the windzorised outcomes. I really should winsorize EVERYTHING. 


# Packages ---------------------------------------------------------------------
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
set.seed(1966)
library(dplyr)
library(data.table)
library(haven)
library(purrr)
library(readxl)
library(lubridate)
library(DescTools)

# Data -------------------------------------------------------------------------
df.sfp <- fread("../../data/output/built/sfp_spending.csv")
frame.school <- fread("../../data/output/frames/frame_school.csv")
df.school <- fread("../../data/output/built/cdd_controls.csv")
df.cpi <- fread("../../data/raw/min_fed/cpi.csv")
df.cdd <- fread("../../data/output/analysis/cdd_analysis.csv")

# Clean CPI --------------------------------------------------------------------
colnames(df.cpi) = c("year", "index", "change")
df.cpi$year <- 
  str_replace(df.cpi$year, "\\*", "") %>%
  str_sub(., -4, -1) %>%
  as.numeric()

idx.2016 = df.cpi[df.cpi$year==2016,]$index
df.cpi$idx = df.cpi$index/idx.2016

# Date to Correct Format -------------------------------------------------------
df.sfp <- 
  df.sfp %>%
  dplyr::mutate(
  date_full_grant = 
    paste0(
      sub("^(\\d{1,2})/(\\d{1,2})/(\\d{4})$", "\\3", date_full_grant),
      "-",
      sub("^(\\d{1,2})/(\\d{1,2})/\\d{4}$", "\\1", date_full_grant), 
      "-",
      sub("^(\\d{1,2})/(\\d{1,2})/\\d{4}$", "\\2", date_full_grant)
    )
  )

df.sfp$date = lubridate::as_date(df.sfp$date_full_grant)

# Assign to school year --------------------------------------------------------

# List of school years
sy.list = unique(df.school$SY)
sy.topyear = str_sub(sy.list, 1, 4)
sy.botyear = str_sub(sy.list, -2, -1)


# Getting the school years covering the relevant year
index.base = rep(NA, nrow(df.sfp))
index.bot = rep(NA, nrow(df.sfp))

for(i in 1:nrow(df.sfp)){
  
  year = as.character(lubridate::year(df.sfp[i,]$date))
  
  out.base = which(sy.topyear %in% year)
  out.bot = which(sy.botyear %in% str_sub(year, -2, -1))
  
  if(length(out.base) == 0){
    index.base[i] = NA
  }
  else(
    index.base[i] = out.base
  )
  
  if(length(out.bot) == 0){
    index.bot[i] = NA
  }
  else(
    index.bot[i] = out.bot
  )
  
}

# And assigning to dataframe the appropriate school year (summers are the year
# before... i guess we could do that the other way too)
df.sfp$SY = 
sy.list[
ifelse(
  lubridate::month(df.sfp$date) >= 9,
  index.base,
  index.bot
)
]

# Inflate to 2016 dollars ------------------------------------------------------
df.sfp$year = lubridate::year(df.sfp$date)
df.sfp <- 
  left_join(
    df.sfp,
    df.cpi,
    by = "year"
  )

df.sfp$spending <- 
  as.numeric(str_replace(df.sfp$fund_total, "\\$", "") %>% 
               str_replace_all(., ",", ""))

df.sfp$spending = df.sfp$spending * df.sfp$idx

# Normalize by enrollment and winsorize ----------------------------------------
xwalk.cds <- 
  dplyr::select(
    frame.school,
    CDSCode,
    NCESDist,
    NCESSchool
  )

df.sfp <- 
  left_join(
    df.sfp,
    xwalk.cds,
    by = "CDSCode"
   )

df.sfp.school <- 
  df.sfp %>%
  summarize(
    .by = c("NCESDist", "NCESSchool", "SY"), 
    Tot.SFP = sum(spending, na.rm = T)
  ) %>%
  dplyr::mutate(
    NCESSchool = bit64::as.integer64(
      paste0(NCESDist, as.character(NCESSchool))
    )
  ) %>%
  dplyr::select(
    -NCESDist
  )

df.cdd <- 
  left_join(
    df.cdd,
    df.sfp.school,
    by = c("NCESSchool", "SY")
  ) %>%
  dplyr::mutate(
    Tot.SFP = ifelse(
      is.na(Tot.SFP), 
      0,
      Tot.SFP
    )
  )

df.cdd <- 
  df.cdd %>%
  dplyr::mutate(
    Tot.SFP.Scaled = Winsorize(Tot.SFP / Stu.Count.Reported,
                               probs = c(0.01, 0.99), na.rm = T)
  )

df.sfp.out <- 
  df.cdd %>%
  dplyr::select(
    NCESSchool, SY, Tot.SFP.Scaled, Stu.Count.Reported, Tot.SFP
  ) %>%
  dplyr::mutate(
    SFP.Binary = ifelse(Tot.SFP.Scaled > 0, 1, 0)
  )

# Export -----------------------------------------------------------------------
write.csv(
  df.sfp.out,
  "../../data/output/analysis/sfp_analysis.csv"
)

