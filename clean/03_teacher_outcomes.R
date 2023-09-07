# Script Target: Teacher-level data and teacher-level outcomes. Including
# reconstructing simple FTE-student ratios; can also do raw teacher-student
# ratios, for example. If I could have a bunch of this stuff run + run by 
# Kate in the next few days that would be awesome.


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
df.tch <- fread("../../data/output/built/fischer_staff.csv")
frame.school <- fread("../../data/output/frames/frame_school.csv")
frame.dist <- fread("../../data/output/frames/frame_district.csv")
df.cdd <- fread("../../data/output/analysis/cdd_analysis.csv")

# Teacher Level Variable Construction ------------------------------------------
df.tch <- 
  dplyr::mutate(
    df.tch, 
    TCH.MastersPlus = ifelse(
      TCH.Edu %in% c("1", "2", "3", "D", "V", "M"),
      1,
      0
    )
  )

# School-Level Summaries -------------------------------------------------------
df.tch.school <- 
  summarize(
    df.tch,
    .by = c("CDSCode", "SY"),
    Tot.Staff = sum(ifelse(TCH.FTE == 1, 1, 1), na.rm = T),
    Tot.FT = sum(TCH.FTE, na.rm = T),
    Tot.EL = sum(Auth.ELD, na.rm = T),
    Tot.EL.FT = sum(ifelse(Auth.ELD == 1 & TCH.FTE == 1, 1, 0), na.rm = T),
    Tot.FC = sum(TCH.FullCred, na.rm = T),
    Tot.Hisp = sum(TCH.Hisp, na.rm = T),
    Tot.Hisp.FT = sum(ifelse(TCH.Hisp == 1 & TCH.FTE == 1, 1, 0), na.rm = T),
    Tot.MastersPlus = sum(TCH.MastersPlus, na.rm = T)
  )

# Number of Students -----------------------------------------------------------
df.students <- 
  df.cdd %>%
  dplyr::mutate(
    Stu.Count.Hisp = Stu.Count.Reported * Stu.Prop.Hisp
  ) %>%
  dplyr::select(
    NCESSchool, NCESDist, SY, Stu.Count.Reported, FTE, Stu.Count.Hisp
  )

xwalk.cds <- 
  frame.school %>%
  dplyr::mutate(
    NCESSchool = bit64::as.integer64(paste0(as.character(NCESDist), NCESSchool))
  ) %>%
  dplyr::select(CDSCode, NCESSchool)
  

df.tch.school <- left_join(
  df.tch.school,
  xwalk.cds,
  by = "CDSCode"
)

# Sometimes multiple NCESSchool are mapping to the same CDSCode... expected 
# behaviour! Since new CDS when merges etc. that is not reflected in NCES.

df.tch.school <- 
  summarize(
    df.tch.school,
    .by = c("NCESSchool", "SY"),
    Tot.Staff = sum(Tot.Staff, na.rm = T),
    Tot.FT = sum(Tot.FT, na.rm = T),
    Tot.EL = sum(Tot.EL, na.rm = T),
    Tot.EL.FT = sum(Tot.EL.FT, na.rm = T),
    Tot.FC = sum(Tot.FC, na.rm = T),
    Tot.Hisp = sum(Tot.Hisp, na.rm = T),
    Tot.Hisp.FT = sum(Tot.Hisp.FT, na.rm = T),
    Tot.MastersPlus = sum(Tot.MastersPlus, na.rm = T)
  )

# Now join since unique
df.tch.school <- 
  left_join(
    df.tch.school,
    df.students,
    by = c("NCESSchool", "SY")
  )

# School Level Variable Construction -------------------------------------------
df.tch.school <- 
  df.tch.school %>%
  dplyr::mutate(
    PC.FT = Tot.FT / Tot.Staff,
    PC.EL = Tot.EL / Tot.Staff,
    RT.FT = Tot.FT / Stu.Count.Reported,
    RT.Staff = Tot.Staff / Stu.Count.Reported,
    PC.EL.FT = Tot.EL.FT / Tot.FT,
    RT.EL.FT = Tot.EL.FT / Stu.Count.Reported,
    RT.EL.FT.H = Tot.EL.FT / Stu.Count.Hisp,
    PC.H.FT = Tot.Hisp.FT / Tot.FT,
    RT.H.FT = Tot.Hisp.FT / Stu.Count.Reported,
    RT.H.FT.H = Tot.Hisp.FT / Stu.Count.Hisp,
    PC.MasterPlus = Tot.MastersPlus / Tot.Staff,
    RT.EL = Tot.EL / Stu.Count.Reported
  )

# Winsorize those normalized by student counts...
df.tch.school <- 
  df.tch.school %>%
  dplyr::mutate(
    RT.FT = Winsorize(RT.FT, probs = c(0.01, 0.99), na.rm = T),
    RT.Staff = Winsorize(RT.Staff, probs = c(0.01, 0.99), na.rm = T),
    RT.EL.FT = Winsorize(RT.EL.FT, probs = c(0.01, 0.99), na.rm = T),
    RT.EL.FT.H = Winsorize(RT.EL.FT.H, probs = c(0.01, 0.99), na.rm = T),
    RT.H.FT = Winsorize(RT.H.FT, probs = c(0.01, 0.99), na.rm = T),
    RT.H.FT.H = Winsorize(RT.H.FT.H, probs = c(0.01, 0.99), na.rm = T),
    RT.EL = Winsorize(RT.EL, probs = c(0.01, 0.99), na.rm = T)
  )

# Restore missingness: FTE missing in 11-12, no auth variables (i.e. EL)
# from 2009-10 to 2011-12
col.a = c("RT.FT", "RT.H.FT", "RT.H.FT.H")
col.b = c("RT.EL.FT", "RT.EL.FT.H")

df.tch.school[col.a] <- lapply(
  df.tch.school[col.a],
  function(x){
    replace(x, df.tch.school$SY %in% c("2011-12"), NA)
  }
)

df.tch.school[col.b] <- lapply(
  df.tch.school[col.b],
  function(x){
    replace(x, df.tch.school$SY %in% c("2009-10", "2011-12", "2010-11"), NA)
  }
)

df.tch.school <- 
  df.tch.school %>%
  dplyr::mutate(
    across(
      c(RT.FT, RT.H.FT, RT.H.FT.H),
      ~ na_if(.x, SY %in% c("2011-12")),
    ),
    across(
      c(RT.EL.FT, RT.EL.FT.H),
      ~na_if(.x, SY %in% c("2009-10", "2011-12", "2010-11")),
    )
  )


# Write ------------------------------------------------------------------------
write.csv(
  df.tch.school,
  "../../data/output/analysis/tch_analysis.csv"
)
