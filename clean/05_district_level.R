# Script Target: Get the school-level outcome variables at the district level. 
# Broadly, gini-coefficients for each of the outcome variables. 

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

# Import Data ------------------------------------------------------------------
df.cdd = fread("../../data/output/analysis/cdd_analysis.csv")
df.tch = fread("../../data/output/analysis/tch_analysis.csv")
df.sfp = fread("../../data/output/analysis/sfp_analysis.csv")
frame.dist = fread("../../data/output/frames/frame_district.csv")
frame.sch = fread("../../data/output/frames/frame_school.csv")

# SFP Outcomes -----------------------------------------------------------------

# Notice no Gini outcomes for this since so rare for multiple schools in a 
# district to get allocations...

nces.xwalk = dplyr::select(df.cdd, NCESDist, V1)
df.sfp = left_join(df.sfp, nces.xwalk, by = "V1")
df.sfp.dist <-
  summarize(
    df.sfp, 
    .by = c("NCESDist", "SY"), 
    Tot.SFP = sum(Tot.SFP, na.rm = T),
    Stu.Count.Reported = sum(Stu.Count.Reported, na.rm = T)
    )

df.sfp.dist <- 
  df.sfp.dist %>%
  dplyr::mutate(
    Tot.SFP.Scaled = Winsorize(
      Tot.SFP / Stu.Count.Reported,
      probs = c(0.01, 0.99),
      na.rm = T
    ),
    SFP.Binary = ifelse(Tot.SFP > 0, 1, 0)
  )

# Teacher Outcomes -------------------------------------------------------------
df.tch.dist <- 
  df.tch %>%
  dplyr::mutate(
    NCESDist = str_sub(as.character(NCESSchool), 1, 6)
  ) %>% 
  summarise(
    .by = c("NCESDist", "SY"),
    Tot.Stu = sum(Stu.Count.Reported, na.rm = T),
    Tot.Stu.Hisp = sum(Stu.Count.Hisp, na.rm = T),
    Tot.Tch = sum(Tot.FT, na.rm = T),
    Tot.H.FT = sum(Tot.Hisp.FT, na.rm = T),
    Tot.EL.FT = sum(Tot.EL.FT, na.rm = T),
    G.RT.FT = Gini(RT.FT, na.rm = T),
    G.RT.H.FT = Gini(RT.H.FT, na.rm = T),
    G.RT.H.FT.H = Gini(RT.H.FT.H, na.rm = T),
    G.RT.EL.FT = Gini(RT.EL.FT, na.rm = T),
    G.RT.EL.FT.H = Gini(RT.EL.FT.H, na.rm = T),
  ) %>%
  dplyr::mutate(
    G.RT.FT = ifelse(is.na(G.RT.FT), 0, G.RT.FT),
    G.RT.H.FT = ifelse(is.na(G.RT.H.FT), 0, G.RT.H.FT),
    G.RT.H.FT.H = ifelse(is.na(G.RT.H.FT.H), 0, G.RT.H.FT.H),
    G.RT.EL.FT = ifelse(is.na(G.RT.EL.FT), 0, G.RT.EL.FT),
    G.RT.EL.FT.H = ifelse(is.na(G.RT.EL.FT.H), 0, G.RT.EL.FT.H)
  ) %>%
  dplyr::mutate(
    Dist.RT.FT = Winsorize(Tot.Tch / Tot.Stu, probs = c(0.01, 0.99), na.rm = T),
    Dist.RT.H.FT = Winsorize(Tot.H.FT / Tot.Stu, probs = c(0.01, 0.99), na.rm = T),
    Dist.RT.H.FT.H = Winsorize()
    Dist.RT.EL.FT = Winsorize(),
    Dist.RT.EL.FT.H = Winsorize(),
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

  
func.reg("RT.FT")
func.reg("RT.H.FT.H")
func.reg("RT.EL.FT.H")
