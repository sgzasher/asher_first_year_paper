# Script Target: Cleaning the CDD data (school & district level) to construct 
# a school-levle dataset (with some district-level controls). A later script 
# will construct a district-level dataset. 


# Packages ---------------------------------------------------------------------
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
set.seed(1966)
library(dplyr)
library(data.table)
library(stringr)
library(DescTools)
library(fixest)

# Read in file -----------------------------------------------------------------
df.cdd <- fread("../../data/output/analysis/cdd_analysis.csv")
df.sfp <- fread("../../data/output/analysis/sfp_analysis.csv")
frame.school <- fread("../../data/output/frames/frame_school.csv")
frame.district <- fread("../../data/output/frames/frame_district.csv")
df.tch <- fread("../../data/output/analysis/tch_analysis.csv")

df.mag <- fread("../../data/replication_directories/magazinnik/panel_agg.csv")
  
# Pre-Treatment Controls ------------------------------------------------
df.2001 <- df.cdd %>% dplyr::filter(SY == "2000-01") %>%
  dplyr::select(-V1, -SY, -NCESDist)

df.cdd <- df.cdd %>% dplyr::select(NCESSchool, NCESDist, SY, V1)

df.cdd <- left_join(df.cdd, df.2001, by = "NCESSchool", relationship = "many-to-one")

# Controls, Outcomes, Treatment ------------------------------------------------
df.sfp <- dplyr::select(df.sfp, V1, Tot.SFP.Scaled, SFP.Binary)

df.cdd <- 
  left_join(
    df.cdd, 
    df.sfp,
    by = c("V1")
  )

# Magazinnik needs the district CDS codes
dist.cds <- dplyr::select(frame.district, NCESDist, CDSCode)

df.cdd <- left_join(df.cdd, dist.cds, by = "NCESDist")

df.cdd$Year = as.integer(str_sub(df.cdd$SY, 1, 4))

df.mag$id = bit64::as.integer64(df.mag$id)
treatment <- dplyr::select(df.mag, id, year, trustee, switcher, switch.type)



df.cdd <- 
  left_join(
    df.cdd, 
    treatment,
    by = c("CDSCode" = "id", "Year" = "year")
  )

# Keep only the non-switchers and causal switchers
df.cdd <-
  dplyr::filter(
    df.cdd,
    switcher == 0 | (switcher == 1 & switch.type %in% c(1, 2))
  )

df.cdd$Title.I = as.factor(df.cdd$Title.I)


# Goal: highest-hispanic schools in segregated districts...

# District-level median hispanicity...
dist.hispan <- 
  df.cdd %>%
  summarize(
    .by = c("NCESDist", "SY"),
    Med.Hisp = median(Stu.Prop.Hisp, na.rm = T)
  )

df.cdd$HighSeg = as.factor(ifelse(df.cdd$Dist.Dissim > 0.3, 1, 0))
df.cdd <- 
  left_join(
    df.cdd, 
    dist.hispan
  )
df.cdd$HighHisp = as.factor(ifelse(df.cdd$Stu.Prop.Hisp > df.cdd$Med.Hisp,1, 0))
df.cdd$tch.per = Winsorize((1 / df.cdd$FTE.Stu.Ratio), probs = c(0.01, 0.99), na.rm = T)


# Teacher-Student Ratio -------------------
# Writing CSVs makes some duplicates for some reasion
df.tch = unique(df.tch[,-"V1"]) %>% dplyr::select(-Stu.Count.Reported, -FTE, -NCESDist)

df.cdd <- left_join(df.cdd, df.tch, by = c("NCESSchool", "SY"))


func.reg <- function(input){
  
  fmla = as.formula(paste0(input, " ~ (trustee * HighHisp) + Stu.Count.Reported + Rev.Local.Property.Scaled + Exp.Instruction.Scaled + Exp.Total.Scaled + Rev.Total.Scaled + Title.I + Stu.Prop.FRSM + Stu.Prop.White + Stu.Prop.Black + Stu.Prop.AAPI | NCESDist + SY"))
  
  print(fmla)
  
  output = list(
    feols(
      fmla, 
      data = df.cdd[df.cdd$HighSeg == 1,]
    ),
    feols(
      fmla, 
      data = df.cdd[df.cdd$HighSeg == 0,]
    ),
    feols(
      fmla, 
      data = df.cdd
    )
  )
  
  return(output)
  
}

# Five main policy input outcome variables (shockingly, not p-hacked!)
func.reg("SFP.Binary")
func.reg("Tot.SFP.Scaled")
func.reg("RT.FT")
func.reg("RT.H.FT.H")
func.reg("RT.EL.FT.H")

# Election outcome variables

# 
