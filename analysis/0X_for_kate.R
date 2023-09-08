# Script Target: Some regressions for Kate. Can write up over the next few 
# days. 

# Packages ---------------------------------------------------------------------
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
set.seed(1966)
library(dplyr)
library(data.table)
library(stringr)
library(DescTools)
library(fixest)

# Read in file -----------------------------------------------------------------

# Controls and outcomes - school level
df.cdd <- fread("../../data/output/analysis/cdd_analysis.csv")
df.sfp <- fread("../../data/output/analysis/sfp_analysis.csv")
df.tch <- fread("../../data/output/analysis/tch_analysis.csv")
df.cen <- fread("../../data/output/analysis/census_school.csv")

# District level
df.sfp.d <- fread("../../data/output/analysis/sfp_analysis_dist.csv")
df.tch.d <- fread("../../data/output/analysis/tch_analysis_dist.csv")
df.cen.d <- fread("../../data/output/analysis/census_district.csv")

# Electoral
df.mag <- fread("../../data/replication_directories/magazinnik/panel_agg.csv")
df.ele <- fread("../../data/output/analysis/election_outcomes_district.csv")

# Frames
frame.school <- fread("../../data/output/frames/frame_school.csv")
frame.district <- fread("../../data/output/frames/frame_district.csv")

# Get A&M Causal Sample --------------------------------------------------------

# Get relevant from A&M; also format CDS
df.mag <- 
  df.mag %>%
  dplyr::mutate(
    CDSCode = bit64::as.integer64(id)
  ) %>%
  dplyr::select(
    CDSCode, trustee, switcher, switch.type, 
    Spanish.Surname, Members, Hispanic, year
  ) %>%
  dplyr::mutate(
    Prop.Board = Spanish.Surname / Members,
    causal = ifelse(
      Prop.Board <= Hispanic & Hispanic >= 15, 1, 0
    )
  ) %>%
  dplyr::select(
    CDSCode, trustee, switcher, switch.type, causal, year
  )

# Then stitch in NCESDist
df.mag <- 
  left_join(
    df.mag,
    dplyr::select(frame.district, CDSCode, NCESDist),
    by = "CDSCode"
  )

# And change year to SY
df.mag <- 
  df.mag %>%
  dplyr::mutate(
    SY = paste0(as.character(year), 
                "-",
                str_sub(
                  as.character(year+1),
                  3,
                  4
                )
          )
  )

# School-level Census Controls -------------------------------------------------
