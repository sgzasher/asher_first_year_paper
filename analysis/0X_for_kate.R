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
library(stargazer)

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


# District-level Census Variables ----------------------------------------------

# Just need dissim & Hispanicity 
df.cen.d <- 
  df.cen.d %>%
  dplyr::select(
    CEN.Dissim, NCESDist, FXZ001, FXS001
  ) %>%
  dplyr::mutate(
    Prop.Hisp = FXZ001 / FXS001
  ) %>%
  dplyr::select(
    -FXZ001, -FXS001
  )

# District-level Dataset -------------------------------------------------------

analysis.dist <- 
  left_join(
    df.sfp.d,
    df.tch.d,
    by = c("NCESDist", "SY")
  )

analysis.dist <- 
  left_join(
    analysis.dist,
    df.cen.d,
    by = "NCESDist"
  )

analysis.dist <- 
  left_join(
    analysis.dist,
    df.ele,
    by = c("NCESDist", "SY")
  )

cds.codes = frame.district[,2:3]

analysis.dist <- 
  left_join(
    analysis.dist,
    cds.codes,
    by = "NCESDist"
  )

analysis.dist <- 
  left_join(
    analysis.dist,
    df.mag[,-"NCESDist"],
    by = c("CDSCode", "SY")
  )

#### Inclusion indicators ####
analysis.dist$include <- 
  ifelse(
    analysis.dist$causal == 1 &
      (analysis.dist$switcher == 0 | 
         (analysis.dist$switcher == 1 & analysis.dist$switch.type %in% c(1, 2))
      ),
    1,
    0
  )

analysis.dist$include2 <- 
  ifelse(
    analysis.dist$causal == 1,
    1,
    0
  )

#### Getting median hispanicity and segregation for treated units
summary(analysis.dist[analysis.dist$include == 1 & analysis.dist$switcher == 1,]$Prop.Hisp)
summary(analysis.dist[analysis.dist$include == 1 & analysis.dist$switcher == 1,]$CEN.Dissim)

# High and low segregation districts
seg.dist <- 
  summarise(
    analysis.dist,
    .by = "NCESDist",
    Dissim = first(CEN.Dissim)
  ) %>%
  dplyr::mutate(
    CEN.HighSeg = as.factor(
      ifelse(
        Dissim > 0.3754,
        "Yes",
        "No"
      )
    )
  ) %>%
  dplyr::select(
    -Dissim
  )

# Join
analysis.dist <- 
  left_join(
    analysis.dist,
    seg.dist,
    by = "NCESDist"
  )

# High and low hispanic districts
seg.dist <- 
  summarise(
    analysis.dist,
    .by = "NCESDist",
    Hisp = first(Prop.Hisp)
  ) %>%
  dplyr::mutate(
    CEN.HighHisp = as.factor(
      ifelse(
        Hisp > 0.3726,
        "Yes",
        "No"
      )
    )
  ) %>%
  dplyr::select(
    -Hisp
  )

# Join
analysis.dist <- 
  left_join(
    analysis.dist,
    seg.dist,
    by = "NCESDist"
  )





# District-level Analyses  -----------------------------------------------------

# Basic loop

dist.analysis <- function(outcome.variable){
  
  # Models
  model.basic = " ~ (trustee * Prop.Hisp) | NCESDist + SY"
  model.noint = " ~ (trustee) | NCESDist + SY"
  
  # Dictionary
  dict.vec =  c("trustee" = "Ward",
                "trustee x \\High Hispanic" = "Ward x CEN.HighHispYes",
                "treat:domgroup::Hisp:isdom" = "Treat x Hispanic Dominant",
                "treat:domgroup::White:isdom" = "Treat x White Dominant",
                "treat" = "Treat")
  
  ## First, in causal sample only
  col1 = feols(as.formula(paste0(outcome.variable, model.noint)),
               analysis.dist[analysis.dist$include == 1,])
  col2 = feols(as.formula(paste0(outcome.variable, model.basic)),
               analysis.dist[analysis.dist$include == 1,])
  col3 = feols(as.formula(paste0(outcome.variable, model.basic)),
               analysis.dist[analysis.dist$include == 1 & 
                             analysis.dist$CEN.HighSeg == "Yes",])
  col4 = feols(as.formula(paste0(outcome.variable, model.basic)),
               analysis.dist[analysis.dist$include == 1 & 
                               analysis.dist$CEN.HighSeg == "No",])
  
  # Output for causal sample
  tab1 = etable(
    col1, col2, col3, col4,
    dict = dict.vec,
    se.below = T,
    signif.code = c("***"=0.01, "**"=0.05, "*"=0.10),
    depvar = F,
    fitstat = c("n", "r2"),
    keep = "Ward"
  )
  
  ## Not causal sample
  col1 = feols(as.formula(paste0(outcome.variable, model.noint)),
               analysis.dist[analysis.dist$causal == 1,])
  col2 = feols(as.formula(paste0(outcome.variable, model.basic)),
               analysis.dist[analysis.dist$causal == 1,])
  col3 = feols(as.formula(paste0(outcome.variable, model.basic)),
               analysis.dist[analysis.dist$causal == 1 & analysis.dist$CEN.HighSeg == "Yes",])
  col4 = feols(as.formula(paste0(outcome.variable, model.basic)),
               analysis.dist[analysis.dist$causal == 1 & 
                               analysis.dist$CEN.HighSeg == "No",])
  
  # Output for not causal
  tab2 = etable(
    col1, col2, col3, col4,
    dict = dict.vec,
    se.below = T,
    signif.code = c("***"=0.01, "**"=0.05, "*"=0.10),
    depvar = F,
    fitstat = c("n", "r2"),
    keep = "Ward"
  )
  
  return(list(tab1, tab2))
  
  
}

### SFP Outcomes ###
dist.analysis("Dist.SFP.Binary")
dist.analysis("Dist.SFP.Scaled")

### Teacher Outcomes ###
dist.analysis("G.RT.FT")
dist.analysis("G.RT.H.FT")
dist.analysis("G.RT.H.FT.H")
dist.analysis("G.RT.EL.FT")
dist.analysis("Dist.RT.EL.FT")
dist.analysis("G.RT.EL.FT.H")
dist.analysis("Dist.RT.EL.FT.H")

### Electoral Outcomes ###
analysis.dist <- 
  analysis.dist %>%
  dplyr::mutate(
    across(
      Sum.Win.Hisp.B:Sum.Ran.Hisp.E,
      ~ .x / Num.Elected
    )
  )

dist.analysis("Sum.Win.Hisp.C")
dist.analysis("Sum.Win.Hisp.B")
dist.analysis("Sum.Win.Hisp.E")
dist.analysis("Sum.Ran.Hisp.C")
dist.analysis("Sum.Ran.Hisp.B")
dist.analysis("Sum.Ran.Hisp.E")

# School-level Analyses --------------------------------------------------------


