# Script Target: Build analysis datasets at district and school level.

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
df.cdd.d <- fread("../../data/output/analysis/cdd_analysis_district.csv")
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

# SFP & Teach
analysis.dist <- 
  left_join(
    df.sfp.d,
    df.tch.d,
    by = c("NCESDist", "SY")
  )

# Census
analysis.dist <- 
  left_join(
    analysis.dist,
    df.cen.d,
    by = "NCESDist"
  )

# Electoral
analysis.dist <- 
  left_join(
    analysis.dist,
    df.ele,
    by = c("NCESDist", "SY")
  )

# Magazinnik
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

# CDD
analysis.dist <-
  left_join(
    analysis.dist,
    df.cdd.d,
    by =c("NCESDist", "SY")
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
    CEN.HighSeg.24 = as.factor(
      ifelse(
        Dissim > 0.3754,
        "Yes",
        "No"
      )
    ),
    CEN.HighSeg.All = as.factor(
      ifelse(
        Dissim > median(Dissim, na.rm = T),
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
hisp.dist <- 
  summarise(
    analysis.dist,
    .by = "NCESDist",
    Hisp = first(Prop.Hisp)
  ) %>%
  dplyr::mutate(
    CEN.HighHisp.24 = as.factor(
      ifelse(
        Hisp > 0.3726,
        "Yes",
        "No"
      )
    ),
    CEN.HighHisp.All = as.factor(
      ifelse(
        Hisp > median(Hisp, na.rm = T),
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
    hisp.dist,
    by = "NCESDist"
  )

### Check group inclusions by treatment status
analysis.dist$switch.keep = 
  analysis.dist$switch.type * analysis.dist$switcher

groups.arm <- 
  analysis.dist %>%
  dplyr::filter(SY == "2005-06") %>%
  summarise(
    .by = "switch.keep",
    CEN.HighHisp.24 = mean(ifelse(CEN.HighHisp.24 == "Yes", 1, 0), na.rm = T),
    CEN.HighHisp.All = mean(ifelse(CEN.HighHisp.All == "Yes", 1, 0), na.rm = T),
    CEN.HighSeg.24 = mean(ifelse(CEN.HighSeg.24 == "Yes", 1, 0), na.rm = T),
    CEN.HighSeg.All = mean(ifelse(CEN.HighSeg.All == "Yes", 1, 0), na.rm = T),
    N = n()
  ) %>%
  dplyr::filter(!(is.na(switch.keep)))

groups.arm
write.csv(groups.arm, "../../data/output/tables/seg_hisp_splits.csv")

## Last up: weights
sy.total <- 
  summarise(
    analysis.dist,
    .by = "SY",
    weight = sum(Dist.Total, na.rm = T)
  )

analysis.dist <-
  left_join(
    analysis.dist, 
    sy.total,
    by = "SY"
  ) %>%
  dplyr::mutate(
    weight = Dist.Total / weight
  )

# District Level: Electoral Outcomes Correct -----------------------------------
analysis.dist <- 
  analysis.dist %>%
  dplyr::mutate(
    Prop.Win.Hisp.B = Sum.Win.Hisp.B / Num.Elected,
    Prop.Win.Hisp.C = Sum.Win.Hisp.C / Num.Elected,
    Prop.Win.Hisp.E = Sum.Win.Hisp.E / Num.Elected,
    Prop.Ran.Hisp.B = Sum.Ran.Hisp.B / Num.Candidates,
    Prop.Ran.Hisp.C = Sum.Ran.Hisp.C / Num.Candidates,
    Prop.Ran.Hisp.E = Sum.Ran.Hisp.E / Num.Candidates,
  )


# District Level: Write --------------------------------------------------------
write.csv(
  analysis.dist,
  "../../data/output/analysis/district_regressions.csv"
)

# School-level Census Controls -------------------------------------------------

# What I can add to mag: median income, proportion of latinos with high school, 
# less than, and some college or more. Recall that there is no need for 
# such controls at the district level. Unemployment rate. 


# First: median income category (as a factor variable)
# Function for median of factor across columns
med.cat = function(x){
  df = as.data.frame(df.cen[x,HF5001:HF5016])
  
  output = c()
  
  for(i in 1:16){
    n = rep(colnames(df)[i], df[1,i])
    output = c(output, n)
  }
  
  output <- as.numeric(substr(output, 5, 6))
  output <- median(output)
  output = colnames(df.cen[x,HF5001:HF5016])[output]
  return(output)
}

# Apply
df.cen$CEN.Med.Inc <- 
  lapply(
    1:nrow(df.cen),
    med.cat
  )

# Write as factor
df.cen <- 
  unnest(df.cen, cols = "CEN.Med.Inc") %>%
  dplyr::mutate(CEN.Med.Inc = as.factor(CEN.Med.Inc))

# Second: education proportions
df.cen <- 
  df.cen %>%
  rowwise() %>%
  dplyr::mutate(
    Tot.Hisp25 = sum(c_across(HK7001:HK7014)),
    Prop.LessHigh = (sum(c_across(HK7001:HK7002)) + sum(c_across(HK7008:HK7009)))/Tot.Hisp25,
    Prop.High = (HK7003 + HK7010)/Tot.Hisp25,
    Prop.College = (sum(c_across(HK7004:HK7007)) + sum(c_across(HK7011:HK7014)))/Tot.Hisp25
  )

df.cen <- 
  df.cen %>%
  dplyr::mutate(
    Prop.LessHigh = ifelse(Tot.Hisp25 == 0, NA, Prop.LessHigh),
    Prop.High = ifelse(Tot.Hisp25 == 0, NA, Prop.High),
    Prop.College = ifelse(Tot.Hisp25 == 0, NA, Prop.College)
  )

# Third: unemployment rate
df.cen <- 
  df.cen %>%
  rowwise() %>%
  dplyr::mutate(
    LF.Total = sum(c_across(HEZ001:HEZ004)),
    CEN.Unemp = (HEZ002 + HEZ004) /LF.Total
  )
df.cen[,c("HEZ001", "HEZ002", "HEZ003", "HEZ004", "CEN.Unemp")]


# School-level Dataset ---------------------------------------------------------

# Checklist
# [X] SFP
# [X] Teacher
# [X] Census
# [X] District Census
# [X] Electoral
# [X] CDD
# [X] Magazinnik 

csv.fix <- function(df){
  
  df = df[,-"V1"]
  df = df[!(duplicated(df))]
  return(df)
}

df.cdd = csv.fix(df.cdd)
df.sfp = csv.fix(df.sfp)
df.tch = csv.fix(df.tch)
df.ele = csv.fix(df.ele)

analysis.school <- 
  left_join(
    df.cdd,
    df.sfp[,-"Stu.Count.Reported"],
    by = c("NCESSchool", "SY")
  )

analysis.school <- 
  left_join(
    analysis.school[,-"FTE"],
    df.tch[,-c("NCESDist", "Stu.Count.Reported")],
    by = c("NCESSchool", "SY")
  )

df.cen <- 
  df.cen %>%
  dplyr::mutate(
    NCESSchool = bit64::as.integer64(
      paste0(as.character(NCESDist), NCESSchool)
    )
  )

df.cen.join <- 
  df.cen %>%
  dplyr::select(
    NCESSchool, CEN.Med.Inc, Prop.LessHigh, Prop.High, Prop.College, CEN.Unemp
  )

df.cen.join <- df.cen.join[!(duplicated(df.cen.join$NCESSchool)),]

analysis.school <- 
  left_join(
    analysis.school,
    df.cen.join,
    by = "NCESSchool"
  )

df.cen.d <- 
  left_join(
    df.cen.d,
    left_join(
      seg.dist,
      hisp.dist,
      by = "NCESDist"
    ),
    by = "NCESDist"
  )

analysis.school <-
  left_join(
    analysis.school,
    df.cen.d,
    by = "NCESDist"
  )

analysis.school <- 
  left_join(
    analysis.school,
    df.ele,
    by = c("NCESDist", "SY")
  )

analysis.school <- 
  left_join(
    analysis.school,
    df.mag[,-c("CDSCode", "year")],
    by = c("NCESDist", "SY")
  )

### Weights
sy.total <- 
  summarise(
    analysis.school,
    .by = "SY",
    weight = sum(Stu.Count.Reported, na.rm = T)
  )

analysis.school <-
  left_join(
    analysis.school, 
    sy.total,
    by = "SY"
  ) %>%
  dplyr::mutate(
    weight = Stu.Count.Reported / weight
  )

### Inclusion indicators ###
analysis.school$include <- 
  ifelse(
    analysis.school$causal == 1 &
      (analysis.school$switcher == 0 | 
         (analysis.school$switcher == 1 & analysis.school$switch.type %in% c(1, 2))
      ),
    1,
    0
  )

analysis.school$include2 <- 
  ifelse(
    analysis.school$causal == 1,
    1,
    0
  )

# School-level Write ---------------------------------------------------------
write.csv(
  analysis.school,
  "../../data/output/analysis/school_regressions.csv"
)
