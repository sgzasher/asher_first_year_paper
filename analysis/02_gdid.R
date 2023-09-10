# Script Target: GDID Regressions 

# Packages ---------------------------------------------------------------------
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
set.seed(1966)
library(dplyr)
library(data.table)
library(stringr)
library(DescTools)
library(fixest)
library(stargazer)

# Data -------------------------------------------------------------------------

# write.csv struggles with long files
csv.fix <- function(df){
  df = df[,-"V1"]
  df = df[!(duplicated(df))]
  return(df)
}

# Read and fix
df.school = fread("../../data/output/analysis/school_regressions.csv")
df.dist = fread("../../data/output/analysis/district_regressions.csv")
df.school = csv.fix(df.school)
df.dist = csv.fix(df.dist)

# Analysis Function: District --------------------------------------------------

analysis.dist <- function(var.outcome, var.split, var.hte, include.group = 1){
  

  # Subsample for analysis
  if(include.group == 1){
    reg.data = df.dist[df.dist$include == 1,]
  }
  if(include.group == 2){
    reg.data = df.dist[df.dist$include2 == 1,]
  }
  if(include.group == 3){
    reg.data = df.dist
  }
  
  # Formulae 
  model.basic = paste0(
    var.outcome,
    " ~ (trustee *",
    var.hte,
    ") | NCESDist + SY"
    )
  model.noint = paste0(
    var.outcome,
    " ~ (trustee) | NCESDist + SY"
  )

  # Columns
  col1 = feols(as.formula(model.noint), reg.data, weights = reg.data$weight)
  col2 = feols(as.formula(model.basic), reg.data, weights = reg.data$weight)
  col3 = feols(as.formula(model.basic), 
               reg.data[reg.data[[var.split]] == "Yes",],
               weights = reg.data[reg.data[[var.split]] == "Yes",]$weight)
  col4 = feols(as.formula(model.basic), 
               reg.data[reg.data[[var.split]] == "No",],
               weights = reg.data[reg.data[[var.split]] == "No",]$weight)
    
  # Table
  tab = etable(
    col1, col2, col3, col4,
    se.below = T,
    signif.code = c("***"=0.01, "**"=0.05, "*"=0.10),
    depvar = T,
    fitstat = c("n", "r2"),
    keep = "trustee"
  )
  
  return(tab)
  
}


# Analysis: Magazinnik District Splits Inappropriate ---------------------------
analysis.dist("Dist.SFP.Scaled",
              "CEN.HighSeg.All",
              "Prop.Hisp",
              1)

# Analysis: Magazinnik Replication & Alternative Dist Splitting ----------------
analysis.dist("Prop.Win.Hisp.E",
              "CEN.HighSeg.All",
              "Prop.Hisp",
              1)

analysis.dist("Prop.Win.Hisp.C",
              "CEN.HighSeg.24",
              "Prop.Hisp",
              1)

analysis.dist("Sum.Win.Hisp.C",
              "CEN.HighSeg.24",
              "CEN.HighHisp.24",
              1)

# Analysis: District -----------------------------------------------------------

### SFP Outcomes ###
analysis.dist("Dist.SFP.Binary",
              "CEN.HighSeg.24",
              "CEN.HighHisp.24",
              1)

analysis.dist("Dist.SFP.Binary",
              "CEN.HighSeg.24",
              "Prop.Hisp",
              1)

analysis.dist("Dist.SFP.Scaled",
              "CEN.HighSeg.24",
              "CEN.HighHisp.24",
              1)

analysis.dist("Dist.SFP.Scaled",
              "CEN.HighSeg.24",
              "Prop.Hisp",
              1)

### Teacher Outcomes ###
analysis.dist("G.RT.H.FT.H",
              "CEN.HighSeg.24",
              "Prop.Hisp",
              1)

analysis.dist("G.RT.EL.FT.H",
              "CEN.HighSeg.24",
              "Prop.Hisp",
              1)

analysis.dist("G.RT.FT",
              "CEN.HighSeg.24",
              "Prop.Hisp",
              1)

analysis.dist("Dist.RT.FT",
              "CEN.HighSeg.24",
              "Prop.Hisp",
              1)

### Electoral Outcomes ###
analysis.dist("Prop.Win.Hisp.C",
              "CEN.HighSeg.24",
              "Prop.Hisp",
              1)

analysis.dist("Prop.Ran.Hisp.C",
              "CEN.HighSeg.24",
              "Prop.Hisp",
              1)

# Analysis Function: School ----------------------------------------------------
analysis.sch <- function(var.outcome, var.split, var.hte, include.group = 1){
  
  # Subsample for analysis
  if(include.group == 1){
    reg.data = df.school[df.school$include == 1,]
  }
  if(include.group == 2){
    reg.data = df.school[df.school$include2 == 1,]
  }
  if(include.group == 3){
    reg.data = df.school
  }
  
  # Formulae 
  model.basic = paste0(
    var.outcome,
    " ~ (trustee * ",
    var.hte,
    ") | NCESDist + SY"
  )
  model.noint = paste0(
    var.outcome,
    " ~ (trustee) | NCESDist + SY"
  )
  model.controls = paste0(
    var.outcome,
    "~ (trustee *",
    var.hte,
    ") + Stu.Count.Reported + Rev.Local.Property.Scaled + ",
    "Exp.Instruction.Scaled + Exp.Total.Scaled + Rev.Total.Scaled + ",
    "Title.I + Stu.Prop.FRSM + Stu.Prop.White + Stu.Prop.Black",
    "+ Stu.Prop.AAPI + CEN.Unemp + Prop.LessHigh + Prop.High + Prop.College",
    " + CEN.Med.Inc | NCESDist + SY"
  )
  
  # Columns
  col1 = feols(as.formula(model.noint), reg.data, weights = reg.data$weight)
  col2 = feols(as.formula(model.basic), reg.data, weights = reg.data$weight)
  col3 = feols(as.formula(model.controls), reg.data, weights = reg.data$weight)
  col4 = feols(as.formula(model.controls), 
               reg.data[reg.data[[var.split]] == "Yes",],
               weights = reg.data[reg.data[[var.split]] == "Yes",]$weight)
  col5 = feols(as.formula(model.controls), 
               reg.data[reg.data[[var.split]] == "No",],
               weights = reg.data[reg.data[[var.split]] == "No",]$weight)
  
  # Table
  tab = etable(
    col1, col2, col3, col4, col5,
    se.below = T,
    signif.code = c("***"=0.01, "**"=0.05, "*"=0.10),
    depvar = T,
    fitstat = c("n", "r2"),
    group = list("Controls" = c(
      "Stu.Count.Reported", "Rev.Local.Property.Scaled",
      "Exp.Instruction.Scaled", "Exp.Total.Scaled", "Rev.Total.Scaled",
      "Title.I", "Stu.Prop.FRSM", "Stu.Prop.White", "Stu.Prop.Black",
      "Stu.Prop.AAPI", "CEN.Unemp", "Prop.LessHigh", "Prop.High", 
      "Prop.College", "CEN.Med.Inc")),
    headers = list(Segregation = list(" " = 3, "High" = 1, "Low" = 1))
  )
  
  return(tab)
  
}


# Analysis: School -------------------------------------------------------------


### SFP Outcomes ###
analysis.sch("SFP.Binary",
              "CEN.HighSeg.24",
              "Stu.Prop.Hisp",
              1)

analysis.sch("Tot.SFP.Scaled",
             "CEN.HighSeg.24",
             "Stu.Prop.Hisp",
             1)

analysis.sch("RT.H.FT",
             "CEN.HighSeg.24",
             "Stu.Prop.Hisp",
             1)

analysis.sch("RT.EL.FT",
             "CEN.HighSeg.24",
             "Stu.Prop.Hisp",
             1)

analysis.sch("RT.FT",
             "CEN.HighSeg.24",
             "Stu.Prop.Hisp",
             1)

analysis.sch("PC.MasterPlus",
             "CEN.HighSeg.24",
             "Stu.Prop.Hisp",
             1)


