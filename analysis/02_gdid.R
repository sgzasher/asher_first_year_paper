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
library(modelsummary)
library(kableExtra)
options(modelsummary_format_numeric_latex = "plain")

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

analysis.dist <- function(var.outcome, var.split, var.hte, include.group = 1, file.arg, caption.arg, label.arg){
  

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
  col4 = feols(as.formula(model.basic), 
               reg.data[reg.data[[var.split]] == "Yes",],
               weights = reg.data[reg.data[[var.split]] == "Yes",]$weight)
  col3 = feols(as.formula(model.basic), 
               reg.data[reg.data[[var.split]] == "No",],
               weights = reg.data[reg.data[[var.split]] == "No",]$weight)
    
  # Table
  c.map = c("trustee" = "Ward", "trustee:Prop.Hisp" = "Ward:Prop. Hisp")
  tab =  modelsummary(list(col1, col2, col3, col4),
                      coef_map = c.map,
                      output = "latex",
                      estimate = "{estimate}{stars}",
                      gof_map = c("nobs", "adj.r.squared"),
                      caption = paste0("\\label{", label.arg, "}", caption.arg)) %>%
    footnote("\\\\textit{Note:} Columns 1 and 2 show esimtates in the entire sample. Columns 3 shows estimates in districts with above treatment unit median dissimilarity index. Column 4 shows estimates in districts with below median. Standard errors in parentheses. All specifications include district and year fixed effects and stanard errors are clustered at the district level.  + p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001",
             threeparttable = T,
             footnote_as_chunk = T,
             general_title = "",
             escape = F)
  
  tab <- tab %>% 
    add_header_above(
      c(
        " " = 3,
        "Low" = 1,
        "High" = 1
      )) %>%
    add_header_above(
      c(
        " " = 3,
        "Segregation" = 2
      )
    ) %>%
    kable_styling(latex_options = "HOLD_position") %>%
    kableExtra::save_kable(file = file.arg)
}


# Analysis Function: School ----------------------------------------------------
# Number from RIPW script: 0.3591 median prop hisp among treated units
# w/ nontrivial pscore

analysis.sch <- function(var.outcome, var.hte, file.arg, caption.arg, label.arg){
  
  # Relevant data
  reg.data = df.school[df.school$include == 1,]
  reg.data.a = reg.data[
    reg.data$CEN.HighSeg.24  == "Yes" & reg.data$Prop.Hisp > 0.3591,]
  reg.data.b = reg.data[
    reg.data$CEN.HighSeg.24  == "No" & reg.data$Prop.Hisp <= 0.3591,]
  
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
  col4 = feols(as.formula(model.noint), reg.data.a, weights = reg.data.a$weight)
  col5 = feols(as.formula(model.basic), reg.data.a, weights = reg.data.a$weight)
  col6 = feols(as.formula(model.controls), reg.data.a, weights = reg.data.a$weight)
  col1 = feols(as.formula(model.noint), reg.data.b, weights = reg.data.b$weight)
  col2 = feols(as.formula(model.basic), reg.data.b, weights = reg.data.b$weight)
  col3 = feols(as.formula(model.controls), reg.data.b, weights = reg.data.b$weight)
  
  # Table
  controls = as.data.frame(t(c("Controls", "No", "No", "Yes", "No", "No", "Yes")))
  attr(controls, 'position') <- 7
  tab = modelsummary(list(col1, col2, col3, col4, col5, col6),
                     coef_omit = c(-1, -2, -3),
                     output = "latex",
                     estimate = "{estimate}{stars}",
                     coef_rename = c("trustee" = "Ward", 
                                     "Stu.Prop.Hisp" = "Prop. Hisp"),
                     gof_map = c("nobs", "adj.r.squared"),
                     add_rows = controls,
                     caption = paste0("\\label{", label.arg, "}", caption.arg)) %>%
    footnote(general = c("\\\\textit{Note:} Columns 1 to 3 show results for schools in districts with below the treatment group median dissimilarity index and (adult) proportion Hispanic. Columns 4 to 6 show results for schools in districts with above median for each. Standard deviations in parentheses. All specificatins include district and year fixed effects and standard errors are clustered at the district level. + p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001"),
             threeparttable = T,
             footnote_as_chunk = T,
             general_title = "",
             escape = F)
    
  tab <- tab %>% 
    add_header_above(
      c(
      " " = 1,
      "Low Hisp & Seg" = 3,
      "High Hisp & Seg" = 3
    )) %>%
    kable_styling(latex_options = "HOLD_position") %>%
    kableExtra::save_kable(file = file.arg)
}


# Analysis: Electoral ----------------------------------------------------------

# Approximate replication of Magazinnik speciifcaiton
analysis.dist("Prop.Win.Hisp.E",
              "CEN.HighSeg.24",
              "Prop.Hisp",
              1,
              "../../output/tables/mag_election_spec.tex", 
              "Magazinnik Result Replicattion",
              "t::mag_spec")

# My preferred version
analysis.dist("Prop.Win.Hisp.C",
              "CEN.HighSeg.24",
              "Prop.Hisp",
              1,
              "../../output/tables/dist_elec_win_c.tex",
              "Effect of Ward Elections on Identity of Winning Candidate",
              "t::d_win_c")

# Other electoral specificaion
analysis.dist("Prop.Ran.Hisp.C",
              "CEN.HighSeg.24",
              "Prop.Hisp",
              1,
              "../../output/tables/dist_elec_ran_c.tex",
              "Effect of Ward Election on Identity of Candidate Pool",
              "t::d_ran_c")

### LEAVE REMAINING VERSIONS TO ROBUSTNESS CHECKS

# Analysis: SFP ----------------------------------------------------------------

# Binary
analysis.dist("Dist.SFP.Binary",
              "CEN.HighSeg.24",
              "Prop.Hisp",
              1,
              "../../output/tables/dist_sfp_binary.tex",
              "District Level Binary SFP Indicator",
              "t::dist.sfp.bin")

analysis.sch("SFP.Binary",
             "Stu.Prop.Hisp",
             "../../output/tables/sch_sfp_binary.tex",
             "School Level Binary SFP Indicator",
             "t::sch.sfp.bin")

# Scaled
analysis.dist("Dist.SFP.Scaled",
              "CEN.HighSeg.24",
              "Prop.Hisp",
              1,
              "../../output/tables/dist_sfp_scaled.tex",
              "District Level SFP Spending Per Student",
              "t::dist.sfp.sca")

analysis.sch("Tot.SFP.Scaled",
             "Stu.Prop.Hisp",
             "../../output/tables/sch_sfp_scaled.tex",
             "School Level SFP Spending Per Student",
             "t::sch.sfp.sca")


# Analysis: Teachers -----------------------------------------------------------

# Total Teachers
analysis.dist("Dist.RT.FT",
              "CEN.HighSeg.24",
              "Prop.Hisp",
              1,
              "../../output/tables/dist_rt_ft.tex",
              "District Level Teacher Student Ratio",
              "t::dist.rt.ft")

analysis.dist("G.RT.FT",
              "CEN.HighSeg.24",
              "Prop.Hisp",
              1,
              "../../output/tables/dist_g_rt_ft.tex",
              "District Level Gini Coef: Teacher Student Ratio",
              "t::dist.g.rt.ft")

analysis.sch("RT.FT",
             "Stu.Prop.Hisp",
             "../../output/tables/sch_rt_ft.tex",
             "School Level Teacher Student Ratio",
             "t::sch.rt.ft")

# Hispanic Teachers
analysis.dist("Dist.RT.H.FT",
              "CEN.HighSeg.24",
              "Prop.Hisp",
              1,
              "../../output/tables/dist_rt_h_ft.tex",
              "District Level Hispanic Teacher Student Ratio",
              "t::dist.rt.h")

analysis.dist("G.RT.H.FT",
              "CEN.HighSeg.24",
              "Prop.Hisp",
              1,
              "../../output/tables/dist_g_rt_h_ft.tex",
              "District Level Gini Coef: Hispanic Teacher Student Ratio",
              "t::dist.g.rt.h")

analysis.sch("RT.H.FT",
             "Stu.Prop.Hisp",
             "../../output/tables/sch_rt_h_ft.tex",
             "School Level Hispanic Teacher Student Ratio",
             "t::sch.rt.h")

# English Language Teachers
analysis.dist("Dist.RT.EL.FT",
              "CEN.HighSeg.24",
              "Prop.Hisp",
              1,
              "../../output/tables/dist_rt_el_ft.tex",
              "District Level ELD-Certified Teacher Student Ratio",
              "t::dist.rt.el")

analysis.dist("G.RT.EL.FT",
              "CEN.HighSeg.24",
              "Prop.Hisp",
              1,
              "../../output/tables/dist_g_rt_el_ft.tex",
              "District Level Gini Coef: ELD-Certified Teacher Student Ratio",
              "t::dist.g.rt.el")

analysis.sch("RT.EL.FT",
             "Stu.Prop.Hisp",
             "../../output/tables/sch_rt_el_ft.tex",
             "School Level ELD-Certified Teacher Student Ratio",
             "t::sch.rt.el")

# Teacher Qualifications
analysis.sch("PC.MasterPlus",
             "Stu.Prop.Hisp",
             "../../output/tables/sch_pc_mast.tex",
             "School Level Percent of Teacher With Masters Degree or Higher",
             "t::sch.masters")
