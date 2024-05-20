# RIPW pre-trends plot for presentation 
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
set.seed(1966)
library(dplyr)
library(data.table)
library(stringr)
library(DescTools)
library(fixest)
library(stargazer)
library(randomForestSRC)
library(parallel)
library(survival)
library(zoo)
library(wCorr)
library(tidyr)
library(ggplot2)
library(ggtext)
library(binsreg)

# Read data --------------------------------------------------------------------
ana.dist = fread("../../data/output/analysis/district_regressions.csv")
psc.dist = fread("../../data/output/analysis/marginal_pscores.R")
ana.sch = fread("../../data/output/analysis/school_regressions.csv")

# Stitch -----------------------------------------------------------------------
analysis.dist <- 
  left_join(ana.dist, psc.dist, by = "NCESDist") %>%
  dplyr::filter(!(is.na(pscore)))

analysis.dist <- analysis.dist[analysis.dist[["include"]]==1,]

analysis.school <- 
  left_join(ana.sch, psc.dist, by = "NCESDist") %>%
  dplyr::filter(!(is.na(pscore)))

analysis.school <- analysis.school[analysis.school[["include"]]==1,]

# District Controls ------------------------------------------------------------
df.cen = fread("../../data/output/analysis/census_district.csv")

df.cen <- 
  df.cen %>%
  dplyr::rename(
    "CEN.Pop" = "FXS001",
    "CEN.Pop.Hisp" = "FXZ001",
    "CEN.Pop.65" = "F15001",
    "CEN.Pop.65.Hisp" = "F2L001",
    "CEN.Hisp.OwnerOcc" = "FW7001",
    "CEN.Hisp.RenterOcc" = "FW7002",
    "CEN.PC.Inc" = "HG4001"
  ) %>%
  dplyr::select(
    NCESDist, CEN.Pop, CEN.Pop.Hisp, CEN.Pop.65,
    CEN.Pop.65.Hisp, CEN.Hisp.OwnerOcc, CEN.Hisp.RenterOcc,
    CEN.PC.Inc
  ) %>%
  dplyr::mutate(
    CEN.Prop.Hisp.Own = CEN.Hisp.OwnerOcc / 
      (CEN.Hisp.OwnerOcc + CEN.Hisp.RenterOcc),
    CEN.Share.Hisp = CEN.Pop.Hisp / CEN.Pop
  )

analysis.dist <- 
  left_join(
    analysis.dist,
    df.cen
  )

# K fold assignment ------------------------------------------------------------

# Have to ensure treated units have assignment
analysis.dist.t = analysis.dist[analysis.dist[["switcher"]]==1,]
analysis.dist.c = analysis.dist[analysis.dist[["switcher"]]==0,]

# Fold assignments
nfolds <- 10

foldid.t = as.data.frame(unique(analysis.dist.t$NCESDist))
colnames(foldid.t) = "NCESDist"
foldid.t$folda = sample(seq(nrow(foldid.t)) %% nfolds + 1)

foldid.c = as.data.frame(unique(analysis.dist.c$NCESDist))
colnames(foldid.c) = "NCESDist"
foldid.c$foldb = sample(seq(nrow(foldid.c)) %% nfolds + 1)

# Assignments to data: District
analysis.dist = left_join(analysis.dist, foldid.t, by = "NCESDist")
analysis.dist = left_join(analysis.dist, foldid.c, by = "NCESDist")
analysis.dist$fold = ifelse(is.na(analysis.dist$folda), 
                            analysis.dist$foldb, 
                            analysis.dist$folda)
analysis.dist <- analysis.dist[,-c("folda", "foldb")]

# Assignments to data: School
analysis.school = left_join(analysis.school, foldid.t, by = "NCESDist")
analysis.school = left_join(analysis.school, foldid.c, by = "NCESDist")
analysis.school$fold = ifelse(is.na(analysis.school$folda),
                              analysis.school$foldb,
                              analysis.school$folda)
analysis.school <- analysis.school[,-c("folda", "foldb")]

# Crump Trimming ---------------------------------------------------------------

# Honestly this is really really light on overlap
# Just get rid of one literal zero treatment prob units
quantile(analysis.dist$pscore, c(0.01, 0.05, 0.15, 0.2))
analysis.dist.exclude = analysis.dist[analysis.dist$pscore <= 0.025]
analysis.dist = analysis.dist[analysis.dist$pscore > 0.025]
analysis.school = analysis.school[analysis.school$pscore > 0.025]

# Contruct RIP weights ---------------------------------------------------------

# Dist Level
max.year <- max(analysis.dist$treat.time)
Pi <- rep(0, max.year)
Pi[2:max.year-1] <- 1 / 2 / max.year
Pi[c(1, max.year)] <- (max.year + 1) / 4 / max.year
rip = Pi[analysis.dist$treat.time] / analysis.dist$pscore

analysis.dist$rip = rip

# School level
max.year <- max(analysis.school$treat.time)
Pi <- rep(0, max.year)
Pi[2:max.year-1] <- 1 / 2 / max.year
Pi[c(1, max.year)] <- (max.year + 1) / 4 / max.year
rip = Pi[analysis.school$treat.time] / analysis.school$pscore

analysis.school$rip = rip

# HTE Analysis: District -------------------------------------------------------

x = summary(analysis.dist[analysis.dist$switcher==1,]$Prop.Hisp)
y = summary(analysis.dist[analysis.dist$switcher==1,]$CEN.Dissim)

## Initial Version
# Bottom Left
cond.q1 = (analysis.dist$include == 1 &
             ((analysis.dist$switcher == 1 & 
                 (analysis.dist$Prop.Hisp >= x[[1]] & analysis.dist$Prop.Hisp < x[[3]])) & 
                (analysis.dist$CEN.Dissim >= y[[1]] & analysis.dist$CEN.Dissim < y[[3]])) | 
             (analysis.dist$switcher == 0))

# Top left
cond.q2 = (analysis.dist$include == 1 &
             ((analysis.dist$switcher == 1 & 
                 (analysis.dist$Prop.Hisp >= x[[3]] & analysis.dist$Prop.Hisp < x[[6]])) & 
                (analysis.dist$CEN.Dissim >= y[[1]] & analysis.dist$CEN.Dissim < y[[3]])) | 
             (analysis.dist$switcher == 0))

# Top right
cond.q3 = (analysis.dist$include == 1 &
             (analysis.dist$switcher == 1 & 
                (analysis.dist$Prop.Hisp >= x[[3]] & analysis.dist$Prop.Hisp < x[[6]]) & 
                (analysis.dist$CEN.Dissim >= y[[3]] & analysis.dist$CEN.Dissim < y[[6]])) | 
             (analysis.dist$switcher == 0))

# Bottom right
cond.q4 = (analysis.dist$include == 1 &
             ((analysis.dist$switcher == 1 & 
                 (analysis.dist$Prop.Hisp >= x[[1]] & analysis.dist$Prop.Hisp < x[[3]])) & 
                (analysis.dist$CEN.Dissim >= y[[3]] & analysis.dist$CEN.Dissim < y[[6]])) | 
             (analysis.dist$switcher == 0))

control.vector = c("CEN.Pop", "CEN.Share.Hisp", "CEN.Pop.65.Hisp",
                   "CEN.PC.Inc", "CEN.Prop.Hisp.Own", "Rev.Local.Property",
                   "Dist.Total")

# Lags and leads of treatment  -------------------------------------------------
analysis.dist <-
  analysis.dist %>%
  group_by(NCESDist) %>%
  dplyr::mutate(
    event.year = ifelse(
      str_length(as.character(treat.time)) == 1,
      as.numeric(
        paste0(
          "200",
          as.character(treat.time)
        )
      ),
      as.numeric(
        paste0(
          "20",
          as.character(treat.time)
        )
      )
    )
    ) %>% 
  dplyr::mutate(
    year = as.numeric(str_sub(SY, 1, 4)),
    treat.l1 = ifelse(year == event.year - 1 & switcher == 1, 1, 0),
    treat.l2 = ifelse(year == event.year - 2 & switcher == 1, 1, 0),
    treat.l3 = ifelse(year == event.year - 3 & switcher == 1, 1, 0),
    treat.l4 = ifelse(year == event.year - 4 & switcher == 1, 1, 0),
    treat.l5 = ifelse(year == event.year - 5 & switcher == 1, 1, 0),
    treat.l6 = ifelse(year == event.year - 6 & switcher == 1, 1, 0),
    treat.l7 = ifelse(year == event.year - 7 & switcher == 1, 1, 0),
    treat.lmax = ifelse(year <= event.year - 8 & switcher == 1, 1, 0),
    treat.p1 = ifelse(year == event.year + 1 & switcher == 1, 1, 0),
    treat.p2 = ifelse(year == event.year + 2 & switcher == 1, 1, 0),
    treat.p3 = ifelse(year == event.year + 3 & switcher == 1, 1, 0),
    treat.pmax = ifelse(year >= event.year + 4 & switcher == 1, 1, 0),
  ) %>%
  ungroup()


# Linear model event study instead...
var.outcome = "Prop.Ran.Hisp.C"
choice.title = "Proportion Ran Hispanic"

event.stud.dist = function(var.outcome, choice.title){
  model.basic = paste0(
    var.outcome,
    " ~ (trustee) + treat.l2 + treat.l3 + treat.l4 + treat.l5",
    " + treat.l6 + treat.l7 + treat.lmax",
    " + treat.p1 + treat.p2 + treat.p3 + treat.pmax",
    "| NCESDist + SY"
  )
  
  analysis.dist$weightme = analysis.dist$rip / analysis.dist$pscore
  
  est = 
    summary(
      feols(as.formula(model.basic), 
            analysis.dist,
            weights = analysis.dist$weightme
      ))
  
  
  # Turn into a plot
  plot = as.data.frame(est$coeftable)
  plot = tibble::rownames_to_column(plot, "var")
  plot = rbind(plot, c("T-1", NA, NA, NA, NA))
  plot$var = c("T0", "T-2", "T-3", "T-4", "T-5", "T-6", "T-7", "T-8",
               "T+1", "T+2", "T+3", "T+4", "T-1")
  plot = plot[c(8, 7, 6,5,4,3,2,13,1,9,10,11,12),]
  plot$upper = as.numeric(plot$Estimate) + 1.96 * as.numeric(plot$`Std. Error`)
  plot$lower = as.numeric(plot$Estimate) - 1.96 * as.numeric(plot$`Std. Error`)
  
  plot$var <- as.character(plot$var)
  plot$var = factor(plot$var, levels = unique(plot$var))
  
  plot$est = as.numeric(plot$Estimate)
  plot$lower = as.numeric(plot$lower)
  plot$upper = as.numeric(plot$upper)
  
  plot$endpoint = c("Yes", "No", "No", "No", "No", "No", "No", "No", 
                    "No", "No", "No", "No", "Yes")
  
  
  p = ggplot(plot, aes(var, est, col = endpoint)) +
    geom_errorbar(aes(ymin = lower,
                      ymax = upper),
                  position = position_dodge(0.2), 
                  width = 0.1)+ 
    theme_classic() +
    geom_point(aes(col = endpoint), position = position_dodge(0.2)) + 
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey") + 
    geom_vline(xintercept = 8, linetype = "dashed", color = "grey") +
    scale_color_manual(name = "Endpoint", 
                       values = c("black", "#B92303")) + 
    labs(col = "Estimation",
         title = paste0(choice.title, " Pre-Trends Evidence"),
         subtitle = "TWFE Event Study Specification w/ RIPW Unit Weights, District-Level",
         y = "Estimate",
         x = "")
  
  return(p)
}

pdf("../../output/evidence_pretrends/dist_win_hisp.pdf", width = 8.3, height = 4.9)
event.stud.dist("Prop.Win.Hisp.C", "Hispanic Proportion Winners")
dev.off()

pdf("../../output/evidence_pretrends/dist_rt_ft.pdf", width = 8.3, height = 4.9)
event.stud.dist("Dist.RT.FT", "Student-Teacher Ratio")
dev.off()

pdf("../../output/evidence_pretrends/dist_g_rt_ft.pdf", width = 8.3, height = 4.9)
event.stud.dist("G.RT.FT", "Gini Coefficient: Student-Teacher Ratio")
dev.off()

# School Level HTE Prep --------------------------------------------------------

# Get student hispanicity quartiles by district
analysis.school <- 
  analysis.school %>%
  group_by(NCESDist) %>%
  mutate(Hisp.Quartile = ntile(Stu.Prop.Hisp, 3)) %>%
  ungroup()

## HH Group
hh = unique(
  analysis.dist[cond.q3,]$NCESDist
)

## LL Group
ll = unique(
  analysis.dist[cond.q1,]$NCESDist
)

# Apply variables
analysis.school <- 
  analysis.school %>%
  mutate(
    HH = ifelse(
      NCESDist %in% hh, "Yes", "No"
    ),
    LL = ifelse(
      NCESDist %in% ll, "Yes", "No"
    )
  )

# Make sure subsetting will work
analysis.school = analysis.school[!is.na(analysis.school$HH == "Yes" & analysis.school$Hisp.Quartile == 1),]

# Controls 
control.vector = c("Stu.Count.Reported", "Rev.Local.Property.Scaled",
                   "Exp.Instruction.Scaled", "Exp.Total.Scaled", "Rev.Total.Scaled",
                   "Title.I", "Stu.Prop.FRSM", "Stu.Prop.White" , "Stu.Prop.Black",
                   "Stu.Prop.AAPI", "CEN.Unemp","Prop.LessHigh","Prop.High")

# School Level Event Study -----------------------------------------------------
lags.leads <-
  analysis.dist %>%
  dplyr::select(
    NCESDist,
    SY,
    treat.l1,
    treat.l2,
    treat.l3,
    treat.l4,
    treat.l5,
    treat.l6,
    treat.l7,
    treat.lmax,
    treat.p1,
    treat.p2,
    treat.p3,
    treat.pmax
  )

analysis.school <-
  left_join(
    analysis.school, 
    lags.leads,
    by = c("NCESDist", "SY")
  )

event.stud.sch = function(var.outcome, choice.title){
  model.basic = paste0(
    var.outcome,
    " ~ (trustee) + treat.l2 + treat.l3 + treat.l4 + treat.l5",
    " + treat.l6 + treat.l7 + treat.lmax",
    " + treat.p1 + treat.p2 + treat.p3 + treat.pmax",
    "| NCESDist + SY"
  )
  
  analysis.school$weightme = analysis.school$rip / analysis.school$pscore
  
  est = 
    summary(
      feols(as.formula(model.basic), 
            analysis.school,
            weights = analysis.school$weightme
      ))
  
  
  # Turn into a plot
  plot = as.data.frame(est$coeftable)
  plot = tibble::rownames_to_column(plot, "var")
  plot = rbind(plot, c("T-1", NA, NA, NA, NA))
  plot$var = c("T0", "T-2", "T-3", "T-4", "T-5", "T-6", "T-7", "T-8",
               "T+1", "T+2", "T+3", "T+4", "T-1")
  plot = plot[c(8, 7, 6,5,4,3,2,13,1,9,10,11,12),]
  plot$upper = as.numeric(plot$Estimate) + 1.96 * as.numeric(plot$`Std. Error`)
  plot$lower = as.numeric(plot$Estimate) - 1.96 * as.numeric(plot$`Std. Error`)
  
  plot$var <- as.character(plot$var)
  plot$var = factor(plot$var, levels = unique(plot$var))
  
  plot$est = as.numeric(plot$Estimate)
  plot$lower = as.numeric(plot$lower)
  plot$upper = as.numeric(plot$upper)
  
  plot$endpoint = c("Yes", "No", "No", "No", "No", "No", "No", "No", 
                    "No", "No", "No", "No", "Yes")
  
  
  p = ggplot(plot, aes(var, est, col = endpoint)) +
    geom_errorbar(aes(ymin = lower,
                      ymax = upper),
                  position = position_dodge(0.2), 
                  width = 0.1)+ 
    theme_classic() +
    geom_point(aes(col = endpoint), position = position_dodge(0.2)) + 
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey") + 
    geom_vline(xintercept = 8, linetype = "dashed", color = "grey") +
    scale_color_manual(name = "Endpoint", 
                       values = c("black", "#B92303")) + 
    labs(col = "Estimation",
         title = paste0(choice.title, " Pre-Trends Evidence"),
         subtitle = "TWFE Event Study Specification w/ RIPW Unit Weights, School-Level",
         y = "Estimate",
         x = "")
  
  return(p)
}

pdf("../../output/evidence_pretrends/sch_rt_ft.pdf", width = 8.3, height = 4.9)
event.stud.sch("RT.FT", "Student-Teacher Ratio")
dev.off()
