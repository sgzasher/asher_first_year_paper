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

# RIPW Function ----------------------------------------------------------------

# Estimate outcome model in each fold, then do final analysis
# Each step: M() is estimate outcome model on just control units, control
# variables out of fold. i.e. m_0. Fits with FE! 
# Then we use the RIPW function written by Lihua's team for the est and CIs
# For HTE estimates, we do this redifining treatment indicators by the deciles...
# Estimate RIPW 
# Just want an RIPW function to start with. I'll just make the tables myself. 

# Inputs... data pre-subset... treatment variable name (for fitting control 
# model)... fold variable name... outcome variable name...
# foruma for fitting control model....

# Source: https://github.com/xiaomanluo/ripwPaper/blob/main/R/ripw.R
ripw <- function(Y, tr,
                 muhat = NULL,
                 Theta = rep(1, nrow(Y))){
  n <- nrow(Y)
  Ytd <- Y
  if (!is.null(muhat)){
    Ytd <- Ytd - muhat
  }
  Theta <- Theta / mean(Theta, na.rm = T)
  Ytd_c <- Ytd - rowMeans(Ytd, na.rm = T)
  tr_c <- tr - rowMeans(tr, na.rm = T)
  
  Gamma_w <- colMeans(Theta * tr_c, na.rm = T)
  Gamma_y <- colMeans(Theta * Ytd_c, na.rm = T)
  Gamma_ww <- mean(Theta * rowSums(tr_c^2, na.rm = T), na.rm = T)
  Gamma_wy <- mean(Theta * rowSums(tr_c * Ytd_c, na.rm = T), na.rm = T)
  denom <- Gamma_ww - sum(Gamma_w^2, na.rm = T)
  numer <- Gamma_wy - sum(Gamma_w * Gamma_y, na.rm = T)
  tauhat <- numer / denom

  Ytd_c <- Ytd_c - tr_c * tauhat
  
  # Handle missingness that arises from values not dense in years
  Gamma_wy[is.na(Gamma_wy)] <- 0
  tr_c[is.na(tr_c)] <- 0
  Ytd_c[is.na(Ytd_c)] <- 0
  Gamma_w[is.na(Gamma_w)] <- 0
  Gamma_y[is.na(Gamma_y)] <- 0
  
  V <- (Gamma_wy + rowSums(tr_c * Ytd_c, na.rm = T) - Ytd_c %*% Gamma_w - tr_c %*% Gamma_y) * Theta / denom
  
  tau_se <- sqrt(sum(V^2, na.rm = T)) / n
  
  tstat <- abs(tauhat / tau_se)
  pval <- pnorm(abs(tstat), lower.tail = FALSE) * 2
  return(list(tauhat = tauhat, se = tau_se,
              tstat = tstat, pval = pval,
              n.unit = nrow(Y),
              n.total = (nrow(Y) * ncol(Y))))
}

ripw.full <- function(
    data,
    name.y,
    name.w,
    vec.controls,
    name.fold,
    name.unit,
    name.unit.fe,
    name.time.fe,
    name.rip){

  # Variables to select from df
  list.keep = unique(c(name.y, name.w, vec.controls, 
                name.fold, name.unit, name.unit.fe, name.time.fe,
                name.rip))
  list.keep = list.keep[list.keep!= ""]
  
  
  # Complete cases of data
  df.ripw = dplyr::select(data, all_of(list.keep))
  df.ripw = df.ripw[complete.cases(df.ripw),]

  # Model for E[Y(0)_it | X_it]
  list.rhs = c("1", vec.controls)
  list.rhs = list.rhs[list.rhs!=""]

  # fmla.model = as.formula(
  #   paste0(name.y, " ~ ", paste(list.rhs, sep = " + "),
  #          " | ", name.unit, " + ", name.time.fe)
  # )
  fmla.model = as.formula(
    paste0(name.y, " ~ ", paste(list.rhs, collapse = " + "),
           " | ", name.time.fe)
  )


  # Step one: get E[Y(0)_it | X_it] estimates
  mu.0.hat = rep(NA, nrow(df.ripw))
  for(i in unique(df.ripw$fold)){
    
    # Hold out fold & vector for output
    idx = df.ripw$fold == i

    # Include only control units for estimation
    df.controls = df.ripw[df.ripw[[name.w]]==0,]
    idx.controls = df.controls$fold == i

    # Estimate model
    m0.model = feols(fmla.model, df.controls[!idx.controls,])

    # Predict on held out fold
    mu.0.hat[idx] = predict(m0.model, newdata = df.ripw[idx,])

  }
  
  # Get fitted values to DF
  df.ripw$muhat = mu.0.hat

  
  # Variables to RIPW format...
  ## Mu hat
  muhat <- df.ripw %>% select(name.unit, name.time.fe, muhat) %>%
    spread(name.time.fe, muhat) %>%
    select(-name.unit) %>%
    as.matrix
  
  ## Treatment
  tr <- df.ripw %>% select(name.unit, name.time.fe, name.w) %>%
    spread(name.time.fe, name.w) %>%
    select(-name.unit) %>%
    as.matrix
  
  ## Outcome
  Y <- df.ripw %>% select(name.unit, name.time.fe, name.y) %>%
    spread(name.time.fe, name.y) %>%
    select(-name.unit) %>%
    as.matrix
  
  ## RIP Weights
  Theta <- df.ripw %>% select(name.unit, name.time.fe, name.rip) %>%
    dplyr::summarise(.by = name.unit, name.rip = first(get(name.rip)))
  Theta = Theta[["name.rip"]]

  # Estimation
  output <- 
    ripw(
      Y,
      tr,
      muhat,
      Theta
    )
  
  return(output)
  
}

# HTE Analysis: District -------------------------------------------------------

## How to do heterogeneity?
pdf(file = "../../output/ripw_plots/hte_units_dist.pdf", width = 8.6, height = 6)
df.plot = rbind(analysis.dist[,-"rip"], analysis.dist.exclude)
df.plot$Trimmed = as.factor(ifelse(df.plot$pscore <= 0.02, 1, 0))

plot(df.plot[df.plot$switcher == 1,]$CEN.Dissim, 
     df.plot[df.plot$switcher == 1,]$Prop.Hisp,
     xlab = "District Segregation Index",
     ylab = "District Proportion Hispanic",
     main = "Heterogeneity Bins for RIPW Analysis",
     col = df.plot[df.plot$switcher == 1,]$Trimmed,
     pch = 16)

abline(v = median(analysis.dist[analysis.dist$switcher == 1,]$CEN.Dissim), 
       lty = 2, col = "grey", lwd=.5)
abline(h = quantile(analysis.dist[analysis.dist$switcher == 1,]$Prop.Hisp, 0.5),
       lty = 2, col = "grey", lwd=.5)
legend("topright", legend = c("Crump-style trimmed", "Included in estimation"), 
       col = c("red", "black"), 
       cex = 0.8, pch = 16)
dev.off()

# Conditions
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

# HTE Function: District -------------------------------------------------------

# Functions
hte.dist <- function(outcome, controls){
  
  if(controls == 0){
    cvec = ""
  }
  
  if(controls == 1){
    cvec = control.vector
  }
  
  # Output df
  seg = c("Low", "Low", "High", "High")
  hisp = c("Low", "High", "High", "Low")
  coef = rep(NA, 4)
  se = rep(NA, 4)
  
  # RIPW calls
  a = ripw.full(analysis.dist[cond.q1,],
    outcome,
    "trustee",
    control.vector,
    "fold",
    "NCESDist",
    "NCESDist",
    "SY",
    "rip"
  )
  b = ripw.full(analysis.dist[cond.q2,],
                outcome,
                "trustee",
                cvec,
                "fold",
                "NCESDist",
                "NCESDist",
                "SY",
                "rip"
  ) 
  c = ripw.full(analysis.dist[cond.q3,],
                outcome,
                "trustee",
                cvec,
                "fold",
                "NCESDist",
                "NCESDist",
                "SY",
                "rip"
  )
  d = ripw.full(analysis.dist[cond.q4,],
                outcome,
                "trustee",
                cvec,
                "fold",
                "NCESDist",
                "NCESDist",
                "SY",
                "rip"
  )
  
  # Fill output: coefs
  coef[1] = a$tauhat
  coef[2] = b$tauhat
  coef[3] = c$tauhat
  coef[4] = d$tauhat

  
  # Fill output: ses
  se[1] = a$se
  se[2] = b$se
  se[3] = c$se
  se[4] = d$se
  
  # Bind and return
  out = cbind(seg, hisp, coef, se)
  return(out)
}

# Reported in text: wher do SFP results come from? -----------------------------

# Actually treated
events = analysis.dist %>%
  summarise(
    .by = c("SY", "trustee"),
    events = sum(Dist.SFP.Binary, na.rm = T)
  ) %>%
  dplyr::filter(
    !(SY %in% c("2000-01", "2001-02", "2002-03", "2003-04", "2004-05"))
  )

pdf("../../output/ripw_plots/ripw_sfp_distribution.pdf", width = 8.6, height = 4.9)
ggplot(events, aes(y = events, x = SY, fill = as.factor(trustee))) + 
  geom_bar(stat = "identity") + 
  theme_classic() + 
  labs(x = "", 
       y = "SFP Projects Funded",
       fill = "Treatment Indicator",
       title = "SFP Events for Treated Units in Realised RIPW Sample",
       caption = "*Note:* SFP project binary indicator across time for districts in sample used for RIPW analysis after propensity score trimming. Trend begins in <br> 2005-06  as that is the first year at which there are treated units. Blue bars represent units that actively have ward elections when project is <br> funded.") + 
  theme(legend.pos = c(0.9,0.9),
        plot.caption = element_markdown(hjust=0))
dev.off()

# In treatment group
events = analysis.dist %>%
  summarise(
    .by = c("SY", "switcher"),
    events = sum(Dist.SFP.Binary, na.rm = T)
  ) %>%
  dplyr::filter(
    !(SY %in% c("2000-01", "2001-02", "2002-03", "2003-04", "2004-05"))
  )

pdf("../../output/ripw_plots/ripw_sfp_distribution_group.pdf", width = 8.6, height = 4.9)
ggplot(events, aes(y = events, x = SY, fill = as.factor(switcher))) + 
  geom_bar(stat = "identity") + 
  theme_classic() + 
  labs(x = "", 
       y = "SFP Projects Funded",
       fill = "Treatment Group Indicator",
       title = "SFP Events for Treatment Arm in Realised RIPW Sample",
       caption = "*Note:* SFP project binary indicator across time for districts in sample used for RIPW analysis after propensity score trimming. Trend begins in <br> 2005-06  as that is the first year at which there are treated units. Blue bars represent the treatment group.") + 
  theme(legend.pos = c(0.9,0.9),
        plot.caption = element_markdown(hjust=0))
dev.off()


# HTE: District ----------------------------------------------------------------

hte.chart <- function(outcome, legend.pos, label, controls){
  
  if(controls == 0){
    cvec = ""
  }
  
  if(controls == 1){
    cvec = control.vector
  }
  
  # Estimate fullmodel
  full.model =
    ripw.full(analysis.dist,
              outcome,
              "trustee",
              cvec,
              "fold",
              "NCESDist",
              "NCESDist",
              "SY",
              "rip"
    )
  
  # Estimate HTE
  p = as.data.frame(hte.dist(outcome, controls))
  p$coef = as.numeric(p$coef)
  p$se = as.numeric(p$se)
  p$hisp = factor(p$hisp, levels = c("Low", "High"))
  
  # Plot
  plot <- 
    ggplot(p, aes(hisp, coef)) + 
    geom_errorbar(
      aes(ymin = coef - (1.96 * se), 
          ymax = coef + (1.96 * se),
          color = seg),
      position = position_dodge(0.1), width = 0.1
    ) + 
    geom_point(aes(color = seg), position = position_dodge(0.1)) + 
    scale_color_manual(name = "District\nSegregation", 
                       values = c("#00AFBB", "#E7B800")) + 
    theme_classic() + 
    geom_hline(yintercept = 0, linetype = "solid", color = "black") + 
    labs(x = "District Hispanicity",
         y = "DATE Estimate",
         title = paste0("RIPW HTE Analysis: ", label),
         subtitle = "TWFE Outcome Model with Random Forest Treatment Model",
         caption = paste0("*Note:* Implementation of RIPW estimator for the DATE, see Arkhangelsky et al. (2023). Red line and interval is aggregate DATE. <br> Tau = ",
                          round(full.model$tauhat, digits = 3),
                          ", SE = ",
                          round(full.model$se, digits = 3),
                          ", P-value = ",
                          round(full.model$pval, digits = 3),
                          ". Districts grouped by being above or below the treatment unit median Hispanic <br> population share or dissimilarity index. ")) +
    theme(legend.position = legend.pos,
          plot.caption = element_markdown(hjust=0)) + 
    geom_hline(yintercept = full.model$tauhat,
               linetype = "longdash", color = "red") + 
    geom_hline(yintercept = full.model$tauhat + (1.96 * full.model$se),
               linetype = "dotted", color = "red") + 
    geom_hline(yintercept = full.model$tauhat - (1.96 * full.model$se),
               linetype = "dotted", color = "red")  
    
    return(plot)
  
}

# Win
pdf(file = "../../output/ripw_plots/dist_win_hisp.pdf", width = 7.9, height = 4.9)
hte.chart("Prop.Win.Hisp.C", c(0.9, 1), "Proportion Board Elected Hispanic", 1)
dev.off()

# Reported in text
ripw.full(analysis.dist[analysis.dist$year > 2003,],
          "Dist.SFP.Binary",
          "trustee",
          control.vector,
          "fold",
          "NCESDist",
          "NCESDist",
          "SY",
          "rip"
)

ripw.full(analysis.dist[cond.q3,],
          "Prop.Win.Hisp.C",
          "trustee",
          control.vector,
          "fold",
          "NCESDist",
          "NCESDist",
          "SY",
          "rip"
)

# Ran
pdf(file = "../../output/ripw_plots/dist_ran_hisp.pdf", width = 7.9, height = 4.9)
hte.chart("Prop.Ran.Hisp.C", c(0.9, 1), "Propotion Candidates Ran Hispanic", 1)
dev.off()

# Reported In Text:
ripw.full(analysis.dist[cond.q3,],
          "Prop.Ran.Hisp.C",
          "trustee",
          control.vector,
          "fold",
          "NCESDist",
          "NCESDist",
          "SY",
          "rip"
)

ripw.full(analysis.dist[cond.q4,],
          "Prop.Ran.Hisp.C",
          "trustee",
          control.vector,
          "fold",
          "NCESDist",
          "NCESDist",
          "SY",
          "rip"
)

ripw.full(analysis.dist[cond.q4 | cond.q3,],
          "Prop.Ran.Hisp.C",
          "trustee",
          control.vector,
          "fold",
          "NCESDist",
          "NCESDist",
          "SY",
          "rip"
)

ripw.full(analysis.dist[cond.q1 | cond.q2,],
          "Prop.Ran.Hisp.C",
          "trustee",
          control.vector,
          "fold",
          "NCESDist",
          "NCESDist",
          "SY",
          "rip"
)


# Binary
pdf(file = "../../output/ripw_plots/dist_sfp_bin.pdf", width = 7.9, height = 4.9)
hte.chart("Dist.SFP.Binary", c(0.9, 1), "Binary Indicator for SFP Project", 1)
dev.off()

# Reported in text
ripw.full(analysis.dist[cond.q3,],
          "Dist.SFP.Binary",
          "trustee",
          control.vector,
          "fold",
          "NCESDist",
          "NCESDist",
          "SY",
          "rip"
)


ripw.full(analysis.dist[cond.q3,],
          "Dist.SFP.Binary",
          "trustee",
          control.vector,
          "fold",
          "NCESDist",
          "NCESDist",
          "SY",
          "rip"
)

ripw.full(analysis.dist[cond.q4 | cond.q3,],
          "Dist.SFP.Binary",
          "trustee",
          control.vector,
          "fold",
          "NCESDist",
          "NCESDist",
          "SY",
          "rip"
)

ripw.full(analysis.dist[cond.q1 | cond.q2,],
          "Dist.SFP.Binary",
          "trustee",
          control.vector,
          "fold",
          "NCESDist",
          "NCESDist",
          "SY",
          "rip"
)


# Scaled
pdf(file = "../../output/ripw_plots/dist_sfp_sca.pdf", width = 7.9, height = 4.9)
hte.chart("Dist.SFP.Scaled", c(0.9, 1), "SFP Spending Per Student", 1)
dev.off()

# Reported in text
ripw.full(analysis.dist[cond.q3 | cond.q4,],
          "Dist.SFP.Scaled",
          "trustee",
          control.vector,
          "fold",
          "NCESDist",
          "NCESDist",
          "SY",
          "rip"
)

# FT
pdf(file = "../../output/ripw_plots/dist_ft.pdf", width = 7.9, height = 4.9)
hte.chart("Dist.RT.FT",  c(0.9, 1), "Full-Time Teacher-Student Ratio", 1)
dev.off()

# H
pdf(file = "../../output/ripw_plots/dist_h.pdf", width = 7.9, height = 4.9)
hte.chart("Dist.RT.H.FT",  c(0.9, 1), "Full Time Hispanic Teacher-Student Ratio", 1)
dev.off()

# EL
pdf(file = "../../output/ripw_plots/dist_el.pdf", width = 7.9, height = 4.9)
hte.chart("Dist.RT.EL.FT",  c(0.9, 1), "Full Time English-Language Teacher-Student Ratio", 1)
dev.off()

ripw.full(analysis.dist[cond.q4,],
          "Dist.RT.EL.FT",
          "trustee",
          control.vector,
          "fold",
          "NCESDist",
          "NCESDist",
          "SY",
          "rip"
)

#GFT
pdf(file = "../../output/ripw_plots/dist_g_ft.pdf", width = 7.9, height = 4.9)
hte.chart("G.RT.FT",  c(0.9, 1), "Gini Coefficient of Full Time TS Ratio", 1)
dev.off()

# Reported in text
sqrt(var(analysis.dist$G.RT.FT, na.rm = T))
sqrt(var(analysis.dist$G.RT.EL.FT, na.rm = T))
ripw.full(analysis.dist[cond.q3 | cond.q4,],
          "G.RT.FT",
          "trustee",
          control.vector,
          "fold",
          "NCESDist",
          "NCESDist",
          "SY",
          "rip"
)

#GH
pdf(file = "../../output/ripw_plots/dist_g_h.pdf", width = 7.9, height = 4.9)
hte.chart("G.RT.H.FT",  c(0.9, 1), "Gini Coefficient of Full Time Hispanic TS Ratio", 1)
dev.off()

#GEL
pdf(file = "../../output/ripw_plots/dist_g_el.pdf", width = 7.9, height = 4.9)
hte.chart("G.RT.EL.FT",  c(0.9, 1), "Gini Coefficient of Full Time EL TS Ratio", 1)
dev.off()

# Reported in text
ripw.full(analysis.dist[cond.q3 | cond.q4,],
          "G.RT.EL.FT",
          "trustee",
          control.vector,
          "fold",
          "NCESDist",
          "NCESDist",
          "SY",
          "rip"
)

ripw.full(analysis.dist[cond.q1 | cond.q2,],
          "G.RT.EL.FT",
          "trustee",
          control.vector,
          "fold",
          "NCESDist",
          "NCESDist",
          "SY",
          "rip"
)


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

## Looking at baselines 
binsreg.demeaned <- function(variable, title, ylabel){
  
  df = analysis.school %>% filter(switcher == 0)
  
  df <- 
    df %>%
    group_by(NCESDist) %>%
    mutate(distmean.var = mean(Stu.Prop.Hisp, na.rm = T)) %>%
    ungroup() %>%
    mutate(Stu.Prop.Hisp = Stu.Prop.Hisp - distmean.var)

  a = binsreg(df[[variable]], df$Stu.Prop.Hisp, plotxrange = c(-0.5, 0.5),
              polyreg = 1,by = df$CEN.HighSeg.24, 
              legendTitle = "High Seg Dist.")
   
return(a$bins_plot)
  
}


a = binsreg.demeaned("Prop.College")
pdf("../../output/school_hispanic/sch_hisp_prop_college.pdf", width = 8.3, height = 4.9)
a + 
  labs(y = "Share Attended College", x = "School Proportion Hispanic Relative to District",
       title = "Binscatter: Block Group Educational Achievement by School Hispanicity ",
       caption = "*Note:* Binned scatterplot at the school level in conrol group districts. X-axis is school student population Hispanic, demeaned by <br> district. Red indicates school in districts with above the median treated unit dissimilarity index. College attendence is taken in the <br> 2000 census for the  block group in which the school is located.")+ 
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey") + 
  theme(plot.caption = element_markdown(hjust = 0))
dev.off()
  
a = binsreg.demeaned("CEN.Unemp")
pdf("../../output/school_hispanic/sch_hisp_cen_unemp.pdf", width = 8.3, height = 4.9)
 a + 
  labs(y = "Unemployment Rate", x = "School Proportion Hispanic Relative to District",
       title = "Binscatter: Block Group Unemployment Rate by School Hispanicity ",
       caption = "*Note:* Binned scatterplot at the school level in conrol group districts. X-axis is school student population Hispanic, demeaned by <br> district. Red indicates  school in districts with above the median treated unit dissimilarity index. Unemployment rate is taken in <br> the 2000 census for the  block group in which the school is located.")+ 
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey") + 
  theme(plot.caption = element_markdown(hjust = 0))
dev.off()

a = binsreg.demeaned("RT.FT")
pdf("../../output/school_hispanic/sch_hisp_rt_ft.pdf", width = 8.3, height = 4.9)
 a+ 
  labs(y = "Teacher-Student Ratio", x = "School Proportion Hispanic Relative to District",
       title = "Binscatter: Teacher-Student Ratio by School Hispanicity ",
       caption = "*Note:* Binned scatterplot at the school level in conrol group districts. X-axis is school student population Hispanic, demeaned <br> by district. Red indicates school in districts with above the median treated unit dissimilarity index.")+ 
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey") + 
  theme(plot.caption = element_markdown(hjust = 0))
dev.off()

a = binsreg.demeaned("RT.H.FT")
pdf("../../output/school_hispanic/sch_hisp_rt_h_ft.pdf", width = 8.3, height = 4.9)
 a + 
  labs(y = "Hispanic Teacher-Student Ratio", x = "School Proportion Hispanic Relative to District",
       title = "Binscatter: Hispanic Teacher-Student Ratio by School Hispanicity ",
       caption = "*Note:* Binned scatterplot at the school level in conrol group districts. X-axis is school student population Hispanic, demeaned <br> by district. Red indicates school in districts with above the median treated unit dissimilarity index.")+ 
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey") + 
  theme(plot.caption = element_markdown(hjust = 0))
dev.off()

a = binsreg.demeaned("RT.EL.FT")
pdf("../../output/school_hispanic/sch_rt_el_ft.pdf", width = 8.3, height = 4.9)
 a+ 
  labs(y = "ELD Teacher-Student Ratio", x = "School Proportion Hispanic Relative to District",
       title = "Binscatter: ELD Teacher-Student Ratio by School Hispanicity ",
       caption = "*Note:* Binned scatterplot at the school level in conrol group districts. X-axis is school student population Hispanic, demeaned <br> by district. Red indicates school in districts with above the median treated unit dissimilarity index. ELD denotes teachers <br> certified  to teach English <br> as a second language.")+ 
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey") + 
  theme(plot.caption = element_markdown(hjust = 0))
dev.off()

a = binsreg.demeaned("SFP.Binary")
pdf("../../output/school_hispanic/sch_hisp_sfp_bin.pdf", width = 8.3, height = 4.9)
 a + 
  labs(y = "SFP Project Indicator", x = "School Proportion Hispanic Relative to District",
       title = "Binscatter: SFP Project Indicator by School Hispanicity ",
       caption = "*Note:* Binned scatterplot at the school level in conrol group districts. X-axis is school student population Hispanic, demeaned <br> by district. Red indicates school in districts with above the median treated unit dissimilarity index. SFP indicator takes value<br> one when a project is funded for the school in a given year and zero otherwise.")+ 
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey") + 
  theme(plot.caption = element_markdown(hjust = 0))
dev.off()

# School Level HTE Functions --------------------------------------------------------

hte.school <- function(outcome){
  
  # Output df
  status = c("High", "High", "High", "Low", "Low", "Low")
  quartile = c("Q1", "Q2", "Q3", "Q1", "Q2", "Q3")
  coef = rep(NA, 6)
  se = rep(NA, 6)
  
  a = ripw.full(analysis.school[(analysis.school$HH == "Yes" & analysis.school$Hisp.Quartile == 1),],
                outcome,
                "trustee",
                control.vector,
                "fold",
                "NCESSchool",
                "NCESDist",
                "SY",
                "rip")
  
  b = ripw.full(analysis.school[(analysis.school$HH == "Yes" & analysis.school$Hisp.Quartile == 2),],
                outcome,
                "trustee",
                control.vector,
                "fold",
                "NCESSchool",
                "NCESDist",
                "SY",
                "rip")
  
  c = ripw.full(analysis.school[(analysis.school$HH == "Yes" & analysis.school$Hisp.Quartile == 3),],
                outcome,
                "trustee",
                control.vector,
                "fold",
                "NCESSchool",
                "NCESDist",
                "SY",
                "rip")
  
  e = ripw.full(analysis.school[(analysis.school$HH == "No" & analysis.school$Hisp.Quartile == 1),],
                outcome,
                "trustee",
                control.vector,
                "fold",
                "NCESSchool",
                "NCESDist",
                "SY",
                "rip")
  
  f = ripw.full(analysis.school[(analysis.school$HH == "No" & analysis.school$Hisp.Quartile == 2),],
                outcome,
                "trustee",
                control.vector,
                "fold",
                "NCESSchool",
                "NCESDist",
                "SY",
                "rip")
  
  g = ripw.full(analysis.school[(analysis.school$HH == "No" & analysis.school$Hisp.Quartile == 3),],
                outcome,
                "trustee",
                control.vector,
                "fold",
                "NCESSchool",
                "NCESDist",
                "SY",
                "rip")
  

  # Fill output: coefs
  coef[1] = a$tauhat
  coef[2] = b$tauhat
  coef[3] = c$tauhat
  coef[4] = e$tauhat
  coef[5] = f$tauhat
  coef[6] = g$tauhat
  
  # Fill output: ses
  se[1] = a$se
  se[2] = b$se
  se[3] = c$se
  se[4] = e$se
  se[5] = f$se
  se[6] = g$se
  
  # Bind and return
  out = cbind(status, quartile, coef, se)
  return(out)
  
  return(a)
  
}

hte.school.plot <- function(outcome, label, legend.pos){

  # Regressions
  p = as.data.frame(hte.school(outcome))
  p$coef = as.numeric(p$coef)
  p$se = as.numeric(p$se)

  # Plot
  ggplot(p, aes(quartile, coef)) + 
    geom_errorbar(
      aes(ymin = coef - (1.96 * se), 
          ymax = coef + (1.96 * se),
          color = status),
      position = position_dodge(0.1), width = 0.1
    ) + 
    geom_point(aes(color = status), position = position_dodge(0.1)) + 
    scale_color_manual(name = "District\nHispanicity\n& Segregation", 
                       values = c("#00AFBB", "#E7B800")) + 
    theme_classic()+ 
    geom_hline(yintercept = 0, linetype = "dashed", color = "black")+ 
    labs(x = "Student Population Hispanicity Relative to District",
         y = "DATE Estimate",
         title = paste0("School-Level RIPW HTE Analysis: ", label),
         subtitle = "TWFE Outcome Model with Random Forest Treatment Model",
         caption = paste0("*Note:* Implementation of RIPW estimator for the DATE, see Arkhangelsky et al. (2023). Estimation separately for schools in districts with<br> simultaneously above and below treated unit average Hispanicity and dissimularity index. Schools are then grouped by tertiles of student<br> Hispanic population relative to the school-wise average in the district.")) + 
    theme(legend.position = legend.pos,
          plot.caption = element_markdown(hjust = 0))

}

pdf("../../output/ripw_plots_school/sch_ripw_sfp_bin.pdf", width = 8.3, height = 4.9)
hte.school.plot("SFP.Binary", "Binary SFP Project Indicator", c(0.9, 0.9))
dev.off()

# This version in only high high districts
hte.school.plot <- function(outcome, label, legend.pos){
  
  # Regressions
  p = as.data.frame(hte.school(outcome))
  p$coef = as.numeric(p$coef)
  p$se = as.numeric(p$se)
  
  # Plot
  ggplot(p[p$status == "High",], aes(quartile, coef)) + 
    geom_errorbar(
      aes(ymin = coef - (1.96 * se), 
          ymax = coef + (1.96 * se),
          color = status),
      position = position_dodge(0.1), width = 0.1
    ) + 
    geom_point(aes(color = status), position = position_dodge(0.1)) + 
    scale_color_manual(name = "District\nHispanicity\n& Segregation", 
                       values = c("#00AFBB", "#E7B800")) + 
    theme_classic()+ 
    geom_hline(yintercept = 0, linetype = "dashed", color = "black")+ 
    labs(x = "Student Population Hispanicity Relative to District",
         y = "DATE Estimate",
         title = paste0("School-Level RIPW HTE Analysis: ", label),
         subtitle = "TWFE Outcome Model with Random Forest Treatment Model",
         caption = paste0("*Note:* Implementation of RIPW estimator for the DATE, see Arkhangelsky et al. (2023). Estimation separately for schools in districts with<br> simultaneously above treated unit average Hispanicity and dissimilarity index. Schools are then grouped by tertiles of student<br> Hispanic population relative to the school-wise average in the district.")) + 
    theme(legend.position = legend.pos,
          plot.caption = element_markdown(hjust = 0))
  
}

pdf("../../output/ripw_plots_school/sch_ripw_rt_ft.pdf", width = 8.3, height = 4.9)
hte.school.plot("RT.FT", "Teacher-Student Ratio", c(0.9, 0.2))
dev.off()

ripw.full(analysis.school[(analysis.school$HH == "Yes" & analysis.school$Hisp.Quartile == 3),],
          "RT.FT",
          "trustee",
          control.vector,
          "fold",
          "NCESSchool",
          "NCESDist",
          "SY",
          "rip")


pdf("../../output/ripw_plots_school/sch_ripw_rt_h_ft.pdf", width = 8.3, height = 4.9)
hte.school.plot("RT.H.FT", "Hispanic Teacher-Student Ratio", c(0.9, 0.9))
dev.off()


pdf("../../output/ripw_plots_school/sch_ripw_rt_el_ft.pdf", width = 8.3, height = 4.9)
hte.school.plot("RT.EL.FT", "ELD Teacher-Student Ratio", c(0.9, 0.9))
dev.off()

ripw.full(analysis.school[(analysis.school$HH == "No"),],
          "RT.EL.FT",
          "trustee",
          control.vector,
          "fold",
          "NCESSchool",
          "NCESDist",
          "SY",
          "rip")
          

ripw.full(analysis.school[(analysis.school$HH == "Yes" & analysis.school$Hisp.Quartile == 2),],
          "RT.EL.FT",
          "trustee",
          control.vector,
          "fold",
          "NCESSchool",
          "NCESDist",
          "SY",
          "rip")

ripw.full(analysis.school[(analysis.school$HH == "Yes" & analysis.school$Hisp.Quartile == 1),],
          "RT.EL.FT",
          "trustee",
          control.vector,
          "fold",
          "NCESSchool",
          "NCESDist",
          "SY",
          "rip")


# This version in only high low districts
hte.school.plot <- function(outcome, label, legend.pos){
  
  # Regressions
  p = as.data.frame(hte.school(outcome))
  p$coef = as.numeric(p$coef)
  p$se = as.numeric(p$se)
  
  # Plot
  ggplot(p[p$status == "Low",], aes(quartile, coef)) + 
    geom_errorbar(
      aes(ymin = coef - (1.96 * se), 
          ymax = coef + (1.96 * se),
          color = status),
      position = position_dodge(0.1), width = 0.1
    ) + 
    geom_point(aes(color = status), position = position_dodge(0.1)) + 
    scale_color_manual(name = "District\nHispanicity\n& Segregation", 
                       values = c("#00AFBB", "#E7B800")) + 
    theme_classic()+ 
    geom_hline(yintercept = 0, linetype = "dashed", color = "black")+ 
    labs(x = "Student Population Hispanicity Relative to District",
         y = "DATE Estimate",
         title = paste0("School-Level RIPW HTE Analysis: ", label),
         subtitle = "TWFE Outcome Model with Random Forest Treatment Model",
         caption = paste0("*Note:* Implementation of RIPW estimator for the DATE, see Arkhangelsky et al. (2023). Estimation separately for schools in districts with<br> simultaneously above treated unit average Hispanicity and dissimilarity index. Schools are then grouped by tertiles of student<br> Hispanic population relative to the school-wise average in the district.")) + 
    theme(legend.position = legend.pos,
          plot.caption = element_markdown(hjust = 0))
  
}

pdf("../../output/ripw_plots_school/sch_ripw_rt_ft_low.pdf", width = 8.3, height = 4.9)
hte.school.plot("RT.FT", "Teacher-Student Ratio", c(0.9, 0.2))
dev.off()

pdf("../../output/ripw_plots_school/sch_ripw_rt_h_ft_low.pdf", width = 8.3, height = 4.9)
hte.school.plot("RT.H.FT", "Hispanic Teacher-Student Ratio", c(0.9, 0.9))
dev.off()


pdf("../../output/ripw_plots_school/sch_ripw_rt_el_ft_low.pdf", width = 8.3, height = 4.9)
hte.school.plot("RT.EL.FT", "ELD Teacher-Student Ratio", c(0.9, 0.9))
dev.off()
          


# In text report: 
sum(analysis.school$SFP.Binary==1, na.rm = T)/nrow(analysis.school)
sum(analysis.dist$Dist.SFP.Binary==1, na.rm = T)/nrow(analysis.dist)
mean(analysis.dist$Dist.RT.FT, na.rm=T)
