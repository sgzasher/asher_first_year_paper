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
analysis.dist = analysis.dist[analysis.dist$pscore > .05]


# Contruct RIP weights ---------------------------------------------------------

# Dist Level
max.year <- max(analysis.dist$treat.time)
Pi <- rep(0, max.year)
Pi[2:max.year-1] <- 1 / 2 / max.year
Pi[c(1, max.year)] <- (max.year + 1) / 4 / max.year
rip = Pi[analysis.dist$treat.time] / analysis.dist$pscore

analysis.dist$rip = rip

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

# Prep -------------------------------------------------------------------------

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
         subtitle = "TWFE Outcome Model with Random Forest Treatment Model Trimming for 5%",
         caption = paste0("*Note:* Implementation of RIPW estimator for the DATE, see Arkhangelsky et al. (2023). Red line and interval is aggregate DATE. <br> Tau = ",
                          round(full.model$tauhat, digits = 3),
                          ", SE = ",
                          round(full.model$se, digits = 3),
                          ", P-value = ",
                          round(full.model$pval, digits = 3),
                          ". Districts grouped by being above or below the treatment unit median Hispanic <br> population share or dissimilarity index. Notice extremely large confidence intervals due to power problems with the 5% estimand.")) +
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

# Demonstration of instability
pdf(file = "../../output/ripw_robust/stability_5_pc.pdf", width = 7.9, height = 4.9)
hte.chart("Prop.Win.Hisp.C", c(0.9, 1), "Proportion Board Elected Hispanic", 1)
dev.off()
