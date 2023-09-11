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

# Crump Trimming ---------------------------------------------------------------

# Honestly this is really really light on overlap
# Just get rid of some literal zero treatment prob units
analysis.dist = analysis.dist[analysis.dist$pscore > 0.01,]

# Contruct RIP weights ---------------------------------------------------------
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
    name.time,
    name.rip){

  # Variables to select from df
  list.keep = c(name.y, name.w, vec.controls, 
                name.fold, name.unit, name.time,
                name.rip)
  list.keep = list.keep[list.keep!= ""]

  
  # Complete cases of data
  df.ripw = dplyr::select(data, list.keep)
  df.ripw = df.ripw[complete.cases(df.ripw),]

  # Model for E[Y(0)_it | X_it]
  list.rhs = c("1", vec.controls)
  list.rhs = list.rhs[list.rhs!=""]
  fmla.model = as.formula(
    paste0(name.y, " ~ ", paste(list.rhs, sep = " + "),
           " | ", name.unit, " + ", name.time)
  )

  # Step one: get E[Y(0)_it | X_it] estimates
  mu.0.hat = rep(NA, nrow(df.ripw))
  for(i in unique(df.ripw$fold)){
    
    # Hold out fold & vector for output
    idx = df.ripw$fold == i

    # Include only control units for estimation
    df.controls = df.ripw[df.ripw[[name.w]]==0,]
    
    # Estimate model
    m0.model = feols(fmla.model, df.controls[-idx])

    # Predict on held out fold
    mu.0.hat[idx] = predict(m0.model, newdata = df.ripw[idx])

  }
  
  # Get fitted values to DF
  df.ripw$muhat = mu.0.hat
  
  # Variables to RIPW format...
  
  ## Mu hat
  muhat <- df.ripw %>% select(name.unit, name.time, muhat) %>%
    spread(name.time, muhat) %>%
    select(-name.unit) %>%
    as.matrix
  
  ## Treatment
  tr <- df.ripw %>% select(name.unit, name.time, name.w) %>%
    spread(name.time, name.w) %>%
    select(-name.unit) %>%
    as.matrix
  
  ## Outcome
  Y <- df.ripw %>% select(name.unit, name.time, name.y) %>%
    spread(name.time, name.y) %>%
    select(-name.unit) %>%
    as.matrix
  
  ## RIP Weights
  Theta <- df.ripw %>% select(name.unit, name.time, name.rip) %>%
    spread(name.time, name.rip) %>%
    select(-name.unit) %>%
    select(1) %>%
    as.vector %>%
    unlist()
  
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

# HTE Function -----------------------------------------------------------------

# Functions
hte.dist <- function(outcome){
  
  # Output df
  seg = c(rep("High", 4), rep("Low", 4))
  hisp = rep(c("Q1", "Q2", "Q3", "Q4"), 2)
  coef = rep(NA, 8)
  se = rep(NA, 8)
  
  # RIPW calls
  a = ripw.full(analysis.dist[analysis.dist$CEN.HighSeg.24=="Yes" & cond.q1,],
    outcome,
    "trustee",
    "",
    "fold",
    "NCESDist",
    "SY",
    "rip"
  )
  b = ripw.full(analysis.dist[analysis.dist$CEN.HighSeg.24=="Yes" & cond.q2,],
                outcome,
                "trustee",
                "",
                "fold",
                "NCESDist",
                "SY",
                "rip"
  ) 
  c = ripw.full(analysis.dist[analysis.dist$CEN.HighSeg.24=="Yes" & cond.q3,],
                outcome,
                "trustee",
                "",
                "fold",
                "NCESDist",
                "SY",
                "rip"
  )
  d = ripw.full(analysis.dist[analysis.dist$CEN.HighSeg.24=="Yes" & cond.q4,],
                outcome,
                "trustee",
                "",
                "fold",
                "NCESDist",
                "SY",
                "rip"
  )
  e = ripw.full(analysis.dist[analysis.dist$CEN.HighSeg.24=="No" & cond.q1,],
                outcome,
                "trustee",
                "",
                "fold",
                "NCESDist",
                "SY",
                "rip"
  )
  f = ripw.full(analysis.dist[analysis.dist$CEN.HighSeg.24=="No" & cond.q2,],
                outcome,
                "trustee",
                "",
                "fold",
                "NCESDist",
                "SY",
                "rip"
  )
  g = ripw.full(analysis.dist[analysis.dist$CEN.HighSeg.24=="No" & cond.q3,],
                outcome,
                "trustee",
                "",
                "fold",
                "NCESDist",
                "SY",
                "rip"
  )
  h = ripw.full(analysis.dist[analysis.dist$CEN.HighSeg.24=="No" & cond.q4,],
                outcome,
                "trustee",
                "",
                "fold",
                "NCESDist",
                "SY",
                "rip"
  )
  
  # Fill output: coefs
  coef[1] = a$tauhat
  coef[2] = b$tauhat
  coef[3] = c$tauhat
  coef[4] = d$tauhat
  coef[5] = e$tauhat
  coef[6] = f$tauhat
  coef[7] = g$tauhat
  coef[8] = h$tauhat
  
  # Fill output: ses
  se[1] = a$se
  se[2] = b$se
  se[3] = c$se
  se[4] = d$se
  se[5] = e$se
  se[6] = f$se
  se[7] = g$se
  se[8] = h$se
  
  # Bind and return
  out = cbind(seg, hisp, coef, se)
  return(out)
}

# Conditions
x = summary(analysis.dist[analysis.dist$switcher==1,]$Prop.Hisp)
cond.q1 = (analysis.dist$include == 1 &
      ((analysis.dist$switcher == 1 & 
      (analysis.dist$Prop.Hisp >= x[[1]] & analysis.dist$Prop.Hisp < x[[2]])) | 
      (analysis.dist$switcher == 0)))
cond.q2 = (analysis.dist$include == 1 &
      ((analysis.dist$switcher == 1 & 
      (analysis.dist$Prop.Hisp >= x[[2]] & analysis.dist$Prop.Hisp < x[[4]])) | 
      (analysis.dist$switcher == 0)))
cond.q3 = (analysis.dist$include == 1 &
      ((analysis.dist$switcher == 1 & 
      (analysis.dist$Prop.Hisp >= x[[4]] & analysis.dist$Prop.Hisp < x[[5]])) | 
      (analysis.dist$switcher == 0)))
cond.q4 = (analysis.dist$include == 1 &
      ((analysis.dist$switcher == 1 & 
      (analysis.dist$Prop.Hisp >= x[[5]] & analysis.dist$Prop.Hisp <= x[[6]])) | 
      (analysis.dist$switcher == 0)))

# Example estimation
p = as.data.frame(hte.dist("Dist.SFP.Scaled"))
p$coef = as.numeric(p$coef)
p$se = as.numeric(p$se)

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
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") + 
  labs(x = "Hispanic (Treated Unit) Quartiles",
       y = "DATE Estimate",
       title = "RIPW HTE Analysis: SFP Project Intensive Margin",
       subtitle = "TWFE Outcome Model with Random Forest Treatment Model",
       caption = "See Arkhangelsky et al. (2023)") + 
  theme(legend.position = c(0.9, 0.13))
