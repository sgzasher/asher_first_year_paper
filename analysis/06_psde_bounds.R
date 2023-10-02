# Script Target: Bounds on PSDE direct effects 

# Packages ---------------------------------------------------------------------
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
set.seed(1966)
library(dplyr)
library(data.table)
library(stringr)
library(DescTools)
library(fixest)
library(stargazer)
library(ggplot2)
library(ggtext)


# Packages ---------------------------------------------------------------------
df.pscore = fread("../../data/output/analysis/2012_crossec_pscores.csv")
df.dist = fread("../../data/output/analysis/district_regressions.csv")
df.pscore = df.pscore[,-"V1"]

df.dist <- 
  left_join(
    df.dist,
    df.pscore,
    by = "NCESDist"
  )

# Frame for analysis -----------------------------------------------------------


hajek.psde <- function(outcome, input.year){
  
# Units with elections in 2012
df.dist.2012 = df.dist[df.dist$SY == "2012-13", ]
df.dist.2012 = df.dist.2012[!is.na(df.dist.2012$Num.Elected),]
df.dist.2012 = df.dist.2012[df.dist.2012$include == 1,]
df.dist.2012 = df.dist.2012[(df.dist.2012$switcher == 1 & df.dist.2012$trustee == 1) | 
                            (df.dist.2012$switcher == 0)]

table(df.dist.2012$trustee)

# Get their 2012 elections and treatment data
df.dist.2012 = df.dist.2012 %>%
  dplyr::select(
    NCESDist, Sum.Win.Hisp.C, trustee, pscore.2012.csec
  )

# Get their 2013 outcome data
df.dist.2013 = df.dist[df.dist$SY == input.year, ]
df.dist.2013 = dplyr::select(df.dist.2013,
                             NCESDist,
                             outcome)

# Stitch together
df.analysis = left_join(
  df.dist.2012,
  df.dist.2013,
  by = "NCESDist"
)

# Trim for pscores; again balancing stability and power... 
# (not that I have power)
df.analysis = df.analysis[df.analysis$pscore.2012.csec > 0.05 & df.analysis$pscore.2012.csec < 0.16]

# IPW weight
df.analysis$IPW = ifelse(
  df.analysis$trustee == 1,
  df.analysis$pscore.2012.csec,
  1 - df.analysis$pscore.2012.csec
)

# Mediator Coding --------------------------------------------------------------
df.analysis$mediator = df.analysis$Sum.Win.Hisp.C > 1

# Basic Stabilized Horwitz-Thompson --------------------------------------------
df.analysis.c = df.analysis[df.analysis$trustee == 0,]
df.analysis.t = df.analysis[df.analysis$trustee == 1,]

df.analysis.t[[outcome]]
mean(df.analysis.c[[outcome]])

denom.t = mean(df.analysis.t$trustee/df.analysis.t$IPW)
denom.c = mean((1 - df.analysis.c$trustee)/df.analysis.c$IPW)

# Hajek estimator
hajek = 
mean((df.analysis.t[[outcome]] / df.analysis.t$IPW) / denom.t, na.rm = T) - 
mean((df.analysis.c[[outcome]] / df.analysis.c$IPW) / denom.c, na.rm = T)

# Bootstrapped bounds for Hajek estimator (not technically correct)
ests = rep(NA, 100)
for(i in 1:100){
  a = sample_n(df.analysis, nrow(df.analysis), replace = T)
  a.t = a[a$trustee == 1,]
  a.c = a[a$trustee == 0,]
  denom.t.bs = mean(a.t$trustee/a.t$IPW)
  denom.c.bs = mean((1 - a.c$trustee)/a.c$IPW)
  
  ests[i] = mean((a.t[[outcome]] / a.t$IPW) / denom.t.bs, na.rm = T) - 
    mean((a.c[[outcome]] / a.c$IPW) / denom.c.bs, na.rm = T)
  
}

# Identify Proportion of Never Takers ------------------------------------------
denom = 1 - mean(df.analysis.c$mediator)
stabil.never = mean(df.analysis$trustee/df.analysis$pscore.2012.csec)
numer = 1 - mean((df.analysis$mediator * df.analysis$trustee)/df.analysis$pscore.2012.csec)/stabil.never

nt.00 = numer/denom

# Identify Proportion of Always Takers ------------------------------------------
denom = mean(df.analysis.t$mediator)
stabil.always =  mean((1 - df.analysis$trustee)/(1-df.analysis$pscore.2012.csec))
numer = mean((df.analysis$mediator * (1-df.analysis$trustee)))/stabil.always

at.11 = numer/denom

# Bounds on PSDE(0) ------------------------------------------------------------
# Transformed outcome
df.analysis$transformed = ifelse(df.analysis$trustee == 1,
  ((df.analysis.t[[outcome]] / df.analysis.t$IPW) / denom.t), 
  ((df.analysis.c[[outcome]] / df.analysis.c$IPW) / denom.c))


# Lower PSDE(0) bound
df.analysis.10 = df.analysis[df.analysis$trustee == 1 & df.analysis$mediator ==0,]
df.analysis.00 = df.analysis[df.analysis$trustee == 0 & df.analysis$mediator ==0,]
a = quantile(df.analysis.00$transformed, numer/denom, na.rm = T)

lower = 
mean(df.analysis.10$transformed) - 
mean(df.analysis.00[df.analysis.00$transformed >= a,]$transformed)

# Upper PSDE(0) bound
upper = 
mean(df.analysis.10$transformed) - 
  mean(df.analysis.00[df.analysis.00$transformed <= a,]$transformed)

# Plot -------------------------------------------------------------------------
uppers = c(upper, max(ests, na.rm = T))
lowers = c(lower, min(ests, na.rm = T))
center = c((lower - upper) / 2, hajek)
title = c("PSDE(0) Bounds", "Hajek ATE Estimator")

df = as.data.frame(cbind(title, uppers, lowers, center))
df$uppers = as.numeric(df$uppers)
df$lowers = as.numeric(df$lowers)
df$center = as.numeric(df$center)


return(df)

}


# Dist.RT.EL.FT ----------------------------------------------------------------
o = hajek.psde("Dist.RT.EL.FT", "2006-07")
j = hajek.psde("Dist.RT.EL.FT", "2007-08")
p = hajek.psde("Dist.RT.EL.FT", "2008-09")

a = hajek.psde("Dist.RT.EL.FT", "2012-13")
b = hajek.psde("Dist.RT.EL.FT", "2013-14")
c = hajek.psde("Dist.RT.EL.FT", "2014-15")
d = hajek.psde("Dist.RT.EL.FT", "2015-16")

df = rbind(o,j,p, a,b,c,d)
df$SY = c(
  "2006-07", "2006-07",
  "2007-08", "2007-08",
  "2008-09", "2008-09",
  "2012-13", "2012-13", 
  "2013-14", "2013-14",
  "2014-15", "2014-15", 
  "2015-16", "2015-16")

# Reporting in text
df

df$center = ifelse(df$title == "Hajek ATE Estimator", df$center, NA)

pdf("../../output/psde_bounds/psde_el_rt.pdf", width = 8.2, height = 5.3)
ggplot(df, aes(SY, center, col = title)) +
  geom_errorbar(aes(ymin = lowers,
                ymax = uppers),
                position = position_dodge(0.2), 
                width = 0.1) +
  theme_classic() +
  geom_point(aes(col = title), position = position_dodge(0.2)) + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") + 
  geom_vline(xintercept = 3.5, linetype = "dashed", color = "grey") + 
  labs(col = "Estimation",
       title = "ELD Teacher-Student Ratio in 2012 Treatment Cohort",
       subtitle = "Hajek ATE Estimator vs. Observational Bounds on PSDE for Never-Takers",
       y = "Estimate",
       x = "",
       caption = "The chart presents estimates for the 2012 ward election treatment cohort. Data on teacher qualifications are unavailable between the <br> 2010-11 and 2012-13 school years, hence the missing years. Hajek estimation is a stabilized IPW estimator for the ATE given <br> propensity score estimates and standard errors presented are bootstrapped without re-estimation of the propensity score i.e. are <br> slightly too narrow. Bounds on the direct effect for never-takers are derived in Propositions 1 and 2 in the text and rely on observable <br> data, selection-on-observables, and a monotonicity assumption. Years before 2012 presented as placebo checks.") + 
  theme(legend.position = c(0.9,0.9),
        plot.caption = element_markdown(hjust=0))
dev.off()

# Dist.RT.FT -------------------------------------------------------------------
o = hajek.psde("Dist.RT.FT", "2006-07")
j = hajek.psde("Dist.RT.FT", "2007-08")
p = hajek.psde("Dist.RT.FT", "2008-09")

a = hajek.psde("Dist.RT.FT", "2012-13")
b = hajek.psde("Dist.RT.FT", "2013-14")
c = hajek.psde("Dist.RT.FT", "2014-15")
d = hajek.psde("Dist.RT.FT", "2015-16")

df = rbind(o,j,p, a,b,c,d)
df$SY = c(
  "2006-07", "2006-07",
  "2007-08", "2007-08",
  "2008-09", "2008-09",
  "2012-13", "2012-13", 
  "2013-14", "2013-14",
  "2014-15", "2014-15", 
  "2015-16", "2015-16")

df$center = ifelse(df$title == "Hajek ATE Estimator", df$center, NA)

pdf("../../output/psde_bounds/psde_rt.pdf", width = 8.6, height = 5.3)
ggplot(df, aes(SY, center, col = title)) +
  geom_errorbar(aes(ymin = lowers,
                    ymax = uppers),
                position = position_dodge(0.2), 
                width = 0.1) +
  theme_classic() +
  geom_point(aes(col = title), position = position_dodge(0.2)) + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") + 
  geom_vline(xintercept = 3.5, linetype = "dashed", color = "grey") + 
  labs(col = "Estimation",
       title = "Teacher-Student Ratio in 2012 Treatment Cohort",
       subtitle = "Hajek ATE Estimator vs. Observational Bounds on PSDE for Never-Takers",
       y = "Estimate",
       x = "",
       caption = "The chart presents estimates for the 2012 ward election treatment cohort. Data on teacher qualifications are unavailable between the <br> 2010-11 and 2012-13 school years, hence the missing years. Hajek estimation is a stabilized IPW estimator for the ATE given <br> propensity score estimates and standard errors presented are bootstrapped without re-estimation of the propensity score i.e. are <br> slightly too narrow. Bounds on the direct effect for never-takers are derived in Propositions 1 and 2 in the text and rely on observable <br> data, selection-on-observables, and a monotonicity assumption. Years before 2012 presented as placebo checks.") + 
  theme(plot.caption = element_markdown(hjust=0))
dev.off()

# Dist.RT.H.FT -----------------------------------------------------------------
o = hajek.psde("Dist.RT.H.FT", "2006-07")
j = hajek.psde("Dist.RT.H.FT", "2007-08")
p = hajek.psde("Dist.RT.H.FT", "2008-09")

a = hajek.psde("Dist.RT.H.FT", "2012-13")
b = hajek.psde("Dist.RT.H.FT", "2013-14")
c = hajek.psde("Dist.RT.H.FT", "2014-15")
d = hajek.psde("Dist.RT.H.FT", "2015-16")

df = rbind(o,j,p, a,b,c,d)
df$SY = c(
  "2006-07", "2006-07",
  "2007-08", "2007-08",
  "2008-09", "2008-09",
  "2012-13", "2012-13", 
  "2013-14", "2013-14",
  "2014-15", "2014-15", 
  "2015-16", "2015-16")

df$center = ifelse(df$title == "Hajek ATE Estimator", df$center, NA)

pdf("../../output/psde_bounds/psde_h_rt.pdf", width = 8.6, height = 5.3)
ggplot(df, aes(SY, center, col = title)) +
  geom_errorbar(aes(ymin = lowers,
                    ymax = uppers),
                position = position_dodge(0.2), 
                width = 0.1) +
  theme_classic() +
  geom_point(aes(col = title), position = position_dodge(0.2)) + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") + 
  geom_vline(xintercept = 3.5, linetype = "dashed", color = "grey") + 
  labs(col = "Estimation",
       title = "Hispanic Teacher-Student Ratio in 2012 Treatment Cohort",
       subtitle = "Hajek ATE Estimator vs. Observational Bounds on PSDE for Never-Takers",
       y = "Estimate",
       x = "",
       caption = "The chart presents estimates for the 2012 ward election treatment cohort. Data on teacher qualifications are unavailable between the <br> 2010-11 and 2012-13 school years, hence the missing years. Hajek estimation is a stabilized IPW estimator for the ATE given <br> propensity score estimates and standard errors presented are bootstrapped without re-estimation of the propensity score i.e. are <br> slightly too narrow. Bounds on the direct effect for never-takers are derived in Propositions 1 and 2 in the text and rely on observable <br> data, selection-on-observables, and a monotonicity assumption. Years before 2012 presented as placebo checks.") + 
  theme(plot.caption = element_markdown(hjust=0))
dev.off()

# Overlap ----------------------------------------------------------------------

# Reported in text
nrow(df.analysis)
table(df.analysis$trustee)

pdf("../../output/psde_bounds/psde_overlap.pdf", width = 8.6, height = 5.3)
ggplot(df.analysis, aes(pscore.2012.csec, fill = as.factor(trustee))) + 
  geom_histogram(col = "black") + 
  theme_classic() + 
  theme(legend.position = c(0.9,0.9)) + 
  labs(fill = "Treatment Indicator",
       title = "Overlap For 2012 Treated Cohort",
       subtitle = "After 5% Propensity Score Trimming",
       x = "Cumulative Propensity Score",
       y = "Count")
dev.off()
        