# Script Target: Pscore modeling

# Packages ---------------------------------------------------------------------
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

# Data -------------------------------------------------------------------------

# write.csv struggles with long files
csv.fix <- function(df){
  df = df[,-"V1"]
  df = df[!(duplicated(df))]
  return(df)
}

# Read and fix
cdd.dist = fread("../../data/output/analysis/cdd_analysis_district.csv")
cen.dist = fread("../../data/output/analysis/census_district.csv")
ana.dist = fread("../../data/output/analysis/district_regressions.csv")
fra.dist = fread("../../data/output/frames/frame_district.csv")
cdd.dist = csv.fix(cdd.dist)
cen.dist = csv.fix(cen.dist)
ana.dist = csv.fix(ana.dist)

# Treatment Data ----------------------------------------------------------------

# Need time-to-event and event data
# Recall: we don't have treatment data outside the causal sample! 
# so don't use it!

# Variables
df.treat <- 
  dplyr::select(
    ana.dist, 
    NCESDist, switcher, causal, 
    switch.type, switch.keep, SY,
    trustee, include, include2
    )

# Ignore if not incuded in preferred specs
df.treat <- 
  dplyr::filter(
    df.treat,
    include == 1
  )

# Switch year
switch.year <- 
  dplyr::filter(df.treat, trustee == 1) %>%
  dplyr::mutate(year = as.numeric(str_sub(SY, 1, 4))) %>%
  summarise(
    .by = "NCESDist",
    switch.year = min(year, na.rm = T)
  )

df.treat = left_join(df.treat, switch.year, by = "NCESDist")

df.treat$treat.time = 
  ifelse(
    !is.na(df.treat$switch.year),
    df.treat$switch.year - 2000,
    16
  ) 

df.treat <- df.treat %>%
  dplyr::select(
    NCESDist, trustee, treat.time, include, include2,
    switcher
  )

# Training Data ----------------------------------------------------------------

# Census data & CCD variables
train.dist = cdd.dist %>% dplyr::filter(SY == "2000-01")
train.dist = left_join(train.dist, cen.dist, by = c("NCESDist"))

# Get just the relevant districts
fra.dist = dplyr::select(fra.dist, NCESDist) %>% dplyr::mutate(inframe = 1)
train.dist = left_join(train.dist, fra.dist, by = c("NCESDist")) %>%
  dplyr::filter(inframe == 1)

# Get treatment data
train.dist = left_join(train.dist, df.treat[!(duplicated(df.treat))], 
                       by = "NCESDist")

# Characters to factor
train.dist <-
  train.dist %>%
  dplyr::mutate(
    across(where(bit64::is.integer64),
    as.numeric)
  )

# Units outside the sample
train.dist <- train.dist[!(is.na(train.dist$treat.time))]

# Variables with no variation
train.dist <- train.dist %>% dplyr::select(
  -c(Rev.State.Billingual, Debt.Short.Outstanding.Start, 
     Debt.Short.Outstanding.End))

# Multiple obs of some districts
train.dist <- train.dist[!(duplicated(train.dist$NCESDist))]
train.dist <- train.dist[train.dist[["include"]]==1,]

# Normalise Training Variables -------------------------------------------------
norm.variable = 
colnames(train.dist[,-c("NCESDist", "SY", "include", "include2", "switcher", "inframe",
                       "NCESDist", "SY", "trustee", "trustee.forced",
                       "treat.time", "switcher.forced")])

for(i in norm.variable){
  vector <- as.vector(train.dist[[i]])
  mean <- mean(vector, na.rm=TRUE)
  sdev <- sqrt(var(vector, na.rm=TRUE))
  vector = vector - mean
  vector = vector / sdev
  train.dist[[i]] = vector
}

# Train propensity score model: Include A --------------------------------------
est.data = impute(
  Surv(treat.time, switcher) ~ ., 
  train.dist[,
             -c("include", "include2", "inframe",
                "NCESDist", "SY", "trustee")]
)

tune.list = tune(
  Surv(treat.time, switcher) ~ ., 
  est.data
)

print(tune.list$optimal)

# Parameters taken from tuning above
pscore.model = 
  rfsrc(Surv(treat.time, switcher)~., 
        est.data,
        ntree = 1000, 
        nodesize = tune.list$optimal[[1]], 
        nsplit = 50, 
        mtry = tune.list$optimal[[2]])


#### Example Plot ####
# example treat and control unit
which(train.dist[train.dist[["include"]]==1,]$switcher==1)
y.hat = rbind(pscore.model$survival.oob[237,],
              pscore.model$survival.oob[5,])

train.dist[["treat.time"]][237]
train.dist[["NCESDist"]][237]
train.dist[["treat.time"]][5]
train.dist[["NCESDist"]][5]

y.hat = as.data.frame(t(y.hat))
y.hat$time.interest = pscore.model$time.interest

pdf(file = "../../output/pscore_plots/surv_curves.pdf", width = 8.6, height = 6)
plot(round(y.hat$time.interest,2), y.hat[,1], type="l", xlab="Time (Year)",   
     ylab="Survival", col=1, lty=1, lwd=2, xaxt = "n",
     main = "Example Survival Curves",
     sub = "Random Forest Model on 2000 Census Block Group Data")
lines(round(y.hat$time.interest,2), y.hat[,2], col=2, lty=2, lwd=2)
abline(v = 9, col = "blue", lwd = 1, lty = 2)
axis(1, at = c(5, 8, 11, 14, 16),  labels = c(2005, 2008, 2011, 2014, 2016))
legend("topright", legend=c("Example Treated Unit (NCES: 624660)","Example Control Unit (NCES: 600016)"), col=c(1:2), lty=1:2, cex=1, lwd=2)
dev.off()

# Convert to Propensity Score --------------------------------------------------

# Interpolate missing years
survival.to.pscore <- function(vector.survival, year.treated, a = 1){
  year.treated = year.treated + 1
  survival.full = rep(NA, 16)
  
  if(a == 1){
    survival.full[c(5, 8, 9, 10, 11, 12, 13, 14, 15, 16)] = vector.survival
  }
  else{
    survival.full[c(2, 5, 8, 9, 10, 11, 12, 13, 14, 15, 16)] = vector.survival
  }
  
  survival.full[1:4] = 1
  survival.full.approx = na.approx(survival.full)
  
  pscore = ifelse(
    year.treated < 16,
    survival.full.approx[year.treated] - (survival.full.approx[year.treated+1]),
    1 - survival.full.approx[16]
  )
  
  return(pscore)
}

# Pscores: trained on only forced switcheds
check = train.dist[train.dist[["include"]]==1,]
table(check$treat.time)
pscores = lapply(
  1:nrow(check),
  FUN = function(x){
    survival.to.pscore(
    pscore.model$survival.oob[x,],
    check$treat.time[x],
    a = 1
  )}
)

# Get the pscores
pscores = as.data.frame(unlist(pscores))
pscores$NCESDist = check$NCESDist
colnames(pscores)[1] = "pscore.forced"

# With treat time
ttime = dplyr::select(df.treat, NCESDist, treat.time)
pscores = left_join(pscores, ttime, by = "NCESDist")

# Also get the pscore for the 2012 treatment cohort
pscores.2012 = lapply(
  1:nrow(check),
  FUN = function(x){
    survival.to.pscore(
      pscore.model$survival.oob[x,],
      13,
      a = 1
    )}
)

# Get them
pscores.2012 = as.data.frame(unlist(pscores.2012))
pscores.2012$NCESDist = check$NCESDist
colnames(pscores.2012)[1] = "pscore.2012"


# Variable Balance -------------------------------------------------------------
covariates.balance <- c("CEN.Dissim", "Prop.Hisp", "Dist.SFP.Binary", "Dist.RT.FT", "Dist.Stu.Hisp", "Exp.Total.Scaled")

# CEN.Dissim, CEN.Prop.Hisp, Dist.RT.FT, Dist.SFP.Binary, Sum.Win.Hisp.C
vars.balance = ana.dist %>% dplyr::filter(SY == "2001-02") %>%
  dplyr::select(NCESDist, CEN.Dissim, Prop.Hisp, Dist.RT.FT, Dist.SFP.Binary, Sum.Win.Hisp.C,
                include, switcher, Dist.Stu.Hisp, Exp.Total.Scaled)

vars.balance = left_join(vars.balance, pscores[,1:2], by = "NCESDist")
vars.balance = left_join(vars.balance, pscores.2012, by = "NCESDist")
vars.balance = left_join(vars.balance, check[,c("NCESDist", "treat.time")], by = "NCESDist")
vars.balance = vars.balance[vars.balance[["include"]]==1,]

# Relvant units
vars.balance = vars.balance[
  vars.balance$treat.time %in% c(13, 16),
]
vars.balance$switcher = ifelse(vars.balance$treat.time == 13, 1, 0)

# Variable Balance: Longer -------------------------------------------------------------

# Formula
fmla.balance <- formula(paste("switcher ~ 0 + ",
                              paste(covariates.balance, 
                                    collapse="+")))

XX <- model.matrix.lm(fmla.balance, vars.balance, na.action=NULL)
W <- vars.balance[,"switcher"]
pp <- ncol(XX)

# Crump trim literal zeros
vars.balance2 = vars.balance[vars.balance$pscore.2012!=0,]

# Unadjusted covariate means, variances and standardized abs mean differences
means.treat <- apply(XX[W == 1,], 2, mean, na.rm=TRUE)
means.ctrl <- apply(XX[W == 0,], 2, mean, na.rm=TRUE)
abs.mean.diff <- abs(means.treat - means.ctrl)

var.treat <- apply(XX[W == 1,], 2, var, na.rm=TRUE)
var.ctrl <- apply(XX[W == 0,], 2, var, na.rm=TRUE)
std <- sqrt(var.treat + var.ctrl)

# Adjusted
XX.treat <- as.matrix(as.data.frame(apply(XX,2, function(x) x*W/vars.balance2$pscore.2012)))
XX.control <- as.matrix(as.data.frame(apply(XX,2, function(x) x*(1-W)/(1-vars.balance2$pscore.2012))))
means.treat.adj <- apply(XX.treat, 2, mean, na.rm=TRUE)
means.ctrl.adj <- apply(XX.control, 2, mean, na.rm=TRUE)
abs.mean.diff.adj <- abs(means.treat.adj - means.ctrl.adj)

var.treat.adj <- apply(XX.treat, 2, var, na.rm=TRUE)
var.ctrl.adj <- apply(XX.control, 2, var, na.rm=TRUE)
std.adj <- sqrt(var.treat.adj + var.ctrl.adj)

pdf(file = "../../output/pscore_plots/balance_pscore.pdf", width = 8.6, height = 6)
par(oma=c(0,4,0,0))
plot(-2, xaxt="n", yaxt="n", xlab="", ylab="", xlim=c(-.01, 1.5), ylim=c(0, pp+1), 
     main="Propensity Score Balance Comparison: 2012 Treated Cohort",
     sub="Standardized Absolute Mean Differences: 2001-02 School Year")
abline(v = 0, col = "black", lwd = 1, lty = 1)
abline(h = seq(1, 6, by=1), lty = 2, col = "grey", lwd=.5)
legend("topright", legend = c("Unadjusted", "Propensity Weighted (After Trimming)"), col=c("red", "blue"), pch=c(16, 19), bty='n', cex=0.8)
axis(side=1, at=c(-1, 0, 0.1, 0.5, 0.75, 1, 1.25, 1.5), las=1)
lines(abs.mean.diff / std, seq(1, pp), type="p", col="red", pch=19, cex = 1)
lines(abs.mean.diff.adj / std.adj, seq(1, pp), type="p", col="blue", pch=19, cex = 0.8)
mtext(c("Dissmilarity Index", "(Adult) Proportion Hispanic", "Binary SFP Indicator", "(FT) Student-Teacher Ratio", "Hispanic Student Pop", "Expenditure Per Student")
, side=2, cex=0.7, at=1:pp, padj=.4, adj=1, col="black", las=1, line=.3)
dev.off()

# Return -----------------------------------------------------------------------
pscores <- 
  pscores %>%
  summarise(
    .by = "NCESDist",
    pscore = first(pscore.forced),
    treat.time = first(treat.time)
  )

write.csv(
  pscores,
  "../../data/output/analysis/marginal_pscores.R"
)
