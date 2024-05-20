# Script Target: Bounds on PSDE direct effects, using new approach
# Even though we have ~ no treated units

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
library(grf)
library(rpart)

# Step 0: Load Data ------------------------------------------------------------
# write.csv struggles with long files
csv.fix <- function(df){
  df = df[,-"V1"]
  df = df[!(duplicated(df))]
  return(df)
}

df.pscore = fread("../../data/output/analysis/2012_crossec_pscores.csv")
df.dist = fread("../../data/output/analysis/district_regressions.csv")
cen.dist = fread("../../data/output/analysis/census_district.csv")
df.pscore = df.pscore[,-"V1"]
cen.dist = csv.fix(cen.dist)

df.dist <- 
  left_join(
    df.dist,
    df.pscore,
    by = "NCESDist"
  )


# Step 1: Get the correct dataframe & assign mediator ---------------------------

# Units with elections in 2012, comparing treated to not-yet-treated units
# i.e. before-treated units being used here... no need to throw that info
# out really?

df.dist = df.dist[df.dist$include == 1]
df.dist.2012 = df.dist[df.dist$SY == "2012-13", ]
df.dist.2012 = df.dist.2012[!is.na(df.dist.2012$Num.Elected),]
df.dist.2012 = df.dist.2012[df.dist.2012$include == 1,]

# Trim for 5% pscores & some units that are just out of bounds really
df.dist.2012 = df.dist.2012[df.dist.2012$pscore.2012.csec > 0.05 & 
                            df.dist.2012$pscore.2012.csec < 0.20]

# Plot propensity scores
ggplot(df.dist.2012, aes(pscore.2012.csec, fill = as.factor(trustee))) + 
  geom_histogram(col = "black") + 
  theme_classic() + 
  theme(legend.position = c(0.9,0.9)) + 
  labs(fill = "Treatment Indicator",
       title = "Overlap For 2012 Treated Cohort",
       subtitle = "After Propensity Score Trimming",
       x = "Cumulative Propensity Score",
       y = "Count")

print(paste0("N = ", nrow(df.dist.2012)))
print(paste0("Treated = ", sum(df.dist.2012$trustee ==1)))

# Assign Mediator Status
df.dist.2012$Mediator = ifelse(
  df.dist.2012$Sum.Win.Hisp.C > 0.9, 
  1, 
  0
)

# Step 2: Get the balancing covariates -----------------------------------------
df.mediator.model = 
  left_join(
    cen.dist,
    dplyr::select(df.dist.2012, NCESDist, Mediator),
    by = "NCESDist"
  ) %>%
  dplyr::filter(
    !is.na(Mediator)
  )

# Step 3: Debiased calculate theta_0 -------------------------------------------

### Step 3.0: Assign Folds For Cross Fitting
nfolds = 10
df.mediator.model$fold = sample(seq(nrow(df.mediator.model)) %% nfolds + 1)


### Step 3.1: Random forest mediator model conditional on covariates
X = as.matrix(
  dplyr::select(
    df.mediator.model,
    -NCESDist,
    -CEN.Dissim,
    -Mediator)
)

M = df.mediator.model$Mediator

# Train and fit out of fold
predictions = rep(NA, nrow(df.mediator.model))

for(fold in unique(df.mediator.model$fold)){
  
  # Relevant Rows
  idx = which(df.mediator.model$fold == fold)
  
  # Train out of fold
  forest.M <- regression_forest(
    X[-idx,],
    M[-idx],
    num.trees = 1000
  )
  
  # Fitteed values in fold
  fitted = predict(forest.M, X[idx,])
  
  # Assign
  for(i in 1:7){
    predictions[idx[i]] = fitted[i,]
  }

}

### Step 3.2: Orthogonalize

# Fitted values
M.hat = unlist(predictions)
W.hat = df.dist.2012$pscore.2012.csec

# Orthogonalised scores
M.orth = df.dist.2012$Mediator - M.hat
W.orth = df.dist.2012$trustee - W.hat

### Step 3.3: Regression 

theta.hat =  lm(M.orth~W.orth)[[1]][2]
print((paste0("Estimate of Theta 0: ", theta.hat)))

# Step 4: Calculate the conditional trimming bound function---------------------

### Step 4.1: Estimate E(D|W=0, X)

# Prep data
controls = unique(df.dist.2012[df.dist.2012$trustee ==0,]$NCESDist)
df.mediator.model.c = df.mediator.model[
  df.mediator.model$NCESDist %in% controls
]
df.mediator.model.c = df.mediator.model.c[,-"fold"]

# K fold assignment
nfolds = 10
df.mediator.model.c$fold = sample(seq(nrow(df.mediator.model.c)) %% nfolds + 1)

# Fit out of fold; assign in fold
scores.selection = rep(NA, nrow(df.mediator.model.c))

for(i in unique(df.mediator.model.c$fold)){
  
  # Units within fold
  idx = which(df.mediator.model.c$fold == i)
  
  # Relevant Data
  X = dplyr::select(df.mediator.model.c, -NCESDist, -CEN.Dissim, -Mediator)
  D = df.mediator.model.c$Mediator
  
  # Train out of fold
  forest.M.c <- regression_forest(
    X[-idx,],
    D[-idx],
    num.trees = 1000
  )
  
  # Fit in fold
  fits = predict(forest.M.c, X[idx,])
  
  # Assign
  for(i in 1:7){
    scores.selection[idx[i]] = fitted[i,]
  }
  
}

# For treated units, assign using final model
df.mediator.model.t = df.mediator.model[
  !df.mediator.model$NCESDist %in% controls
]

fits = predict(forest.M.c, 
               df.mediator.model.t[,-c("NCESDist", "CEN.Dissim", "Mediator")])

# Use to construct q(x) for each unit!
qx.controls = cbind(df.mediator.model.c$NCESDist,
                    1 - (theta.hat/scores.selection),
                    1 - scores.selection - theta.hat)
qx.treat = cbind(df.mediator.model.t$NCESDist,
                 1 - (theta.hat/fits),
                 1 - fits - theta.hat)

colnames(qx.controls) = c("NCESDist", "Qx", "Q1x")
colnames(qx.treat) = c("NCESDist", "Qx", "Q1x")
qx.controls = as.data.frame(qx.controls)
qx.treat = as.data.frame(qx.treat)

# Assign back to main data...
df.dist.2012 = left_join(df.dist.2012, qx.controls, by = "NCESDist")
df.dist.2012 = left_join(df.dist.2012, qx.treat, by = "NCESDist")
df.dist.2012$Qx = ifelse(is.na(df.dist.2012$Qx.x), 
                         df.dist.2012$Qx.y,
                         df.dist.2012$Qx.x)
df.dist.2012$Q1x = ifelse(is.na(df.dist.2012$Q1x.x), 
                         df.dist.2012$Q1x.y,
                         df.dist.2012$Q1x.x)
df.dist.2012 = df.dist.2012[,-c("Qx.x", "Qx.y", "Q1x.x", "Q1x.y")]

# Step 5: Estimate Quantiles & Return Bounds w/ HT ATE Est (Function) ----------

# Give it a dataframe with the X variables, Qx, Q1x, Mediator coding, outcome,
# and propensity score. 
# It will return an estimate for the Hajek ATE with bootstrapped CIs
# And the PSDE bounds after fitting the nuisance parameters

est.psde.hajek <- function(df){
  
  ### First: Hajek ATE ###
    
    # Inverted Pscore Weights
    df$IPW = ifelse(df$trustee == 1, 
                    df$pscore.2012.csec,
                    1 - df$pscore.2012.csec)
    
    # Basic quantities
    df.t = df[df$trustee == 1,]
    df.c = df[df$trustee == 0,]
    denom.t = mean(df.t$trustee/df.t$IPW)
    denom.c = mean((1 - df.c$trustee)/df.c$IPW)
  
    # Main estimate
    hajek = 
    mean((df.t[["outcome"]] / df.t$IPW) / denom.t, na.rm = T) - 
    mean((df.c[["outcome"]] / df.c$IPW) / denom.c, na.rm = T)
    
    # Bootstrapped CIs
    ests = rep(NA, 100)
    for(i in 1:100){
      a = sample_n(df, nrow(df), replace = T)
      a.t = a[a$trustee == 1,]
      a.c = a[a$trustee == 0,]
      denom.t.bs = mean(a.t$trustee/a.t$IPW)
      denom.c.bs = mean((1 - a.c$trustee)/a.c$IPW)
      
      ests[i] = mean((a.t[["outcome"]] / a.t$IPW) / denom.t.bs, na.rm = T) - 
        mean((a.c[["outcome"]] / a.c$IPW) / denom.c.bs, na.rm = T)
    }
    
    hajek.upper = max(ests, na.rm = T)
    hajek.lower = min(ests, na.rm = T)
    
  ### Second: Gamma function ###
  
    # Relevant units
    in.gamma = df.t[df.t$Mediator == 0,]
    
    # This is on three units; there is no meaningful way to do this well
    # So just gonna go with our best; effectively is taking the closest unit 
    # from the set of three... just gonna train on all of them for the other 
    # units and go leave-one-out for the set in here...
    fit.t0 = as.data.frame(in.gamma$NCESDist)
    fit.t0$fit = NA
    colnames(fit.t0) = c("NCESDist", "GammaX")

    for(i in 1:nrow(in.gamma)){
      X = as.matrix(in.gamma[,1:318])
      Y = in.gamma$outcome
      model = regression_forest(X[-i,], Y[-i], num.trees = 1000, honesty = F,
                                sample.fraction = 1,
                                ci.group.size = 1)
      fit.t0[i,2] = predict(model, as.data.frame(t((X[i,]))))
    }
  
    # For the rest of the data
    model.all = regression_forest(X, Y, num.trees = 1000, honesty = F,
                                  sample.fraction = 1,
                                  ci.group.size = 1)
    
    in.gamma.all = df[!(df$NCESDist %in% in.gamma$NCESDist),]
    X = as.matrix(in.gamma.all[,1:318])
    fit.all = as.data.frame(in.gamma.all$NCESDist)
    fit.all$fit = unlist(predict(model.all, X))
    colnames(fit.all) = c("NCESDist", "GammaX")
    
    # Together - notice that in practice we are just assigning a mean!
    # And we should say that in the presentation!
    # All the action happening on the Delta function
    fits = rbind(fit.all, fit.t0)
    df = left_join(df, fits, by = "NCESDist")

  ### Third: Quantiles ###
    
    # This part needs some thinking because we need the fit for each unit! 
    # Might make most sense to just to leave-one-out?
    # And then do whole sample to fit the remaining units?
    in.delta = df.c[df.c$Mediator ==0,]
    
    # Five-fold cross-fitting
    nfolds = 5
    in.delta$fold = sample(seq(nrow(in.delta)) %% nfolds + 1)
    X = as.matrix(in.delta[,1:318])
    Y = in.delta$outcome
    
    fit.c0 = as.data.frame(in.delta$NCESDist)
    fit.c0$fit = NA
    fit.c0$fit2 = NA
    fit.c0$type = "C0"
    colnames(fit.c0) = c("NCESDist", "YLx", "YUx", "Type")
    
    for(i in 1:5){
      idx = which(in.delta$fold == i)
      model = quantile_forest(X[-idx,], Y[-idx], num.trees = 1000)
      
      # For each fold unit, need correct prediction
      for(j in 1:length(idx)){
        q = in.delta[idx[j],]$Qx
        fit.c0[idx[j],2] = predict(
          model,
          as.data.frame(t(X[idx[j],])),
          quantiles = 1-q
        )
        fit.c0[idx[j],3] = predict(
          model,
          as.data.frame(t(X[idx[j],])),
          quantiles = q
        )
      }
    }
    
    # Now for the remaining units, can train on all of the 00 data
    # Build data
    model = quantile_forest(X, Y, num.trees = 1000)
    in.delta.all = df[!(df$NCESDist %in% in.delta$NCESDist)] 
    X = as.matrix(in.delta.all[,1:318])
    Y = in.delta.all$outcome
    fit.all = as.data.frame(in.delta.all$NCESDist)
    fit.all$fit = NA
    fit.all$fit2 = NA
    fit.all$type = "N"
    colnames(fit.all) = c("NCESDist", "YLx", "YUx", "Type")
    
    # Loop
    for(j in 1:nrow(in.delta.all)){
      q = in.delta.all[j,]$Qx
      fit.all[j,2] = predict(
        model,
        as.data.frame(t(X[j,])),
        quantiles = 1-q
      )
      fit.all[j,3] = predict(
        model,
        as.data.frame(t(X[j,])),
        quantiles = q
      )
    }
    
    # Append
    fits = rbind(fit.c0, fit.all)

  ### Fourth: Delta Functions ###
  
  fits$DeltaL = NA
  fits$DeltaU = NA
  for(j in 1:nrow(fits)){
    
    # Access elements with [[]]
    row = fits[j,]
    
    # For C0 units, make sure you're fitting out of fold
    if(row[[4]] == "C0"){
      
      # Data from delta matrices; relevant subsets
      X = as.matrix(in.delta[,1:318])
      Y = in.delta$outcome
      subset.L = Y > row[[2]]
      subset.U = Y < row[[3]]
      
      # Find the relevant fold
      fold = in.delta[in.delta$NCESDist == row[[1]],]$fold
      idx = in.delta$fold == fold
      
      # Train models on the out-of-fold data
      model.L = regression_forest(X[subset.L & !idx,],
                                  Y[subset.L & !idx],
                                  num.trees = 1000)
      model.U = regression_forest(X[subset.U & !idx,],
                                  Y[subset.U & !idx],
                                  num.trees = 1000)
      
      # Fit in-fold
      fits[j,]$DeltaL = predict(
        model.L,
        as.data.frame(t(X[j,]))
      )
      fits[j,]$DeltaU = predict(
        model.U,
        as.data.frame(t(X[j,]))
      )
    }
    
    # For other units, fit with all the data
    else{
    
      # Start with training data from all of in.delta
      X = as.matrix(in.delta[,1:318])
      Y = in.delta$outcome

      subset.L = Y > row[[2]]
      subset.U = Y < row[[3]]
        
      # Train the models
      model.L = regression_forest(X[subset.L,],
                                  Y[subset.L],
                                  num.trees = 1000)
      model.U = regression_forest(X[subset.U,],
                                  Y[subset.U],
                                  num.trees = 1000)
      
      # Now fit
      fits[j,]$DeltaL = predict(
        model.L,
        as.data.frame(df[df$NCESDist == row[[1]],1:318])
      )
      fits[j,]$DeltaU = predict(
        model.U,
        as.data.frame(df[df$NCESDist == row[[1]],1:318])
      )
    }
  }
    
  ### Fifth: Integrate ###
  integrand.U = df$GammaX - as.numeric(fits$DeltaU)
  integrand.L = df$GammaX - as.numeric(fits$DeltaL)
  denom = mean(df$Q1x)
  
  U = mean(integrand.U * df$Q1x) / denom
  L = mean(integrand.L * df$Q1x) / denom
  
  ### Sixth: Output ###
  bounds = c(L, U, NA)
  hajek = c(hajek.lower, hajek.upper, hajek)
  output = as.data.frame(rbind(hajek, bounds))
  colnames(output) = c("Lower", "Upper", "Centre")
  output$Type = c("Hajek", "PSDE(0)")
  rownames(output) = NULL
  return(output) 
}


# Step 6: Apply function for different outcomes; plot --------------------------


# Basic Data
data.analysis = dplyr::select(
  df.dist.2012,
  NCESDist,
  pscore.2012.csec,
  trustee,
  Qx,
  Q1x
)

data.analysis = left_join(
  df.mediator.model[,-c("CEN.Dissim", "fold")],
  data.analysis,
  by = "NCESDist"
)

# Function: Pick an outcome and some years,
# It loops through the years, getting the outcome, running the estimation
# and outputting a dataframe for plotting. 
bound.outcome = function(output.name, vector.years){
  
  for(year.name in vector.years){
    
    df = data.analysis
    out.join = as.data.frame(cbind(
      df.dist[df.dist$SY==year.name & 
              df.dist$NCESDist %in% data.analysis$NCESDist,][[output.name]],
      df.dist[df.dist$SY==year.name & 
                df.dist$NCESDist %in% data.analysis$NCESDist,][["NCESDist"]]))
    colnames(out.join) = c("outcome", "NCESDist")
    
    df = left_join(df, out.join, by = "NCESDist")
    result = est.psde.hajek(df)
    result$SY = year.name
    
    if(which(vector.years == year.name) == 1){
      output = result
    }
    else{
      output = rbind(output, result)
    }

  }
  
  return(output)

}

# Step 7: Plot -----------------------------------------------------------------
choice.outcome = "Dist.RT.EL.FT"
choice.years = c("2007-08", "2008-09", "2012-13", "2013-14", "2014-15", "2015-16")
choice.title = "ELD Teacher-Student Ratio"

choice.vline = which(choice.years == "2012-13") - 0.5
df = bound.outcome(choice.outcome, choice.years)

pdf("../../output/psde_bounds/psde_v2.pdf", width = 8.6, height = 5.3)
ggplot(df, aes(SY, Centre, col = Type)) +
  geom_errorbar(aes(ymin = Lower,
                    ymax = Upper),
                position = position_dodge(0.2), 
                width = 0.1) +
  theme_classic()+
  geom_point(aes(col = Type), position = position_dodge(0.2)) + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") + 
  geom_vline(xintercept = choice.vline, linetype = "dashed", color = "grey") + 
  scale_color_manual(values = c("#00AFBB", "#FC4E07")) + 
  labs(col = "Estimation",
       title = paste0(choice.title, " in 2012 Treatment Cohort"),
       subtitle = "Hajek ATE Estimator vs. Observational Bounds on PSDE for Never-Takers",
       y = "Estimate",
       x = "",
       caption = "The chart presents estimates for the 2012 ward election treatment cohort. Data on teacher qualifications are unavailable <br> between the 2010-11 and 2012-13 school years, hence the missing years. Hajek estimation is a stabilized IPW estimator <br> for the ATE given propensity score estimates and standard errors presented are bootstrapped without re-estimation of the <br> propensity score i.e. are slightly too narrow. Bounds on the direct effect for never-takers are derived in the distributed memo <br> and rely on observable data, selection-on-observables, and a monotonicity assumption. Years before 2012 presented as <br> placebo checks. Gamma estimate: 0.099.") + 
  theme(plot.caption = element_markdown(hjust=0))
dev.off()

