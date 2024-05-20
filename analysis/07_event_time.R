# Script Target: Event-Time Panel for PSDE Bounds

# Packages ---------------------------------------------------------------------
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
set.seed(1966)
library(dplyr)
library(data.table)
library(stringr)
library(DescTools)
library(fixest)

# Read -------------------------------------------------------------------------
# write.csv struggles with long files
csv.fix <- function(df){
  df = df[,-"V1"]
  df = df[!(duplicated(df))]
  return(df)
}

# Read and fix
df.school = fread("../../data/output/analysis/school_regressions.csv")
df.dist = fread("../../data/output/analysis/district_regressions.csv")
psc.dist = fread("../../data/output/analysis/marginal_pscores.R")
df.school = csv.fix(df.school)
df.dist = csv.fix(df.dist)
df.dist = df.dist[df.dist$include == 1,]

# Treated Units Time of Next Election ------------------------------------------
df.dist.t = df.dist[df.dist$switcher == 1,]

# Function for first appearance of function
first.year <- function(df, var.name){
  df.k = df[!is.na(df[[var.name]]),]
  out.year = min(df.k$year, na.rm = T)
  return(out.year)
}

# Function for mediator status
assign.mediator <- function(df, unit.name, year.name){
  
  status = df[df$NCESDist==unit.name & year == year.name,]$Sum.Win.Hisp.C
  return(status)
  
}

# List of unique districts, matrix for output
dist.list = unique(df.dist.t$NCESDist)
dist.matrix = matrix(data = NA, nrow = 25, ncol = 4)
colnames(dist.matrix) = c("NCESDist", "Year.Switch", "Year.T.Elec", "Mediator")


# Mediator status is mediator status in first election-year after treatment
for(x in dist.list){
  
  idx = which(dist.list == x)
  z = first.year(df.dist.t[df.dist.t$NCESDist == x & df.dist.t$trustee==1,], 
                 "NCESDist")
  y = first.year(df.dist.t[df.dist.t$NCESDist == x & df.dist.t$trustee==1,], 
                 "Prop.Win.Hisp.B")
  m = assign.mediator(df.dist.t, x, y)
  m = ifelse(length(m) > 0, m, NA)
  
  dist.matrix[idx, 1] = x
  dist.matrix[idx,2] = z
  dist.matrix[idx,3] = y
  dist.matrix[idx,4] = m
}

# Format, mediator to binary
dist.matrix = as.data.frame(dist.matrix)
dist.matrix$Mediator = ifelse(dist.matrix$Mediator > 0.9, 1, 0)


# Create Treated-Unit ----------------------------------------------------------

# Assign mediator based on first election year; take outcomes at lags and leads
# in event-time
\