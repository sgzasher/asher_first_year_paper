# Script Target: SFP spending data from Fischer. Literally just trying to 
# read it in for now; later will try and turn into a school-level panel
# and even later than that construct into district-level equalisation outcomes. 
# Will clean up a little bit and drag in the school-level identifiers from the 
# frame for this particular script. 

# Packages ---------------------------------------------------------------------
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
set.seed(1966)
library(dplyr)
library(data.table)
library(haven)
library(purrr)
library(stringr)
library(parallel)
library(doParallel)
library(stringdist)

# Read School Frame ------------------------------------------------------------
df.frame = fread("../../data/output/frames/frame_school.csv")
df.sfp = fread("../../data/raw/fischer_sfp/sfp_data.csv",
               header = T)

# Step 1: Match on (Inferred) CDS Code -----------------------------------------

# Row number to trace back later...
df.sfp$rownum = seq.int(nrow(df.sfp)) 

# SFP inferred CDS
df.sfp$CDSCode <- 
  str_extract(
    df.sfp$school_name,
    "(?<=\\()\\d+(?=\\))"
  )

# Frame CDE
df.frame <- 
  df.frame %>%
  dplyr::mutate(
    CDSCode = as.character(CDSCode)
  )

# Match directly with grepl
match.cds <- 
  mclapply(
    df.sfp$CDSCode,
    FUN = function(x){str_detect(df.frame$CDSCode, paste0(x, "$"))}
  )

# Set of schools that did not match on this step; step that nailed it
s1.zero = df.sfp[as.vector(sapply(match.cds, FUN = sum) == 0),]
s1.single = df.sfp[as.vector(sapply(match.cds, FUN = sum) == 1),]

# Step 2a: Fuzzy Matching Functions --------------------------------------------

# Function for fuzzy string matching
fzzy.fst <- function(choice.function, choice.max, df1, df2, choice.variables){
  
  v1 = getElement(df1, choice.variables[1])
  v2 = getElement(df2, choice.variables[2])
  
  returnme <-
    mclapply(
      v1,
      FUN = function(x){
        stringdist(x, v2, method = choice.function) < choice.max
      }
    )
  
  return(returnme)
}

# Standardization Script
clean_strings <- function(vector.string){
  vs = vector.string
  vs <- paste(str_extract_all(vs, '[:alnum:]*'))
  vs <- 
    vs %>%
    str_replace_all("[:punct:]", "") %>%
    str_replace_all("\"", "") %>%
    str_replace_all(",", "") %>%
    str_replace_all("^c", "") %>%
    str_replace_all(" ", "") %>%
    str_replace_all("Elementary", "") %>%
    str_replace_all("Middle", "") %>%
    str_replace_all("District", "") %>%
    str_replace_all("School", "") %>%
    str_replace_all("High", "") %>%
    str_replace_all("Unified", "") %>%
    toupper()
  return(vs)
}

# Full fuzzy application script 
fzzy.run <- function(c.fun, c.max, c.max.school, df.1, df.2){
  
  out.school <- 
    fzzy.fst(
      choice.function = c.fun,
      choice.max = c.max.school, 
      df1 = df.1, 
      df2 = df.2, 
      choice.variables = c("Stand_School", "Stand_School"))
  
  out.dist <- 
    fzzy.fst(
      choice.function = c.fun,
      choice.max = c.max, 
      df1 = df.1, 
      df2 = df.2, 
      choice.variables = c("Stand_District", "Stand_District"))
  
  out.count <- 
    fzzy.fst(
      choice.function = c.fun,
      choice.max = c.max, 
      df1 = df.1, 
      df2 = df.2, 
      choice.variables = c("Stand_County", "Stand_County"))
  
  output <- 
    mclapply(
      X = 1:nrow(df.1),
      FUN = function(x){out.school[[x]] & out.dist[[x]] & out.count[[x]]}
    )
  
  return(output)
  
}

# Step 2b: Fuzzy Matching Application ------------------------------------------

# Remove CDS codes
s1.zero <- 
  s1.zero %>%
  dplyr::mutate(
    StandSchName = 
      gsub("\\s*\\(\\d+\\)$", "", school_name)
  )

# Standardize SFP
s1.zero$Stand_School = clean_strings(s1.zero$StandSchName)
s1.zero$Stand_County = clean_strings(s1.zero$county_name)
s1.zero$Stand_District = clean_strings(s1.zero$district_name)

# Standardize frame
df.frame$Stand_School = clean_strings(df.frame$School)
df.frame$Stand_County = clean_strings(df.frame$County)
df.frame$Stand_District = clean_strings(df.frame$District)

#  Fuzzy merge
match.s2 = fzzy.run(
  c.fun = "jw",
  c.max = 0.1,
  c.max.school = 0.3,
  df.1 = s1.zero,
  df.2 = df.frame
)
# New set of nailed it
s2.many = s1.zero[as.vector(sapply(match.s2, FUN = sum) > 1),]
s2.single = s1.zero[as.vector(sapply(match.s2, FUN = sum) == 1),]

# Step 2c: Fuzzy Matching Diambiguation ----------------------------------------

# Note: previously, expanded for the set not matching on CDS. Now, narrow
# on the set that matched for many school names

## Round One! At 20%
match.s2.20 = fzzy.run(
  c.fun = "jw",
  c.max = 0.1,
  c.max.school = 0.2,
  df.1 = s2.many,
  df.2 = df.frame
)

# New set of nailed it
s2.20.single = s2.many[as.vector(sapply(match.s2.20, FUN = sum) == 1),]
s2.20.many = s2.many[as.vector(sapply(match.s2.20, FUN = sum) > 1),]

## Round Two! At 10%
match.s2.10 = fzzy.run(
  c.fun = "jw",
  c.max = 0.1,
  c.max.school = 0.1,
  df.1 = s2.20.many,
  df.2 = df.frame
)

# Final set of nailed it
s2.10.single = s2.20.many[as.vector(sapply(match.s2.10, FUN = sum) == 1),]

# SFP Matched Set --------------------------------------------------------------

# Function to quickly return frame CDS codes corresponding to lists of logical
# match vectors from merge steps

match.codes <- function(input.df, input.list, frame.df){
  
  codes <-
  mclapply(
    1:nrow(input.df),
    FUN = function(x){
      ifelse(
        str_detect(
          as.character(frame.df[input.list[[x]], "CDSCode"]), "c\\("),
        "Many",
        ifelse(
          str_detect(
            as.character(frame.df[input.list[[x]], "CDSCode"]), "char"),
            "None",
            as.character(frame.df[input.list[[x]], "CDSCode"])
        )
      )
    }
  )
    
  return(codes)
}

# Apply matched codes
df.sfp$CDSCode = match.codes(df.sfp, match.cds, df.frame)
s1.zero$CDSCode = match.codes(s1.zero, match.s2, df.frame)
s2.many$CDSCode = match.codes(s2.many, match.s2.20, df.frame)
s2.20.many$CDSCode = match.codes(s2.20.many, match.s2.10, df.frame)

# Concatenate correctly...
df.matched <- 
  bind_rows(
    df.sfp[!(df.sfp$CDSCode %in% c("None", "Many")),],
    s1.zero[!(s1.zero$CDSCode %in% c("None", "Many")),],
    s2.many[!(s2.many$CDSCode %in% c("None", "Many")),],
    s2.20.many[!(s2.20.many$CDSCode %in% c("None", "Many")),]
  ) %>%
  dplyr::select(
    rownum, CDSCode
  )

# Output Data ------------------------------------------------------------------

df.sfp <- 
  df.sfp %>%
  dplyr::select(
    rownum, school_name, status, 
    project_type, fund_total, date_full_grant
  ) 

df.output <- 
  left_join(
    df.sfp, 
    df.matched,
    by = "rownum"
  ) %>%
  dplyr::filter(
    str_detect(project_type, "Modern")
  ) %>% 
  dplyr::select(
    -rownum
  ) %>%
  dplyr::mutate(
    CDSCode = 
      unlist(ifelse(CDSCode == "NULL", "", CDSCode))
  )

write.csv(
  df.output,
  "../../data/output/built/sfp_spending.csv"
)
