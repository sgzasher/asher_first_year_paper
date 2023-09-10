# Script Target: Cleaning the CDD data (school & district level) to construct 
# a school-levle dataset (with some district-level controls). A later script 
# will construct a district-level dataset. 


# Packages ---------------------------------------------------------------------
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
set.seed(1966)
library(dplyr)
library(data.table)
library(stringr)
library(DescTools)

# Read in file -----------------------------------------------------------------
df.school <- fread("../../data/output/built/cdd_controls.csv")
df.district <- fread("../../data/output/built/cdd_controls_district.csv")

# School Level ------------------------------------------------------------------

# Different codes for different reasons for missingness
df.school <- 
  df.school %>%
  mutate(across(where(is.numeric), ~na_if(., -2))) %>%
  mutate(across(where(is.numeric), ~na_if(., -9))) %>%
  mutate(across(where(is.numeric), ~na_if(., -1))) %>%
  mutate(Title.I.School = case_match(Title.I.School,
                                     "1" ~ "Yes", "2" ~ "No",
                                     "0" ~ NA, "M" ~ NA, 
                                     "Missing" ~ NA, 
                                     "N" ~ NA, 
                                     "Not Applicable" ~ NA)) %>%
  mutate(across(where(is.character), ~na_if(., ""))) %>%
  mutate(Title.I = case_match(Title.I,
                              "1" ~ "Yes", "2" ~ "No",
                              "0" ~ NA, "M" ~ NA, 
                              "Missing" ~ NA, 
                              "N" ~ NA, 
                              "Not Applicable" ~ NA))

### Building Magazinnik variables... 

# Total enrollment
df.school$Stu.Count.Reported = df.school$Stu.Count.Reported

# Prop black, white, asian american or pacific islander (constructed)
df.school$Stu.Prop.White = df.school$Stu.White / df.school$Stu.Count.Reported
df.school$Stu.Prop.Black = df.school$Stu.Black / df.school$Stu.Count.Reported
df.school$Stu.AAPI = ifelse(
  is.na(df.school$Stu.AAPI),
  df.school$Stu.Asian + df.school$Stu.Pacific,
  df.school$Stu.AAPI
)
df.school$Stu.Prop.AAPI = df.school$Stu.AAPI / df.school$Stu.Count.Reported
df.school$Stu.Prop.Hisp = df.school$Stu.Hisp / df.school$Stu.Count.Reported

# Prop receiving free school meals
df.school$Stu.Prop.FRSM = df.school$TOT.FRSM / df.school$Stu.Count.Reported

# My own variable: teacher student!
# Be super cautios using this because it is INCREDIBLY weird... 
# I guess scaling by FTE is a lot of the issue?
df.school$FTE.Stu.Ratio = df.school$Stu.Count.Reported / df.school$FTE

# District Dissimilarity Index -------------------------------------------------

# Total students white and hispanic
df.dist.totals <- 
  df.school %>%
  summarise(
    .by = c("NCESDist", "SY"),
    Dist.Stu.Hisp = sum(Stu.Hisp, na.rm = T),
    Dist.Stu.White = sum(Stu.White, na.rm = T),
    Dist.Total = sum(Stu.Count.Reported, na.rm = T)
  )

# Back to school data
df.school <- 
  left_join(
    df.school,
    df.dist.totals, 
    by = c("NCESDist", "SY")
  )

# Index summand
df.school <- 
  mutate(
    df.school,
    summand = abs((Stu.White / Dist.Stu.White) - (Stu.Hisp/ Dist.Stu.Hisp))
  )

# Function to calculate index
func.dissim <- function(vector){
  return(
    0.5 * sum(vector, na.rm = T)
  )
}

# Index! That was nice and easy! Distribution looks correct
df.dist.dissim <- 
  df.school %>%
  summarise(
    .by = c("NCESDist","SY"), 
    Dist.Dissim = func.dissim(summand)
  )

# District Level ---------------------------------------------------------------

# Missingness codes 
df.district <- 
  df.district %>%
  mutate(across(where(is.numeric), ~na_if(., -2))) %>%
  mutate(across(where(is.numeric), ~na_if(., -9))) %>%
  mutate(across(where(is.numeric), ~na_if(., -1)))

# Fix leading zero in df.district...
df.district <- 
  dplyr::mutate(
    df.district,
    NCESDist = as.integer(str_sub(NCESDist, 2, 8))
  )

# Get total enrollments
df.district <-
  left_join(
    df.district,
    df.dist.totals,
    by = c("NCESDist", "SY")
  )


### Magazinnik variables...

# Property taxes collected
df.district$Rev.Local.Property.Scaled = 
  as.numeric(df.district$Rev.Local.Property) / df.district$Dist.Total

# Total current spending on instruction; total educational revenues and exp
df.district$Exp.Instruction.Scaled = 
  as.numeric(df.district$Exp.Instruction) / df.district$Dist.Total

df.district$Exp.Total.Scaled = 
  as.numeric(df.district$Exp.Total) / df.district$Dist.Total

df.district$Rev.Total.Scaled = 
  as.numeric(df.district$Rev.Total) / df.district$Dist.Total

# Select Variables and Write ---------------------------------------------------
df.output <- 
  df.school %>%
  dplyr::select(
    NCESSchool, NCESDist, SY, FTE, Stu.Count.Reported, FTE.Stu.Ratio,
    Stu.Prop.White, Stu.Prop.Black, Stu.Prop.AAPI, Stu.Prop.Hisp,
    Stu.Prop.FRSM, Title.I, Dist.Total
  )

df.output <-
  left_join(df.output, df.dist.dissim, by = c("NCESDist", "SY"))

df.district <- 
  left_join(df.district, df.dist.dissim, by = c("NCESDist", "SY"))

write.csv(
  df.district,
  "../../data/output/analysis/cdd_analysis_district.csv"
)

df.district <- 
  dplyr::select(
    df.district, 
    NCESDist, SY, Rev.Local.Property.Scaled, Exp.Instruction.Scaled,
    Exp.Total.Scaled, Rev.Total.Scaled
  )

df.output <- 
  left_join(df.output, df.district, by = c("NCESDist", "SY"))

write.csv(
  df.output,
  "../../data/output/analysis/cdd_analysis.csv"
)


