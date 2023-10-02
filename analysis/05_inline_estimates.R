# Script Target: Estimation of in-line estimates from draft. 

# Packages ---------------------------------------------------------------------
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
set.seed(1966)
library(dplyr)
library(data.table)
library(stringr)
library(DescTools)
library(fixest)
library(stargazer)
library(modelsummary)

# Introduction -----------------------------------------------------------------

# In the 2001-02 school year, almost no (6%) Californian school boards were 
# elected across single-member districts.Through concerted efforts by civil 
# rights groups and activist organizations, this had risen to 21% by 2016-17.

df = fread("../../data/output/analysis/district_regressions.csv")
table(df$SY, df$trustee)
56/(56+837)
193/(193+712)

# School Board Policymaking ----------------------------------------------------

# Of the 919 school boards in my sample (see below), 574 meet the criteria 
# described by the LCCR and 131 switched to ward systems by 2017. Only 25 of 
# those were directly compelled by some kind of legal action.  

df.b = fread("../../data/output/frames/frame_district.csv")
df.2 = fread("../../data/output/analysis/district_regressions.csv")

df.2.check = df.2 %>%
  summarise(
    .by = "NCESDist",
    causal = first(causal, na.rm = T),
    include = first(include, na.rm = T),
    switcher = first(switcher, na.rm = T),
    switch.type = first(switch.type, na.rm = T),
    trustee = mean(trustee, na.rm = T)
  )

# 25 units with missing causal; all are excluded anyway
table(df.2.check$causal)

df.2.check.2 = df.2.check[df.2.check$causal==1,]
table(df.2.check.2$switch.type, df.2.check.2$include)


df.2 = df.2[df.2$SY == "2000-01",]
df.2 = df.2[df.2$NCESDist %in% df$NCESDist]
table(df.2$include, df.2$trustee)
df.3 = df.2[df.2$include == 1]

# How many schools in those boards?
df.s = fread("../../data/output/analysis/school_regressions.csv")
df.s = df.s[df.s$NCESDist %in% unique(df.3$NCESDist),]
length(unique(df.s$NCESSchool))

# Data -------------------------------------------------------------------------

#### Electoral ####
df = fread("../../data/output/analysis/election_outcomes_race.csv")
nrow(df)
table(df$ELEC.Ward)

# Ignore some cases where folk ran uncontested
sum(df$Margin==1, na.rm = T) / nrow(df)
df = df[df$Margin != 1,]
df$prop.cand = df$Sum.Ran.Hisp.C / df$Num.Candidates
df$prop.win =df$Sum.Win.Hisp.C / df$Num.Elected

table = df[,c("Num.Elected", "Num.Candidates", "Incumbents", 
              "VoteTotal", "Margin", "prop.cand", 
              "prop.win")]

stargazer(table, type = "latex", median = T, nobs = F,
          title = "Electoral Summary Statistics",
          covariate.labels = c(
            "Seats",
            "Candidates",
            "Incumbents",
            "Ballots Cast",
            "Margin",
            "Exp. Prop. Cand. Hispanic",
            "Exp. Prop. Winners Hispanic"
          ), 
          notes = c("\\textit{Note:} Summary statistics for 4,333 contested school board elections held between 2000",
                    "and 2017. Electoral margin reports the difference in vote shares between marginal", 
                    "winning and losing candidates."
                    ),
          notes.align = "l",
          out = "../../output/data_section/elec_desc.tex",
          label = "t::elec_desc",
          table.placement = "H")

#### SFP ####
df = fread("../../data/output/built/sfp_spending.csv")

# Plot of projets over time
dates = as.numeric(str_sub(df$date_full_grant, -4, -1))
sum(dates == 2016, na.rm = T)
sum(dates == 2002, na.rm = T)
pdf("../../output/data_section/sfp_time.pdf", width = 8.6, height = 6)
barplot(table(dates), 
        main = "SFP Projects Funded Over Time",
        ylab = "Number of SFP Projects")
dev.off()

#### Staff ####

# Include note that all indicators are binary. 
df = fread("../../data/output/built/fischer_staff.csv")
t = df[,-c("CDSCode", "TCH.RaceNA")]
stargazer(t, type = "latex", nobs = F, min.max = F,
          title = "Teacher Summary Statistics",
          covariate.labels = c(
          "Male",
          "AAPI",
          "Hispanic",
          "Black",
          "White",
          "Other Race",
          "Full Time Equivalent Units",
          "Fully Credentialed",
          "General Elementary Cert.",
          "General Secondary Cert.",
          "Math Authorization",
          "English Authorizaton",
          "English Language Development Auth."
          ),
          notes = c("\\textit{Note:} Summary statistics for 5,665,504 teacher-years between",
                    "2000 and 2017. All indicators are binary."),
          notes.align = "l",
          out = "../../output/data_section/teach_desc.tex",
          label = "t::teach_desc",
          table.placement = "H")


#### Demographic ####

## CCD

df = fread("../../data/output/built/cdd_controls.csv") 
df = dplyr::mutate(
  df,
  across(c("Stu.White", "Stu.Hisp", "Stu.Count.Reported", "TOT.FRSM"), function(x){ifelse(x < 0, 0, x)})
)

df.stu <- 
  df %>%
  summarise(
    .by = c("NCESDist", "SY"),
    Stu.White = sum(Stu.White, na.rm = T),
    Stu.Hisp = sum(Stu.Hisp, na.rm = T),
    Stu.Count = sum(Stu.Count.Reported, na.rm = T),
    Stu.FRSM = sum(TOT.FRSM, na.rm = T)
  )

df = fread("../../data/output/analysis/district_regressions.csv")
df = left_join(df, df.stu, by = c("NCESDist", "SY"))
t = df[,c("Rev.Total", "Rev.Local.Property", 
          "Exp.Total", "Exp.Instruction", "Stu.White", "Stu.Hisp", 
          "Stu.Count", "Stu.FRSM")]
t = t %>% mutate(across(c("Rev.Total", 
                          "Rev.Local.Property", 
                          "Exp.Total", 
                          "Exp.Instruction"),
                 round, 1))

stargazer(t, type = "latex", digits = 3, median = T, nobs = F, 
          digit.separator = "",
          title = "CCD Demographic and Financial Summary Statistics",
          covariate.labels = c(
            "Total Revenue",
            "Property Tax Revenue",
            "Total Expenditure",
            "Instruction Expenditure",
            "White Student Pop",
            "Hispanic Student Pop",
            "Total Student Pop",
            "FRSM Student Pop"
          ),
          notes = c("\\textit{Note:} Summary statistics from 15,468 district-years between 2000 and 2017. FRSM",
                    "refers to Free and Reduced School Meals. Data is at block-group level and joined",
                    "to schools spatially."),
          notes.align = "l",
          out = "../../output/data_section/demo_ccd.tex",
          label = "t::demo_ccd",
          min.max = F,
          table.placement = "H")



## Census
df = fread("../../data/output/analysis/census_district.csv")
df$Prop.Hisp = df$FXZ001 / df$FXS001
df = df[,c("FXS001", "FXZ001", "Prop.Hisp", "CEN.Dissim", "F2L001","HEZ001", "HEZ003", "HF5001",
           "HK7007", "HK7014")]
quantile(df$CEN.Dissim)

# Tot pop, hisp pop, prop hisp, Hisp over 65, employed men, employed women
# Housholds earning less that 10k, Graduate men, graduate women

stargazer(df, type = "latex", digits = 2, median = T, nobs = F,
          title = "Census Socioeconomic Summary Statistics",
          covariate.labels = c(
            "Total Population",
            "Hispanic Population",
            "Proportion Hispanic",
            "Dissimilarity Index",
            "Hispanic Over 65s",
            "Employed Male Pop",
            "Employed Female Pop",
            "Households Earning Sub 10k",
            "Men with Graduate Educ.",
            "Women with Graduate Educ."
          ),
          notes = c("\\textit{Note:} Summary statistics from 919 districts taken from 2000 census. Data is",
                    "at block group level and joined to districts spatially."),
          notes.align = "l",
          out = "../../output/data_section/demo_cen.tex",
          label = "t::demo_cen",
          table.placement = "H")


#### Tch-Student Ratios ####
df = fread("../../data/output/analysis/district_regressions.csv")
df <- 
  df %>%
  summarise(
    .by = "SY",
    Dist.RT.FT = mean(Dist.RT.FT, na.rm = T),
    Dist.RT.H.FT = mean(Dist.RT.H.FT, na.rm = T),
    Dist.RT.EL.FT = mean(Dist.RT.EL.FT, na.rm = T)
  )

pdf("../../output/data_section/time_rt_ft.pdf", width = 8.6, height = 6)
barplot(df$Dist.RT.FT ~ df$SY, 
        main = "Time Trends in Average District Teacher-Student Ratio",
        ylab = "Average Teach Student Ratio",
        xlab = "")
dev.off()
pdf("../../output/data_section/time_rt_h_ft.pdf", width = 8.6, height = 6)
barplot(df$Dist.RT.H.FT ~ df$SY, 
        main = "Time Trends in Average District Hispanic Teacher-Student Ratio",
        ylab = "Average Hisp Teach Student Ratio",
        xlab = "")
dev.off()
pdf("../../output/data_section/time_rt_el_ft.pdf", width = 8.6, height = 6)
barplot(df$Dist.RT.EL.FT ~ df$SY, 
        main = "Time Trends in Average District ELD Teacher-Student Ratio",
        ylab = "Average ELD Teach Student Ratio",
        xlab = "")
dev.off()


# Teacher outcomes at district level
df = fread("../../data/output/analysis/district_regressions.csv")
df = df[,c("Dist.SFP.Binary", "Dist.RT.FT", "Dist.RT.H.FT", "Dist.RT.EL.FT",
           "G.RT.FT", "G.RT.H.FT", "G.RT.EL.FT", 
           "Dist.Total", "Dist.Stu.Hisp")]
stargazer(df, type = "latex", median = T,
          digits = 2, nobs = T,
          title = "District Level Outcome Variables",
          covariate.labels = c(
            "SFP Indicator", "TS Ratio",
            "Hispanic TS Ratio", "ELD TS Ratio",
            "Gini: Overall", "Gini: Hispanic", 
            "Gini: ELD", "Students", "Hisp. Students"
          ),
          notes = c("\\textit{Note:} Summary statistics for approximately 15,000 district-years. TS Ratio",
                    "refers to the teacher-student ratio. Gini refers to the Gini coefficient across",
                    "schools of the teach student ratio for categories of teachers."),
          notes.align = "l",
          out = "../../output/data_section/district_teacher_outcomes.tex",
          label = "at::teachers_district",
          table.placement = "H")


# At the school level
df = fread("../../data/output/analysis/school_regressions.csv")
df = df[,c("SFP.Binary", "RT.FT", "RT.H.FT", "RT.EL.FT", "Stu.Count.Reported", "Stu.Count.Hisp")]
stargazer(df, type = "latex", median = T,
          digits = 2, nobs = T,
          title = "School Level Outcome Variables",
          covariate.labels = c(
            "SFP Indicator", "TS Ratio",
            "Hispanic TS Ratio", "ELD TS Ratio",
            "Students", "Hisp. Students"
          ),
          notes = c("\\textit{Note:} Summary statistics for approximately 158,000 school-years. TS Ratio",
                    "refers to the teacher-student ratio."),
          notes.align = "l",
          out = "../../output/data_section/school_teacher_outcomes.tex",
          label = "at::teachers_school",
          table.placement = "H")

# Total Effects ----------------------------------------------------------------
df = fread("../../data/output/analysis/district_regressions.csv")
sum(df$Dist.SFP.Binary==1, na.rm = T)/nrow(df)

df = fread("../../data/output/analysis/school_regressions.csv")
sum(df$SFP.Binary==1, na.rm = T)/nrow(df)
