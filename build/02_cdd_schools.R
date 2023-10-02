# Script Target: Get school-level socioeconomic/demographic data from the CCD. 
# On the way, lots of unzipping, formatting etc. which is gonna be a 
# massive pain in the ass. 

# Packages ---------------------------------------------------------------------
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
set.seed(1966)
library(dplyr)
library(data.table)
library(haven)
library(purrr)
library(stringr)

# Format A Extraction ----------------------------------------------------------

# Formatting the SAS 3-file partition SAS files for 2000-2007
# Notice we subset to California inside of the function!

read.formatA <- function(path.data){
  
  # Unzip raw files
  list.unzip = list.files(path.data)
  
  for(i in list.unzip){
    unzip(paste0(path.data, "/", i), 
          exdir = path.data)
  }
  
  # Get resulting SAS files
  list.files = list.files(path.data)
  list.files = list.files[str_sub(list.files, -3, -1) != "zip"]
  
  # For each SAS file; read and append
  out.data = read_sas(paste0(path.data, "/", list.files[1]))
  out.data <- 
    out.data %>%
    dplyr::add_row(
      read_sas(paste0(path.data, "/", list.files[2]))
    ) %>%
    dplyr::add_row(
      read_sas(paste0(path.data, "/", list.files[3]))
    ) %>%
    dplyr::filter(
      FIPST == "06"
    )
  
  # Remove unzipped (ZIP files remain)
  for(i in list.files){
    file.remove(paste0(path.data, "/", i))
  }
  
  # Return
  return(out.data)
  
}

# Format B Extraction ----------------------------------------------------------

read.formatB <- function(path.data){
  
  # Unzip raw files; single file per path
  unzipme = list.files(path.data)
  unzip(paste0(path.data, "/", unzipme),
        exdir = path.data)
  
  # Get resulting TXT files
  readme = list.files(path.data)
  readme = readme[str_sub(readme, -3, -1) != "zip"]

  # Read data
  out.data = fread(paste0(path.data, "/", readme), 
                   colClasses = "character") %>%
              dplyr::filter(
                FIPST == "06"
              )

  # Remove unzipped
  file.remove(paste0(path.data, "/", readme))
  
  # Return
  return(out.data)
}

# Format C Extraction ----------------------------------------------------------

read.formatC <- function(path.data){
  
  # Variables for left joins
  vars.join = c("SURVYEAR", "FIPST", "STABR", "STATENAME", "LEAID",
                "ST_LEAID", "SCHID", "ST_SCHID", "NCESSCH", "SCH_NAME")
  
  # Unzip raw files; single file per path
  unzipme = list.files(path.data)
  
  for(file in unzipme){
    unzip(paste0(path.data, "/", file),
          exdir = path.data)
  }
 
  
  # Get resulting TXT files
  readme = list.files(path.data)
  readme = readme[str_sub(readme, -3, -1) != "zip"]

  # Read data
  out.data = fread(paste0(path.data, "/", readme[1]), 
                   colClasses = "character") %>%
            dplyr::filter(
              FIPST == "06"
            )
  
  for(file in readme[2:5]){
    temp.file = fread(paste0(path.data, "/", file),
                 colClasses = "character") %>%
               dplyr::filter(
                 FIPST == "06"
               )
    
    out.data = left_join(
      out.data, 
      temp.file, 
      by = vars.join
      )
    
    rm(temp.file)
  }
     
  # Remove unzipped
  for(i in readme){
    file.remove(paste0(path.data, "/", i))
  }
  
  # Return
  return(out.data)
}

# Variable Name Standardization ------------------------------------------------

# Last 2 chars of each var are the year; need to chop it off

stand.varnames <- function(df, id.list){
  varnames = colnames(df)
  varnames = varnames[!(varnames %in% id.list)]
  varnames = sub("(.*)(.{2})", "\\1", varnames)
  colname.output = c(id.list, varnames)
  return(colname.output)
}


# 2000-07: Data Read -----------------------------------------------------------

# This data is in .dat format spread over 3 files. 
base.path = "../../data/raw/ccd/nonfiscal/"
list.years = list.files(base.path)[1:7]
idvars = c("NCESSCH", "FIPST", "LEAID", "SCHNO")

# Read in first file; notice we're about to chop 2 chars from SY
df.nonfiscal <-
  read.formatA(paste0(base.path, "2000-01")) %>%
  dplyr::mutate(
    SYXX = "2000-01"
  )

# Get list of variable names that are not IDs, remove year appendage
colnames(df.nonfiscal) = stand.varnames(df.nonfiscal, idvars)

# Append the rest in a loop
for(i in 2:7){
  print(list.years[i])
  
  # Read in file, note year
  temp <- 
    read.formatA(paste0(base.path, list.years[i])) %>%
    dplyr::mutate(
      SYXX = list.years[i]
    )
  
  # Standardize the column names
  colnames(temp) = stand.varnames(temp, idvars)
  
  # Lat and Lon are causing trouble
  temp <- temp %>%
    dplyr::mutate(
      LATCOD = as.character(LATCOD),
      LONCOD = as.character(LONCOD)
    )
  
  # Need bind to deal with new variables
  df.nonfiscal <- 
    bind_rows(
      df.nonfiscal, 
      temp
    )
  
  # Remove temp; loop
  rm(temp)
}

# 2000-07: Data Selection ------------------------------------------------------

# Note: uncollected because in CDD dirctory...
# GSLO, GSHI, LEVEL, TYPE, STATUS, MAGNET, CHARTER, 

df.nonfiscal <- 
  df.nonfiscal %>%
  dplyr::select(
    NCESSCH, FIPST, LEAID, LEANM, SCHNAM, 
    FTE, TITLEI, STITLI, FRELCH, REDLCH, 
    TOTFRL, MEMBER, AM, ASIAN, HISP, BLACK, WHITE, 
    TOTETH, SY
  )

df.nonfiscal$PACIFIC <- NA
df.nonfiscal$TR <- NA

names.cols <- c(
  "NCESSchool", "FIPS.State", "NCESDist", "District", "School",
  "FTE", "Title.I", "Title.I.School", 
  "FSM", "RSM", "TOT.FRSM", "Stu.Count.Reported",
  "Stu.Native", "Stu.AAPI", "Stu.Hisp", "Stu.Black", "Stu.White", 
  "Stu.NonWhite", "SY", "Stu.Pacific", "Stu.MultiEth" 
)

colnames(df.nonfiscal) = names.cols

# 2007-14: Data Read -----------------------------------------------------------

# This data is in single txt files; easier we will deal with
list.years = list.files(base.path)[8:14]

df.0714 <- 
  read.formatB(paste0(base.path, "2007-08")) %>%
  dplyr::mutate(
    SYXX = "2007-08"
  )

# Note: in 2010-11, we get rid of the year IDs and add a "Survyear" variable...
colnames(df.0714) = stand.varnames(df.0714, idvars)

# Read in
for(file in list.years[1:7]){
  print(file)
  
  # Read in file, note year
  temp <- 
    read.formatB(paste0(base.path, file)) %>%
    dplyr::mutate(
       SYXX = file
    )
  
  # Standardize the column names for relevant years
  if(which(file == list.years) < 4){
    colnames(temp) = stand.varnames(temp, idvars)
  }
  
  # If not relevant, drop Survyear
  if(which(file == list.years) >= 4){
    temp <- temp %>% dplyr::select(-SURVYEAR) %>%
      dplyr::rename(SY = SYXX)
  }
  
  # Need bind to deal with new variables
  df.0714 <- 
    bind_rows(
      df.0714, 
      temp
    )
  
  # Remove temp; loop
  # rm(temp)
}

# 2007-14: Data Selection ------------------------------------------------------

df.0714 <- 
  df.0714 %>%
  dplyr::select(
    NCESSCH, FIPST, LEAID, LEANM, SCHNAM, 
    FTE, TITLEI, STITLI, FRELCH, REDLCH, 
    TOTFRL, MEMBER, AM, ASIAN, HISP, BLACK, WHITE, PACIFIC,
    TR, TOTETH, SY
  )

names.cols <- c(
  "NCESSchool", "FIPS.State", "NCESDist", "District", "School",
  "FTE", "Title.I", "Title.I.School", 
  "FSM", "RSM", "TOT.FRSM", "Stu.Count.Reported",
  "Stu.Native", "Stu.Asian", "Stu.Hisp", "Stu.Black", "Stu.White", 
  "Stu.Pacific", "Stu.MultiEth", "Stu.NonWhite", "SY"
)

colnames(df.0714) = names.cols

# 2014-16: Data Read -----------------------------------------------------------

# Now spread across multiple files. We need data from membership, staff, and 
# free lunch surveys to get the set of variables above. No loop here; only 
# two files with this particular format

df.1416 = read.formatC(paste0(base.path, "2014-15")) %>%
  dplyr::mutate(
    SY = "2014-15"
  )

temp = read.formatC(paste0(base.path, "2015-16")) %>%
  dplyr::mutate(
    SY = "2015-16"
  )

df.1416 <- 
  bind_rows(
    df.1416,
    temp
  )

rm(temp)

# 2014-16: Data Selection ------------------------------------------------------

# Notice from documentation: "Member" refers to total elementary and 
# secondary school students; ignored "adult education" (only added in these
# survey waves)

df.1416 <- 
  df.1416 %>%
  dplyr::select(
    SCHID, FIPST, LEAID, LEA_NAME, SCH_NAME, 
    FTE, TITLEI, STITLEI, FRELCH, REDLCH, 
    TOTFRL, MEMBER, AM, AS, HI, BL, WH, HP, TR, SY
  )

df.1416$TOTETH <- NA

names.cols <- c(
  "NCESSchool", "FIPS.State", "NCESDist", "District", "School",
  "FTE", "Title.I", "Title.I.School", 
  "FSM", "RSM", "TOT.FRSM", "Stu.Count.Reported",
  "Stu.Native", "Stu.Asian", "Stu.Hisp", "Stu.Black", "Stu.White", 
  "Stu.Pacific", "Stu.MultiEth", "SY", "Stu.NonWhite"
)

colnames(df.1416) = names.cols

# Quick format fix: the NCESSchool has lost the NCESDist prefix
df.1416 <- 
  dplyr::mutate(
    df.1416,
    NCESSchool = paste0(NCESDist, NCESSchool)
  )

# 2016-17: Member Data  --------------------------------------------------------

# The member data especially went to long format for no clear reason... 
# also massively explodes the file size... why would they do that?
path.1617 = paste0(base.path, "2016-17")

# Read in with relevant columns
df.1617.members <- 
  fread(
    paste0(path.1617, "/ccd_sch_052_1617_l_2a_11212017.csv"),
    colClasses = "character") %>%
  dplyr::filter(
    ST == "CA",
    TOTAL_INDICATOR %in% c(
      "Derived - Education Unit Total minus Adult Education Count",
      "Derived - Subtotal by Race/Ethnicity and Sex minus Adult Education Count"
      )
  ) %>%
  dplyr::select(
    NCESSCH, FIPST, LEAID, RACE_ETHNICITY, STUDENT_COUNT
  ) %>%
  dplyr::mutate(
    RACE_ETHNICITY = case_match(
      RACE_ETHNICITY,
      "American Indian or Alaska Native" ~ "Native",
      "Asian" ~ "Asian",
      "Black or African American" ~ "Black",
      "Native Hawaiian or Other Pacific Islander" ~ "Pacific",
      "Not Specified" ~ "NS",
      "White" ~ "White",
      "Asian" ~ "Asian",
      "Hispanic/Latino" ~ "Hisp",
      "No Category Codes" ~ "Count.Reported",
      "Two or more races" ~ "MultiEth"
    )
  ) %>%
  dplyr::filter(
    RACE_ETHNICITY != "NS"
  )

# Collapse the gender dimensions
df.1617.members <- 
  df.1617.members %>%
  dplyr::mutate(
    STUDENT_COUNT = as.numeric(STUDENT_COUNT)
  ) %>%
  summarise(
    Stu = sum(STUDENT_COUNT),
    .by = c("NCESSCH", "FIPST", "LEAID", "RACE_ETHNICITY")
    )

# Reshape to wide
df.1617.members <- 
  df.1617.members %>%
  reshape(
    idvar = c("NCESSCH", "FIPST", "LEAID"),
    timevar = "RACE_ETHNICITY",
    direction = "wide"
  ) %>%
  dplyr::rename(
    NCESSch = NCESSCH,
    FIPS.State = FIPST,
    NCESDist = LEAID
  )

# 2016-17: Staff Data ----------------------------------------------------------
df.1617.staff <- 
  fread(
    paste0(path.1617, "/ccd_sch_059_1617_l_2a_11212017.csv"),
    colClasses = "character"
  ) %>%
  dplyr::filter(
    ST == "CA",
    NCESSCH %in% df.1617.members$NCESSch
  ) %>%
  dplyr::select(
    NCESSCH, FIPST, LEAID, TEACHERS
  ) %>%
  dplyr::rename(
    NCESSch = NCESSCH,
    FIPS.State = FIPST,
    NCESDist = LEAID,
    FTE = TEACHERS
  )

# 2016-17: FSM Data ------------------------------------------------------------

df.1617.fsm <- 
  fread(
    paste0(path.1617, "/ccd_sch_033_1617_l_2a_11212017.csv"),
    colClasses = "character"
  ) %>%
  dplyr::filter(
    ST == "CA",
    NCESSCH %in% df.1617.members$NCESSch
  ) %>%
  dplyr::select(
    NCESSCH, FIPST, LEAID, LUNCH_PROGRAM, STUDENT_COUNT
  ) %>%
  dplyr::mutate(
    LUNCH_PROGRAM = case_match(
      LUNCH_PROGRAM, 
      "Free lunch qualified" ~ "FSM",
      "Reduced-price lunch qualified" ~ "RSM",
      "No Category Codes" ~ "TOT.FRSM"
    ),
    STUDENT_COUNT = as.numeric(STUDENT_COUNT)
  ) %>%
  dplyr::filter(
    LUNCH_PROGRAM %in% c("FSM", "RSM", "TOT.FRSM")
  ) %>%
  reshape(
    idvar = c("NCESSCH", "FIPST", "LEAID"),
    timevar = "LUNCH_PROGRAM",
    direction = "wide"
  )

cols = c("NCESSch", "FIPS.State", "NCESDist",
         "FSM", "RSM", "TOT.FRSM")

colnames(df.1617.fsm) = cols

# 2016-17: School Characteristics Data -----------------------------------------

df.1617.chars <- 
  fread(
    paste0(path.1617, "/ccd_sch_129_1617_w_1a_11212017.csv"),
    colClasses = "character"
  ) %>%
  dplyr::filter(
    ST == "CA",
    NCESSCH %in% df.1617.members$NCESSch
  ) %>%
  dplyr::select(
    FIPST, NCESSCH, LEAID, TITLEI_STATUS_TEXT
  ) %>%
  dplyr::mutate(
    Title.I = ifelse(
      !(TITLEI_STATUS_TEXT %in% c(
        "Not a Title I school",
        "Not reported",
        "Missing"
      )),
      1,
      0
    ),
    Title.I.School = ifelse(
      TITLEI_STATUS_TEXT %in% c(
        "Title I schoolwide eligible school-No program",
        "Title I schoolwide school"
      ),
      1,
      0
    )
  ) %>%
  dplyr::mutate(
    Title.I = ifelse(
      TITLEI_STATUS_TEXT %in% c("Not reported", "Missing"),
                     NA, Title.I),
    Title.I.School = 
      ifelse(TITLEI_STATUS_TEXT %in% c("Not reported", "Missing"),
                     NA, Title.I.School),
  ) %>%
  dplyr::select(
    NCESSCH, FIPST, LEAID, Title.I, Title.I.School
  ) %>%
  dplyr::rename(
    NCESSch = NCESSCH,
    NCESDist = LEAID,
    FIPS.State = FIPST
  )

# 2016-17: School Directory Data -----------------------------------------------

df.1617.directory <- 
  fread(
    paste0(path.1617, "/ccd_sch_029_1617_w_1a_11212017.csv"),
    colClasses = "character"
  ) %>%
  dplyr::filter(
    ST == "CA",
    NCESSCH %in% df.1617.members$NCESSch
  ) %>%
  dplyr::select(
    NCESSCH, FIPST, LEAID, SCH_NAME, LEA_NAME
  ) %>%
  dplyr::mutate(SY = "2016-17") %>%
  dplyr::rename(
    NCESSch = NCESSCH,
    FIPS.State = FIPST,
    NCESDist = LEAID,
    District = LEA_NAME,
    School = SCH_NAME
  )

# 2016-17: Combine -------------------------------------------------------------
  
# Fill in missings...
frame.1617 = dplyr::select(df.1617.members, NCESSch, NCESDist, FIPS.State)
df.1617.fsm = left_join(frame.1617, df.1617.fsm,
                        by = c("NCESSch", "NCESDist", "FIPS.State"))

df.1617 <- 
  purrr::reduce(
    list(
      df.1617.directory,
      df.1617.chars,
      df.1617.staff,
      df.1617.fsm,
      df.1617.members
    ),
    dplyr::left_join,
    by = c("NCESSch", "NCESDist", "FIPS.State")
  )

df.1617$Stu.NonWhite = NA

rm(
  df.1617.directory,
  df.1617.chars,
  df.1617.staff,
  df.1617.fsm,
  df.1617.members,
  frame.1617
)

# Combine All And Export -------------------------------------------------------

df.1617 <- df.1617 %>% 
  dplyr::mutate(across(everything(), as.character)) %>%
  dplyr::rename(NCESSchool = NCESSch)
df.nonfiscal <- df.nonfiscal %>% 
  dplyr::mutate(across(everything(), as.character))

df.export <- 
  bind_rows(
    df.nonfiscal,
    df.0714,
    df.1416,
    df.1617
  )

write.table(
  df.export,
  "../../data/output/built/cdd_controls.csv",
  append = FALSE,
  row.names = FALSE,
  sep = ","
)
