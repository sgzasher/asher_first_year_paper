# Script Target: Achievement data from Fischer. Will attempt standardization 
# later if I think of something...

# Packages ---------------------------------------------------------------------
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
set.seed(1966)
library(dplyr)
library(data.table)
library(haven)
library(purrr)
library(stringr)

# Get list for reading  --------------------------------------------------------
base.path = "../../data/raw/cde/fischer_achievement"
files.all = list.files(base.path)

# How do we need to read them in?
files.txt = files.all[str_sub(files.all, -3, -1) %in% c("txt", "TXT")]
files.tab = files.all[str_sub(files.all, -3, -1) %in% c("tab")]
files.dta = files.all[str_sub(files.all, -3, -1) %in% c("dta")]

# And are they the test files or the entity files?
files.test = files.all[str_detect(files.all, "test")]
files.entity = files.all[str_detect(files.all, "entity")]

# Column Name Standardization
colnames.fixed <- function(x){
  output = 
    str_replace_all(toupper(x), " ", "")
  return(output)
}

# Text Files: Column Names -----------------------------------------------------

# First: get the relevant column names for each file
files.read = files.test[files.test %in% files.txt]

# Check headers... 
txt.colnames <- 
  lapply(files.test[files.test %in% files.txt], function(x){
    header = colnames(fread(paste0(base.path, "/", x), nrow = 1))
    return(header)
  })

# Standardize
txt.colnames <-
  lapply(txt.colnames, colnames.fixed)

# Get the set of headers that we're intersted in
keep.colnums <- 
lapply(txt.colnames, function(x){
  which(str_detect(x,"CODE|TESTID|YEAR|SUBGROUP|MEANSCAL|TESTED|GRADE" ))
})

# Read & Prepare the Text Files ------------------------------------------------

read.txt <- function(file.name, keep.cols, path.in){
  
  # Read
  df.out <- fread(
    paste0(path.in, "/", file.name),
    select = keep.cols,
    colClasses = "character"
  )
 
  return(df.out)
   
}
  

func.prepare <- function(df.input){
  
  df.out <- df.input
  
  # Colnames
  colnames(df.out) = colnames.fixed(colnames(df.out))
  
  # Rename some columns that change over time
  colnames(df.out) <- colnames(df.out) %>%
    str_replace_all("TESTYEAR", "YEAR") %>%
    str_replace_all("SUBGROUPID", "SUBGROUP") %>%
    str_replace_all("MEANSCALESCORE", "MEANSCALEDSCORE")
  
  
  # Subtypes & Exams
  df.out <- df.out %>% dplyr::filter(SUBGROUP %in% c("1", "78"))
  df.out <- df.out %>%
    dplyr::filter(
      GRADE %in% c("3", "4", "5", "6", "7", "8", "11"),
      TESTID %in% c("1", "2")
    )
  
  # Select 
  df.out <- dplyr::select(df.out,
                          COUNTYCODE, DISTRICTCODE, SCHOOLCODE,
                          YEAR, SUBGROUP, GRADE, TESTID,
                          STUDENTSTESTED,
                          MEANSCALEDSCORE)
  
  colnames(df.out) <- 
    c("FIPS.County", "NCESDist", "NCESSchool", "SY",
      "Stu.Group", "Stu.Grade", "Test", 
      "Students.Cell", "MeanScore")
  
  return(df.out)
  
}

# Read Text Files --------------------------------------------------------------

# First one
df.txt <- 
  read.txt(files.read[[1]], keep.colnums[[1]], base.path) %>% 
  func.prepare()

# And remaining
for(i in 2:length(files.read)){
  
  print(files.read[[i]])
  
  # Temp file
  temp <- 
    read.txt(files.read[[i]], keep.colnums[[i]], base.path) %>%
    func.prepare()
  
  # Append
  df.txt <- bind_rows(df.txt, temp)
  
  # Remove temp
  rm(temp)
}

# Read DTA Files ---------------------------------------------------------------

# Only want the 2002
files.read = files.test[files.test %in% files.dta]
df.dta <- read_dta(paste0(base.path, "/", files.read[[2]])) %>%
  dplyr::mutate(across(everything(), as.character))

# Format and process
df.dta$Subgroup = "1"
df.dta <- df.dta %>% dplyr::filter(
  GradeID %in% c("3", "4", "5", "6", "7", "8", "11"),
  TestID %in% c("1", "2")
  ) %>%
  dplyr::select(
    cds, Year, Subgroup, GradeID, TestID, NumberTested, MeanScaledScore
  )

colnames(df.dta) <- 
  c("CDSCode", "SY", "Stu.Group", "Stu.Grade",
    "Test", "Student.Cell", "MeanScore")

# Read TAB Files ---------------------------------------------------------------

files.read = files.test[files.test %in% files.tab]
df.tab <- read.table(paste0(base.path, "/", files.read[[2]]), 
                     header = T,
                     fill = T)

# Refactor Variables -----------------------------------------------------------
