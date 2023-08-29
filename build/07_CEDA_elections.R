# Script Target: CEDA Data. Only issue is that we're spread over a few files
# and that we're including a bunch of elections we don't care about. So an 
# append & subset job only. 

# Packages ---------------------------------------------------------------------
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
set.seed(1966)
library(dplyr)
library(data.table)
library(haven)
library(purrr)
library(readxl)
library(lubridate)

# District Frame ---------------------------------------------------------------
frame.district = fread("../../data/output/frames/frame_district.csv")

# Read script ------------------------------------------------------------------

read.file <- function(input.path){
  
  # Get correct sheet
  sheets = excel_sheets(input.path)
  sheet.read = sheets[str_detect(sheets, "andid")]
  
  # Read correct sheet
  output = read_excel(input.path, sheet = sheet.read,
                      col_types = "text")
  return(output)
}



# Read & Append ----------------------------------------------------------------

# Set of files
list.years = as.character(2000:2017)
list.files = list.files("../../data/raw/ceda")

# Read first
df.ceda = read.file(
  paste0(
    "../../data/raw/ceda/",
    list.files[grepl("2000", list.files)]
  )
)

# Read remaining
for(i in as.character(2001:2017)){
  
  # Read current
  temp = read.file(
    paste0(
      "../../data/raw/ceda/",
      list.files[grepl(i, list.files)]
    )
  )
  
  # Append
  df.ceda <- 
    bind_rows(
      df.ceda, 
      temp
    )
  
  # Remove
  rm(temp)
}

# Clean and subset -------------------------------------------------------------
df.ceda <- 
  df.ceda %>%
  dplyr::mutate(
    DATE = as.Date(
      as.numeric(DATE), origin = "1899-12-30")
  ) %>%
  dplyr::filter(
    OFFICE %in% c("School Board Member", "SCHOOL BOARD MEMBER")
  ) %>%
  dplyr::mutate(
    rownum = row_number()
  )

# Fuzzy Join Functions ---------------------------------------------------------

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
    # str_replace_all("Elementary", "") %>%
    # str_replace_all("Middle", "") %>%
    # str_replace_all("District", "") %>%
    # str_replace_all("School", "") %>%
    # str_replace_all("High", "") %>%
    # str_replace_all("Unified", "") %>%
    toupper()
  return(vs)
}

# Full fuzzy application script 
fzzy.run <- function(c.fun, c.max, df.1, df.2){
  
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
      FUN = function(x){out.dist[[x]] & out.count[[x]]}
    )
  
  return(output)
  
}

# Step 1: Tight Matches Only ---------------------------------------------------
frame.district$Stand_District = clean_strings(frame.district$District)
frame.district$Stand_County = clean_strings(frame.district$County)
df.ceda$Stand_District = clean_strings(df.ceda$PLACE)
df.ceda$Stand_County = clean_strings(df.ceda$CNTYNAME)


fuzzy.results <- 
  fzzy.run(
    "jw",
    0.1,
    df.ceda,
    frame.district
  )

ceda.none = df.ceda[as.vector(sapply(fuzzy.results, FUN = sum) == 0),]
ceda.many = df.ceda[as.vector(sapply(fuzzy.results, FUN = sum) > 1),]

# Step 2: With Standardizations ------------------------------------------------

# Standardization Script
clean_strings_lots <- function(vector.string){
  vs = vector.string
  vs <- paste(str_extract_all(vs, '[:alnum:]*'))
  vs <- 
    vs %>%
    str_replace_all("[:punct:]", "") %>%
    str_replace_all("\"", "") %>%
    str_replace_all(",", "") %>%
    str_replace_all("^c", "") %>%
    str_replace_all(" ", "") %>%
    toupper() %>%
    str_replace_all("ELEMENTARY", "") %>%
    str_replace_all("MIDDLE", "") %>%
    str_replace_all("DISTRICT", "") %>%
    str_replace_all("SCHOOL", "") %>%
    str_replace_all("HIGH", "") %>%
    str_replace_all("UNION", "") %>%
    str_replace_all("UNIFIED", "") %>%
    str_replace_all("JOINT", "")
  return(vs)
}

# Standardize more
ceda.none$Stand_District = clean_strings_lots(ceda.none$PLACE)
frame.district$Stand_District = clean_strings_lots(frame.district$District)

# Get results
fuzzy.results.s2 <- 
  fzzy.run(
    "jw",
    0.2,
    ceda.none,
    frame.district
  )

# These guys STILL not matching
ceda.nonen = ceda.none[as.vector(sapply(fuzzy.results.s2, FUN = sum) == 0),]

# Step 3: Without County -------------------------------------------------------

# Also: ignore all the community colleges (about half the sample)
ceda.nonen <- 
  ceda.nonen %>%
  dplyr::filter(
    !(str_detect(toupper(PLACE), "COMMUNITY"))
  )

# Estimate from a sample of non-matchers: approx. 60% of rows have wrong county
# In reality, gets about 58% (!)
fuzzy.results.s3 <- 
  fzzy.fst(
    choice.function = "jw",
    choice.max = 0.1, 
    df1 = ceda.nonen, 
    df2 = frame.district, 
    choice.variables = c("Stand_District", "Stand_District"))

ceda.nohope = ceda.nonen[as.vector(sapply(fuzzy.results.s3, FUN = sum) == 0),]

# Step 4: Disambiguate ---------------------------------------------------------

# First step manys
ceda.many$Stand_District = clean_strings_lots(ceda.many$PLACE)
fuzzy.results.s41 <- 
  fzzy.run(
    "jw",
    0.01,
    ceda.many, 
    frame.district
  )

# Second step manys
ceda.many.s2 = ceda.none[as.vector(sapply(fuzzy.results.s2, FUN = sum) > 1),]
ceda.many.s2$Stand_District = clean_strings_lots(ceda.many.s2$PLACE)
fuzzy.results.s42 <- 
  fzzy.run(
    "jw",
    0.01,
    ceda.many.s2, 
    frame.district
  )

# Third step manys: literally zero matched so just skipping that step and 
# considering them no-hopers. This is a pretty good job for now, I think!

# Step 5: Concatenate ----------------------------------------------------------

# Function to quickly return frame NCES codes corresponding to lists of logical
# match vectors from merge steps

match.codes <- function(input.df, input.list, frame.df){
  
  codes <-
    mclapply(
      1:nrow(input.df),
      FUN = function(x){
        ifelse(
          str_detect(
            as.character(frame.df[input.list[[x]], "NCESDist"]), "c\\("),
          "Many",
          ifelse(
            str_detect(
              as.character(frame.df[input.list[[x]], "NCESDist"]), "int"),
            "None",
            as.character(frame.df[input.list[[x]], "NCESDist"])
          )
        )
      }
    )
  
  return(codes)
}

# Single matchers from step 1
df.ceda$NCESDist = match.codes(df.ceda, fuzzy.results, frame.district)

# Single matchers from step 2
ceda.none$NCESDist = match.codes(ceda.none, fuzzy.results.s2, frame.district)

# Single matchers from step 3
ceda.nonen$NCESDist = match.codes(ceda.nonen, fuzzy.results.s3, frame.district)

# Multi-single matchers; step 1
ceda.many$NCESDist = match.codes(ceda.many, fuzzy.results.s41, frame.district)

# Multi-single matcers; step 2
ceda.many.s2$NCESDist = match.codes(ceda.many.s2, fuzzy.results.s42, 
                                    frame.district)

# Concatenate
df.matched <- 
  bind_rows(
    df.ceda, 
    ceda.none,
    ceda.nonen, 
    ceda.many, 
    ceda.many.s2
  ) %>%
  dplyr::select(
    NCESDist, rownum
  ) %>%
  dplyr::filter(
    !(NCESDist %in% c("Many", "None"))
  )

# Output -----------------------------------------------------------------------

# Remove dist from ceda
df.ceda <- 
  df.ceda %>%
  dplyr::mutate(
    NCESDist = unlist(df.ceda$NCESDist)
  ) %>%
  dplyr::select(
    -NCESDist
  )

# Stich in matched NCES
df.ceda <- 
  left_join(
    df.ceda, 
    df.matched, 
    by = "rownum"
  )

# Repair column names
colnames(df.ceda) <- str_replace(colnames(df.ceda), "#", "")

# Subset
df.output <- 
  df.ceda %>%
  dplyr::select(
    RecordID, RaceID, NCESDist, DATE,
    AREA, VOTE, LAST, FIRST, BALDESIG, 
    INCUMB, CAND, VOTES, SUMVOTES,
    ELECTED
  )

# Fix NCESDist for writing
df.output <-
  dplyr::mutate(
    df.output, 
    NCESDist = unlist(ifelse(NCESDist == "NULL", "", NCESDist))
  )

# Column names
colnames(df.output) <- 
  c("RecordID", "RaceID", "NCESDist", 
    "ELEC.Date", "ELEC.Area", "ELECT.K",
    "CAND.FirstName", "CAND.LastName",
    "CAND.BalDes", "CAND.Incumb", 
    "ELEC.CandNum", "CAND.Votes", 
    "ELEC.VoteTotal", "CAND.Elected")

# Output
write.csv(
  df.output,
  "../../data/output/build/ceda_candidates.csv"
)

