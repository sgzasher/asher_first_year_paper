# Script Target: Identify Population/Universe of Relevant California Schools
# Restrictions: set of districts in Abott and Magazinnik (for treatment data)
# Notice some districts in Abott and Magazinnik have data but 
# are closed due to an incorrect imputation implementation

# Packages ---------------------------------------------------------------------
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
set.seed(1966)
library(dplyr)
library(data.table)
library(tidyverse)

# Data Read --------------------------------------------------------------------
data.directory <- fread("../../data/raw/cde/directory/pubschls.txt")
data.magazinnik <- 
  fread("../../data/replication_directories/magazinnik/panel_agg.csv")

# Relevant districts -----------------------------------------------------------

# Only want the set of public school-providing districts
# 0: County office of education
# 52, 54, 56: Elementary, Unified, High
choice.districts <- 
  c(0, 52, 54, 56)

data.dists <-
  data.directory %>%
  dplyr::mutate(
    disttag = ifelse(
      str_sub(as.character(CDSCode), -7, -1) == "0000000", 1, 0
    )
  ) %>% 
  dplyr::filter(
    disttag == 1,
    DOC %in% choice.districts
  )

# Subset to active -------------------------------------------------------------

# Schools to xref against
# Notice: ignoring schools that closed before 2000; 1000 cases that cause me
# lots of problems 
data.schools <- 
  data.directory %>%
  dplyr::mutate(
    disttag = ifelse(
      str_sub(as.character(CDSCode), -7, -1) == "0000000", 1, 0
    )
  ) %>% 
  dplyr::filter(
    disttag == 0,
    (as.numeric(str_sub(ClosedDate, -4, -1)) > 2001 | ClosedDate == "No Data"),
    Charter == "N"
  )

# Nonactive
dists.nonac <- 
  dplyr::filter(
    data.dists,
    StatusType != "Active"
  ) %>%
  dplyr::select(
    CDSCode, District, StatusType
  ) 

# Number of schools attached
dists.nonac$count.schools <- 
  as.vector(
    sapply(
      dists.nonac$District,
      FUN = function(x){
        sum(data.schools$District == x & data.schools$StatusType != "Active")
        }
    )
  )

# Inspection: .25% of sample has problems here
sum(dists.nonac$count.schools)/nrow(data.schools) * 100

# Kill them! District level
data.output.districts <- 
  data.dists %>%
  dplyr::filter(
    StatusType == "Active"
  ) %>%
  dplyr::select(
    CDSCode, NCESDist, County, District, WebSite, DOC, DOCType
  )

# Subset to Magazinnik ---------------------------------------------------------

# Get treatment data
data.magazinnik <- 
  data.magazinnik %>%
  dplyr::mutate(
    id = bit64::as.integer64(data.magazinnik$id)
  )

# Get NCESCodes attached
data.magazinnik <- 
  left_join(
    data.magazinnik,
    data.dists,
    by = c("id" = "CDSCode")
  ) %>%
  dplyr::select(
    id, switcher, switch.type, NCESDist
  )

# Unique cases; IDs for subset
data.magazinnik <- 
  unique(data.magazinnik) %>%
  dplyr::select(-id)

# Kill them! District level
data.output.districts <- 
  data.output.districts %>%
  dplyr::filter(
    NCESDist %in% data.magazinnik$NCESDist
  )

# Get the treatment data in the panel!
data.output.districts <- 
  left_join(
    data.output.districts,
    data.magazinnik,
    by = "NCESDist"
  ) %>%
  dplyr::mutate(
    switch.type = case_match(
      switch.type,
      1 ~ "Legal Action", 
      2 ~ "Legal Threat", 
      3 ~  "Voluntary",
      4 ~ "Unknown"
    )
  )

# School Level Frame -----------------------------------------------------------

# Note which districts survive
keep.district = unique(data.output.districts$NCESDist)

# Want only those schools (also gets rid of ROCs etc.)
data.output.school <- 
  data.schools %>%
  dplyr::filter(
    NCESDist %in% keep.district 
  ) %>%
  dplyr::select(
    CDSCode, NCESDist, NCESSchool, StatusType, County, District, 
    School, Street, City, Zip, WebSite, OpenDate, ClosedDate, 
    Charter, Latitude, Longitude
  )

# Export Data ------------------------------------------------------------------
write.csv(
  data.output.districts,
  "../../data/frames/frame_district.csv"
)

write.csv(
  data.output.school,
  "../../data/frames/frame_school.csv"
)
