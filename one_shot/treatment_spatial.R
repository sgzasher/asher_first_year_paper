setwd(dirname(rstudioapi::getSourceEditorContext()$path))
set.seed(1966)
library(dplyr)
library(ggplot2)
library(data.table)
library(fuzzyjoin)
library(tidyverse)
library(rdrobust)
library(osmdata)
library(sf)
library(tigris)
options(tigris_use_cache = TRUE)

# Data Read -----------
data.magazinnik <- fread("../../data/magazinnik/panel_agg.csv")
data.sf.district <- st_read("../../data/geospatial/district_boundary/EDGE_SCHOOLDISTRICT_TL16_SY1516/schooldistrict_sy1516_tl16.shp")
data.xwalk <- fread("../../data/cde/pubschls.txt")

# Magazinnik Snippet ------

# CODE SNIPPET FROM MAGAZINNIK FOR CAUSAL SAMPLE

# eliminate always trustee places and voluntary conversions
df = data.magazinnik
df$trust.leg <- ifelse(df$trustee==1 & df$switcher==1 & df$switch.type %in% c(1, 2), 1, 0)
always <- unique(df$id[df$switcher==0 & df$trustee==1])
voluntary <- unique(df$id[df$switcher==1 & !df$switch.type %in% c(1,2)])
data <- df[!df$id %in% c(always, voluntary),]

# create subset: <60% eligible to vote, latino (on average over the sample)
data$propl.18p <- data$propl.18p.foreign.natcit + data$propl.18p.native # proportion over 18 who can vote
data$propl.voting <- data$propl.18p * data$prop.l
aveprops <- tapply(data$propl.voting, data$id, mean, na.rm=TRUE)
dropthese <- names(aveprops)[aveprops>.6 & !is.na(aveprops)]
sub60 <- data[!data$id %in% dropthese,]

# create further subset: eligible according to Rubin
data$propboard <- (data$Spanish.Surname/data$Members)*100
eligibles <- unique(data$id[data$propboard<=data$Hispanic & data$Hispanic>=15 & data$trustee==0 & data$year==2002]) 
sub60 <- sub60[sub60$id %in% eligibles,]

# Magazinnik Prep ----------

# The authors stored their IDs as numerics for unknowable reasons. There are 
# MANY trailing zeroes in the NCES IDs so we're just ignoring a class of
# them almost entirely. 

# Take the 2016 snapshot, get current status and treatment status
# Should code up switch type 

data.treatment <- 
  data.magazinnik %>%
  dplyr::filter(
    year == 2016
  ) %>%
  dplyr::select(
    id, trustee, switcher, switch.type, threat
  ) %>%
  dplyr::mutate(
    causal = ifelse(id %in% unique(sub60$id), 1, 0),
    id = bit64::as.integer64(id)
  ) %>%
  dplyr::rename(
    CDSCode = id
  )

# Grab NCESDist, Name from XWalk

data.treatment <- 
left_join(
  data.treatment, 
  dplyr::select(
    data.xwalk,
    CDSCode, NCESDist, District
  ),
  by = "CDSCode"
)

# SF Prep ----------------
data.sf.district <- 
  data.sf.district %>%
  dplyr::rename(
    NCESDist = GEOID
  ) %>%
  dplyr::filter(
    STATEFP == "06"
  )

# SF Merge & Process ---------
data.sf.district <- 
  left_join(
    data.sf.district,
    data.treatment,
    by = "NCESDist"
  ) %>%
  dplyr::mutate(
    CDSCode = as.character(CDSCode)
  )

# We get 864 matched rows, should be about enough for the simple spatial plot

# Save to SF for QGIS (ArcGIS once I'm connected to internet)
st_write(data.sf.district,
         "../data/output/geospatial/treatment_spatial/treatment_spatial.shp",
         append = F,
         SHAPE_RESTORE_SHX = T)


