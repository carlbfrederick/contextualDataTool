library(usethis)
library(curl)
library(tidyverse)
library(readxl)
library(sf)
library(rmapshaper)

yr <- "2017-18"

#School & District Report Card Data
a <- tempdir()

curl_download(paste("http://dpi.wi.gov/sites/default/files/imce/accountability/xls/", yr, "_district_reportcard_data.xlsx", sep = ""),
              destfile = file.path(a, "district.xlsx"))

dist_reportcard <- read_excel(file.path(a, "district.xlsx"), sheet = "Data")

curl_download(paste("http://dpi.wi.gov/sites/default/files/imce/accountability/xls/", yr, "_school_reportcard_data.xlsx", sep = ""),
              destfile = file.path(a, "school.xlsx"))

sch_reportcard <- read_excel(file.path(a, "school.xlsx"), sheet = "Data")

use_data(dist_reportcard)
use_data(sch_reportcard)

#Download GIS data from DPI openmaps data then import below
dpi_crs <- list(epsg = 4326,
                proj4string = "+proj=longlat +datum=WGS84")
class(dpi_crs) <- "crs"

elem2018      <- st_read("data-raw/elem2018/WI_Elementary_School_Districts_2018.shp") %>%
  mutate(DISTRICT_TYPE = "Elementary") %>%
  mutate_if(is.factor, as.character)
unified2018   <- st_read("data-raw/unified2018/WI_Unified_School_Districts_2018.shp") %>%
  mutate(DISTRICT_TYPE = "Unified") %>%
  mutate_if(is.factor, as.character)
secondary2018 <- st_read("data-raw/secondary2018/WI_Secondary_School_Districts_2018.shp") %>%
  mutate(DISTRICT_TYPE = "Secondary") %>%
  mutate_if(is.factor, as.character)

dist_maps <- rbind(rbind(unified2018, elem2018), secondary2018) %>%
  st_transfrom(dpi_crs) %>%
  ms_simplify(keep = 0.05)


use_data(dist_maps)


