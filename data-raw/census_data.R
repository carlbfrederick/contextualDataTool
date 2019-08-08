library(usethis)
library(tidyverse)
library(tidycensus)
library(sf)
library(rmapshaper)

options(tigris_use_cache = TRUE)

thisyr <- 2017

dpi_crs <- list(epsg = 4326,
                proj4string = "+proj=longlat +datum=WGS84")
class(dpi_crs) <- "crs"

#Vars available at block-group level
wiBlkGrp <- get_acs(geography = "block group", year = thisyr, survey = "acs5",
                    geometry = TRUE, state = "WI", output = "wide",
                    variables = c("B23025_001", "B23025_005", "B23025_007",               #labor force 16 or over

                                  "B16004_002", "B16004_003", "B16004_004", "B16004_005",
                                  "B16004_006", "B16004_007", "B16004_008", "B16004_009",
                                  "B16004_010", "B16004_011", "B16004_012", "B16004_013",
                                  "B16004_014", "B16004_015", "B16004_016", "B16004_017",
                                  "B16004_018", "B16004_019", "B16004_020", "B16004_021",
                                  "B16004_022", "B16004_023",                             #language spoken at home for 5-17 year-olds

                                  "B28005_002", "B28005_003", "B28005_004", "B28005_005",
                                  "B28005_006", "B28005_007",                             #computer & internet at home under 18

                                  "B08303_001", "B08303_002", "B08303_003", "B08303_004",
                                  "B08303_005", "B08303_006", "B08303_007", "B08303_008",
                                  "B08303_009", "B08303_010", "B08303_011", "B08303_012",
                                  "B08303_013",                                           #travel time to work

                                  "B25002_001", "B25002_002", "B25002_003",               #Occupancy Status (HH - vacant vs occupied)
                                  "B25003_001", "B25003_002", "B25003_003"                #Tenure
                    )) %>%
  transmute(
    GEOID              = GEOID,

    lf_unemployed      = B23025_005E / B23025_001E,
    lf_nilf            = B23025_007E / B23025_001E,
    lf_disconneted     = lf_nilf + lf_unemployed,

    la_anyspanish      = B16004_004E / B16004_002E,
    la_anyindoeuro     = B16004_009E / B16004_002E,
    la_anyasianpac     = B16004_014E / B16004_002E,
    la_anyother        = B16004_019E / B16004_002E,
    la_anynoteng       = la_anyspanish + la_anyindoeuro + la_anyasianpac + la_anyother,
    la_notwellspanish  = (B16004_007E + B16004_008E) / B16004_002E,
    la_notwellindoeuro = (B16004_012E + B16004_013E) / B16004_002E,
    la_notwellasianpac = (B16004_017E + B16004_018E) / B16004_002E,
    la_notwellother    = (B16004_022E + B16004_023E) / B16004_002E,
    la_notwellnoteng   = la_notwellspanish + la_notwellindoeuro + la_notwellasianpac + la_notwellother,

    it_nocomputer      = B28005_007E / B28005_002E,
    it_nointernet      = (B28005_006E + B28005_007E) / B28005_002E,
    it_nobroadband     = (B28005_004E + B28005_006E + B28005_007E) / B28005_002E,
    it_broadband       = B28005_005E / B28005_002E,

    dr_avgcommute      = ((  2.5 * B08303_002E) +
                          (  7.0 * B08303_003E) +
                          ( 12.0 * B08303_004E) +
                          ( 17.0 * B08303_005E) +
                          ( 22.0 * B08303_006E) +
                          ( 27.0 * B08303_007E) +
                          ( 32.0 * B08303_008E) +
                          ( 37.0 * B08303_009E) +
                          ( 42.0 * B08303_010E) +
                          ( 52.0 * B08303_011E) +
                          ( 74.5 * B08303_012E) +
                          (120.0 * B08303_013E)) / B08303_001E,
    dr_45minplus       = (B08303_011E + B08303_012E + B08303_013E) / B08303_001E,
    dr_60minplus       = (B08303_012E + B08303_013E) / B08303_001E,

    hh_owner           = B25003_002E / B25002_001E,
    hh_renter          = B25003_003E / B25002_001E,
    hh_vacant          = B25002_003E / B25002_001E,

    geometry           = geometry
  ) %>%
  sf::st_transform(dpi_crs) %>%
  mutate_if(is_numeric, ~if_else(is.nan(.), NA_real_, .)) %>%
  ms_simplify(keep = 0.05)

#Vars available at tract level only
wiTract <- get_acs(geography = "tract", year = thisyr, survey = "acs5",
                   geometry = TRUE, state = "WI", output = "wide",
                   variables = c("B15001_003", "B15001_004", "B15001_005", "B15001_006",
                                 "B15001_007", "B15001_008", "B15001_009", "B15001_010", #edu attainment age 18-24 male
                                 "B15001_044", "B15001_045", "B15001_046", "B15001_047",
                                 "B15001_048", "B15001_049", "B15001_050", "B15001_051", #edu attainment age 18-24 female

                                 "B09005_001", "B09005_002", "B09005_003", "B09005_004",
                                 "B09005_005", "B09005_006",                             #hh-type children under 18

                                 "B07001_003", "B07001_019", "B07001_035", "B07001_051",
                                 "B07001_067", "B07001_083",                             #residential mobility in past year for 5-17 year-olds

                                 "B06001_003", "B06001_015", "B06001_027", "B06001_039",
                                 "B06001_051",                                           #place of birth for 5-17 year-olds

                                 "B27001_003", "B27001_004", "B27001_005", "B27001_006",
                                 "B27001_007", "B27001_008",                             #Health Insurance coverage status 0-18 years male
                                 "B27001_031", "B27001_032", "B27001_033", "B27001_034",
                                 "B27001_035", "B27001_036"                              #Health Insurance coverage status 0-18 years female
                   )) %>%
  transmute(
    GEOID = GEOID,
    ed_lths    = (B15001_004E + B15001_005E + B15001_045E + B15001_046E) / (B15001_003E + B15001_044E),
    ed_hsplus  = (B15001_006E + B15001_007E + B15001_008E + B15001_009E + B15001_010E +
                  B15001_047E + B15001_048E + B15001_049E + B15001_050E + B15001_051E) /
                 (B15001_003E + B15001_044E),
    ed_baplus  = (B15001_009E + B15001_010E + B15001_050E + B15001_051E) / (B15001_003E + B15001_044E),

    fs_marriedpar = B09005_003E / B09005_001E,
    fs_singlepar  = (B09005_004E + B09005_005E) / B09005_001E,
    fs_nonfamily  = B09005_006E / B09005_001E,

    rm_samehouse  = B07001_019E / B07001_003E,
    rm_outcounty  = (B07001_051E + B07001_067E + B07001_083E) / B07001_003E,
    rm_outstate   = (B07001_067E + B07001_083E) / B07001_003E,
    rm_outusa     = B07001_083E / B07001_003E,

    pb_samestate   = B06001_015E / B06001_003E,
    pb_foreignborn = B06001_051E / B06001_003E,

    hi_yesinsure   = (B27001_004E + B27001_007E + B27001_032E + B27001_035E) /
                     (B27001_003E + B27001_006E + B27001_031E + B27001_034E),

    hi_notinsure   = (B27001_005E + B27001_008E + B27001_033E + B27001_036E) /
                     (B27001_003E + B27001_006E + B27001_031E + B27001_034E),

    geometry = geometry
  ) %>%
  sf::st_transform(dpi_crs) %>%
  mutate_if(is_numeric, ~if_else(is.nan(.), NA_real_, .)) %>%
  ms_simplify(keep = 0.05)

#save data for package
use_data(wiBlkGrp)
use_data(wiTract)
