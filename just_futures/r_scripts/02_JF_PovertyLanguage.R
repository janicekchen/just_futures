# 02_JF_PovertyLanguage.R
# Author: Janice Chen
# Desc: mapping of ACS variables
# Initial Date: 10/11/2021
# Last Modified: 10/20/2021

library(tidycensus) # for collecting census data
library(tigris) # census geometries
library(tidyverse) # data wrangling
library(sf) # geocomputation 
library(mapdeck) # mapping in mapbox
library(colourvalues) # for color palettes
library(htmlwidgets)

source("r_scripts/00_JF_Library.R")

vars <- load_variables(2019, "acs5")

poverty_vars <- data.frame(language = c(rep.int("spanish", 2),
                                        rep.int("indo-eur", 2),
                                        rep.int("asian-pi", 2),
                                        rep.int("other", 2)),
                           var = c("B16009_005",
                                   "B16009_011",
                                   "B16009_006", 
                                   "B16009_012",
                                   "B16009_007",
                                   "B16009_013",
                                   "B16009_008",
                                   "B16009_014"))

total_vars <- data.frame(language = c(rep.int("spanish", 3),
                                      rep.int("indo-eur", 3),
                                      rep.int("asian-pi", 3),
                                      rep.int("other", 3)),
                         var = c("B16004_004",
                                 "B16004_026",
                                 "B16004_048",
                                 "B16004_009",
                                 "B16004_031",
                                 "B16004_053",
                                 "B16004_014",
                                 "B16004_036",
                                 "B16004_058",
                                 "B16004_019",
                                 "B16004_041",
                                 "B16004_063"
                                 ))

# gathering data for poverty & language variables (poverty by language spoken at home, irrespective of English level)
poverty_acs <- get_acs(geography = "tract",
               variables = poverty_vars$var,
               output = "tidy",
               state = c("OR", "WA", "ID"),
               geometry = FALSE,
               survey = "acs5", 
               summary_var = "B16009_002")

# gathering data for language in the total population
total_acs <- get_acs(geography = "tract",
                     variables = total_vars$var,
                     output = "tidy",
                     state = c("OR", "WA", "ID"),
                     geomegry = FALSE,
                     survey = "acs5",
                     summary_var = "B16004_001")

# joining languages to variable names and then aggregating by language (bc tables currently separated by age group)
poverty_acs_agg <- poverty_acs %>%
  left_join(poverty_vars, by = c("variable" = "var")) %>%
  group_by(language, GEOID, NAME) %>%
  summarise(estimate = sum(estimate), moe = sum(moe), summary_est = first(summary_est)) %>%
  mutate(est_share = estimate/summary_est * 100) # also adding normalized column

total_acs_agg <- total_acs %>%
  left_join(total_vars, by = c("variable" = "var")) %>%
  group_by(language, GEOID, NAME) %>%
  summarise(estimate = sum(estimate), moe = sum(moe), summary_est = first(summary_est)) %>%
  mutate(est_share = estimate/summary_est * 100) # also adding normalized column

  
# joining total population data to poverty-language to calculate disproportionate representation of certain language speakers below poverty threshold
dispro_poverty <- left_join(poverty_acs_agg, total_acs_agg[c(1:2, 7)], by = c("GEOID", "language"))

# renaming columns
names(dispro_poverty)[7:8] <- c("poverty_est_share", "total_est_share")

# adding column that calculates difference between the estimates 
dispro_poverty <- dispro_poverty %>%
  mutate(share_diff = poverty_est_share - total_est_share)

# downloading tract and state geometry
pnw_tracts()
pnw_states()

# joining geometry to data
dispro_poverty <- left_join(dispro_poverty, tracts) %>%
  st_as_sf(sf_column_name = "geometry")

asianpi_dispro <- dispro_poverty %>%
  filter(language == "asian-pi")

mb_token <- "pk.eyJ1IjoiamFuaWNla2NoZW4iLCJhIjoiY2p3bnM0cWhxMGRoazN5cGw1dTV3Ymd5NCJ9.8lUlOe5D3txI8QgZXx19dw"

polygons(token = mb_token, df = asianpi_dispro, fill = "share_diff", palette = colourvalues::get_palette("rdylgn")[11:1,], title = "Share Diff.") 

# mapdeck(token = mb_token, style = mapdeck_style("dark"), zoom = 4, location = c(-118.16054641317953, 45.3390081860396)) %>%
#   add_polygon(
#     data = asianpi_dispro, # data table
#     fill_colour = "share_diff", # variable name goes here
#     fill_opacity = 0.5,
#     legend = T,
#     palette = "magma", # reversing color scale
#     update_view = FALSE,
#     legend_options = list(title =  "share")
#   )



spanish_dispro <- dispro_poverty %>%
  filter(language == "spanish")


polygons(mb_token, df = spanish_dispro, fill = "share_diff", palette = colourvalues::get_palette("rdylgn")[11:1,], title = "Share Diff.")


# MAPPING LANGUAGE
# using total_acs_agg table from above
# joining to geometry
lang_acs <- left_join(total_acs_agg, tracts) %>%
  st_as_sf()

# creating list of languages to loop through
lang_list <- unique(total_acs_agg$language)

for (lang in lang_list) {
  filt_df <- lang_acs %>%
    filter(language == lang)

  map <- polygons(mb_token, df = filt_df, fill = "est_share", palette = "magma")
  saveWidget(map, file = paste0("maps/00_census_october/", lang, "_acs2019.html"))

}


# RACE
# retrieving variables from decennial census
# white, black, asian, native hawaiian / PI, native american / alaska native, hispanic, other

census_vars <- load_variables(2020, "pl")
race_vars <- data.frame(race = c("white", "black", "aina", "asian", "nhpi","other"), 
                        var_name = c("P1_003N",
                                     "P1_004N",
                                     "P1_005N",
                                     "P1_006N",
                                     "P1_007N",
                                     "P1_008N"))

ethnicity_vars <- data.frame(race = "hispanic", var_name = "P2_002N" )

#P1_002N summary var for race (total onerace pop)
# P2_001N summary var for hisapnic (total pop)

race_dec <- tidycensus::get_decennial(
                           geography = "tract",
                           geometry = TRUE,
                           year = 2020,
                           variables = "P1_003N",
                           summary_var = "P1_002N")

?get_decennial
# where place have become more X in the last decade??


