# 01_JF_CensusExploration.R 
# Author: Janice Chen
# Desc: gathering data under relevant topics such as race, ethnicity, gender, language, workforce from 1990, 2000, 2010, and 2020 censuses.
# Initial Date: 09/29/2021
# Last Modified: 10/06/2021

library(tidycensus) # for interfacing with Census API
library(tigris) # for retrieving Census boundaries
library(tidyverse) # for data wrangling & visualization
library(geojsonsf) # for exporting sf to geojson
library(geojsonio) # for exporting geojson to local
library(sf) # geocomputation
library(mapdeck) # for visualizing with Mapbox API

# RUN THESE NEXT TWO LINES OF CODE IF THIS IS FIRST TIME WORKING WITH TIDYCENSUS
# key <-"[INSERT CENSUS API KEY HERE]"
# census_api_key(key, install = TRUE)

# storing file path for this section of the project
file_path <- "Topics/General/GIS/"

years = c(2000, 2010, 2020) # 1990 decennial Census endpoint has been removed by the Census Bureau
# 
# loading variable lists from all three years
var_allYears = lapply(years, function(year) {
  load_variables(year, "pl")
})

# Idk what sf1 really means yet 0930
var_2000_sf1 = load_variables(2000, "sf1")
var_2010_sf1 = load_variables(2010, "sf1")

# separating all variables into independent data frames
for (i in 1:length(var_allYears)) {
  df <- var_allYears[[i]]
  name <- paste0("var_", years[i])
  assign(name, df, envir = .GlobalEnv)
}

# creating table to store race variables
race_variables <- c("totalWhite", "totalBlack", "totalAIAN", "totalAsian", "totalNHPI", "totalSOR")
race_varTable <- data.frame(id = 3:8, variable_name = race_variables)

# first retrieving numbers on how much of population identifies as "single race"
race_singlerace <- race_varTable %>%
  slice(2)
singlerace_2000 <- get_decennial(geography = "tract",
                                 variable = "PL001002",
                                 year = 2000,
                                 sumfile = "pl",
                                 state = c("OR", "WA", "ID"),
                                 summary_var = "PL001001") %>%
  mutate(value_norm = value / summary_value)

# downloading race data for those years 
for (year in years) {
  if (year == 2000) {
    vars <- paste0("PL00100", race_varTable$id)
    summary_var <- "PL001002"
  } else if (year == 2010) {
    vars <- paste0("P00100", race_varTable$id)
    summary_var <- "P001002"
  }
    else {
    vars <- paste0("P1_00", race_varTable$id, "N")
    summary_var <- "P1_002N"
  }
    temp_df <- get_decennial(geography = "block group",
                             variables = vars,
                             year = year,
                             sumfile = "pl",
                             state = c("OR", "WA", "ID"),
                             summary_var = summary_var)
    obj_name <- paste0("race_", year)
    assign(obj_name, temp_df, envir = .GlobalEnv)
}

# pivoting tables to wide format (for webgeoda)
race_2000 <- race_2000 %>%
  pivot_wider(names_from = variable, values_from = value)

race_2010 <- race_2010 %>%
  pivot_wider(names_from = variable, values_from = value)

race_2020 <- race_2020 %>%
  pivot_wider(names_from = variable, values_from = value)

# renaming cols
names(race_2000)[4:9] <- race_varTable$variable_name
names(race_2010)[4:9] <- race_varTable$variable_name
names(race_2020)[4:9] <- race_varTable$variable_name

# exporting as CSVs

write.csv(race_2000, paste0(file_path, "data/processed/race_2000.csv"))
write.csv(race_2010, paste0(file_path, "data/processed/race_2010.csv"))
write.csv(race_2020, paste0(file_path, "data/processed/race_2020.csv"))

# retrieving geometry
tracts <- tracts(state = c("OR", "WA", "ID"))

# exporting geometry
geojson_write(tracts, geometry = "polygon", file = paste0(file_path, "data/processed/census_tracts_2019.geojson"))

# getting block data for 2020
race_2020_bg <- get_decennial(geography = "block group",
                              variables = paste0("P1_00", race_varTable$id, "N"),
                              year = 2020,
                              sumfile = "pl",
                              state = c("OR", "WA", "ID"),
                              summary_var = "P1_002N",
                              geometry = TRUE)

race_varTable <- race_varTable %>%
  mutate(var_2020 = paste0("P1_00", id, "N")) # adding 2020 variable column for joining to big data frame

race_2020_bg <- left_join(race_2020_bg, race_varTable[2:3], by = c("variable" = "var_2020"))

# NOTE 10/10: DOWNLOADING BY BLOCK WITH GEOMETRY YIELDS ERROR, CURRENTLY TESTING BY BLOCK GROUP

### MAPPING ###
mb_token <- "pk.eyJ1IjoiamFuaWNla2NoZW4iLCJhIjoiY2p3bnM0cWhxMGRoazN5cGw1dTV3Ymd5NCJ9.8lUlOe5D3txI8QgZXx19dw"

# testing with one variable (white)
white_2020 <- race_2020_bg %>%
  filter(variable_name == "totalWhite")
  
mapdeck(token = mb_token, style = mapdeck_style("dark")) %>%
  add_polygon(
    data = white_2020,
    fill_colour = "value",
    fill_opacity = 0.7
  )


race_split <- race_2020_bg %>%
  split(.$variable_name)

generate_samples <- function(data) {
  suppressMessages(st_sample(data, size = round(data$value / 50)))
} 

# 
# points <- map(race_split, generate_samples)

# ^ this is returning error that st_sample cannot operate on null geometries....trying to filter out empty geometries

race_2020_test <- race_2020_bg %>%
  filter(!st_is_empty(geometry))

race_split_nonull <- race_2020_test %>%
  split(.$variable_name)

points <- map(race_split_nonull, generate_samples)

points_test <- imap(points, 
               ~st_sf(data.frame(race = rep(.y, length(.x))),
                      geometry = .x))


points_test <- do.call(rbind, points_test)

points_test <- points_test %>% group_by(race) %>% summarise()
                             