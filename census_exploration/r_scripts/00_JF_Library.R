# 00_JF_Library.R
# Author: Janice Chen
# Desc: functions used often
# Initial Date: 10/18/2021
# Last Modified: 10/18/2021

library(tidycensus) # for collecting census data
library(tigris) # census geometries
library(tidyverse) # data wrangling
library(sf) # geocomputation 
library(mapdeck) # mapping in mapbox
library(colourvalues)


# RETRIEVING GEOGRAPHIES OF THE PNW
pnw_states <- function() {
  df <- states(cb = TRUE) %>%
    filter(NAME == "Idaho"| NAME == "Oregon"| NAME == "Washington")
  
  assign("states", df, envir = .GlobalEnv)
}


pnw_tracts <- function() {
  or_tracts <- tracts(state = "OR") %>%
    select(c(4))
  wa_tracts <- tracts(state = "WA") %>%
    select(4) 
  id_tracts <- tracts(state = "ID") %>%
    select(4)
  
  tracts <- rbind(or_tracts, wa_tracts, id_tracts)
  
  assign("tracts", tracts, envir = .GlobalEnv)
}

pnw_counties <- function() {
  counties <- counties(state = c("OR", "WA", "ID")) 
  # 
  assign("counties", counties, envir = .GlobalEnv)
}

# MAPPING
polygons <- function(token, df, fill, palette, title) {
  mapdeck(token = token, style = mapdeck_style("light"), zoom = 4, location = c(-118.16054641317953, 45.3390081860396)) %>%
    add_polygon(
      layer_id = "data-layer",
      data = df,
      fill_colour = fill,
      fill_opacity = .4,
      legend = T,
      palette = palette,
      update_view = FALSE,
      legend_options = list(title = title)
    ) 
}

# OPACITY NOT TRIGGERING BECAUSE OF CERTAIN PALETTES — WORKS FOR VIRIDIS





