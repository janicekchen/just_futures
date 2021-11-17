## PIVOTING NHGIS-DOWNLOADED DATA ON HISPANIC POPULATION 1970-2010, JOINING to 2020 DATA AND TOTAL POPULATION DATA

library(dplyr)
library(tidyverse)

# READING IN HISPANIC DATA
data <- read.csv("data/raw/hispanic_1970-2010_tract/nhgis0004_ts_nominal_tract.csv")

data_tidy <- data %>%
  pivot_longer(cols = c(19:23), names_to = "year")

# extracting year from variable name (e.g. going from A35AA2010 to 2010)
data_tidy$year <- as.numeric(substr(data_tidy$year, 6, 10))

# creating 1 GJOIN column
data_tidy <- data_tidy %>%
  mutate(GJOIN = ifelse(year == "1970", GJOIN1970, 
                        ifelse(year == "1980", GJOIN1980,
                               ifelse(year == "1990", GJOIN1990,
                                      ifelse(year == "2000", GJOIN2000, GJOIN2010)))))

# selecting relevant columnsm
data_tidy <- data_tidy %>%
  select(c(1, 7:8, 10:11, year, value, GJOIN))

# filtering where there is GJOIN
data_tidy <- data_tidy %>%
  filter(GJOIN != "")


# READING IN TOTAL POPULATION DATA
total_pop <- read.csv("data/raw/totalpop_1970-2010_tract/nhgis0008_ts_nominal_tract.csv")

# pivoting and selecting relevant columns
pop_tidy <- total_pop %>% 
  pivot_longer(cols = c(21:25), names_to = "year") %>%
  select(c(1:6, 8:9, 11:12, 23:24))

pop_tidy$year <- as.numeric(substr(pop_tidy$year, 6, 10)) # clipping year name and making numeric

# changing "value" column name in preparation for join
names(pop_tidy)[12] <- "total_pop"

pop_tidy <- pop_tidy %>%
  mutate(GJOIN = ifelse(year == "1970", GJOIN1970, 
                        ifelse(year == "1980", GJOIN1980,
                               ifelse(year == "1990", GJOIN1990,
                                      ifelse(year == "2000", GJOIN2000, GJOIN2010)))))

pop_tidy <- pop_tidy %>%
  select(c(NHGISCODE, STATE, STATEFP, COUNTY, COUNTYFP, GJOIN, year, total_pop))

# selecting relevant columns in preparation for join 
pop_tidy_join <- pop_tidy %>%
  select(c(NHGISCODE, year, total_pop))

# LEFT JOINING BECAUSE DON'T NEED DATA FOR WHERE THERE IS NO HISPANIC POPULATION
latinx_forexport <- data_tidy %>%
  left_join(pop_tidy_join, by = c("year", "NHGISCODE"))

write.csv(latinx_forexport, "data/processed/latinx_1970-2010_tidy_tract.csv")
write.csv(pop_tidy, "data/processed/totalpop_1970-2010_tidy_tract.csv")

# filter by year so you don't have to do it in QGIS : )
df <- latinx_forexport %>%
  filter(year == 1970)


years <- c(1970, 1980, 1990, 2000, 2010)

latinx_forexport %>%
  filter(year == 1970) %>%
  write.csv("data/processed/latinx_1970_tidy_tract.csv")

latinx_forexport %>%
  filter(year == 1980) %>%
  write.csv("data/processed/latinx_1980_tidy_tract.csv")

latinx_forexport %>%
  filter(year == 1990) %>%
  write.csv("data/processed/latinx_1990_tidy_tract.csv")

latinx_forexport %>%
  filter(year == 2000) %>%
  write.csv("data/processed/latinx_2000_tidy_tract.csv")

latinx_forexport %>%
  filter(year == 2010) %>%
  write.csv("data/processed/latinx_2010_tidy_tract.csv")

#idk why this for loop isn't working
# lapply(years, function(year) {
#   latinx_forexport %>%
#     filter(year == year) %>%
#     write.csv(paste0("data/processed/latinx_", year, "_tidy_tract.csv"))
# })
