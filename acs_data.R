# Libraries -----------------------------------------------------------------
library(tidyverse)
library(psrccensus)
library(psrcelmer)
library(tidycensus)
library(sf)
library(units)

source("functions.R")

# Inputs ------------------------------------------------------------------

blockgroup_splits <- readRDS("data/blockgroup_splits.rds")
tract_splits <- readRDS("data/tract_splits.rds")

# Data Years
pre_api_years <- c(2012)
api_years <- c(2017, 2022)
analysis_years <- c(pre_api_years, api_years)

# Make sure these match the boundary definition you want the data to be based on
rgc_title <- "Regional Growth Center (4/23/2024)"
school_title <- "Planning Academy School 2024"

# Coordinate Reference Systems
wgs84 <- 4326
spn <- 2285

# Silence the dplyr summarize message
options(dplyr.summarise.inform = FALSE)
acs_pre2013_bg_dir <- "X:/DSA/shiny-uploads/acs_blockgroups_pre2013"

# Factor Levels
county_order <- c("Region", "King County", "Kitsap County", "Pierce County", "Snohomish County")

age_order <- c("Under 17", "18 to 34", "35 to 49", "50 to 64", "65+", "Total")

race_order <- c("American Indian\nand Alaska\nNative", "Asian", 
                "Black or African\nAmerican", "Hispanic or\nLatino", 
                "Native Hawaiian\nand Other\nPacific Islander",
                "Other", "Non-hispanic\nWhite", "Total")
income_order <- c("Less than\n$20,000", "$20,000 to\n$35,000", "$35,000 to\n$50,000",
                  "$50,000 to\n$75,000", "$75,000 to\n$100,000", "$100,000 to\n$150,000",
                  "$150,000 to\n$200,000", "$200,000 or\nmore", "Total")

tenure_order <- c("Renter", "Owner", "Total")

structure_order <- c("SF detached", "Moderate-low\ndensity", "Moderate-high\ndensity",
                     "High density", "Other", "Total")

burden_order <- c("Not cost\nburdened (<30%)", "Cost burdened\n(30-49.9%)",
                  "Severely cost\nburdened (50+%)", "Not computed", "Total")

education_order <- c("No high school\ndiploma", "High school", "Some college",
                     "Bachelor’s\ndegree","Graduate degree", "Total")

mode_order <- c("Drove\nAlone", "Carpooled", "Transit", "Bike", "Walk","Work from\nHome", "Other", "Total")

vehicle_order <- c("No vehicles","1 vehicle", "2 vehicles", "3 vehicles", "4 or more vehicles", "Total")

# Census Tables
age_table <- "B01001"
race_table <- "B03002"
income_table <- "B19001"
tenure_table <- "B25003"
structure_table <- "B25024"
renter_burden_table <- "B25070"
owner_burden_table <- "B25091"
education_table <- "B15002"
mode_table <- "B08301"
vehicle_table <- "B08201"

# Center Shapefiles -------------------------------------------------------
rgc_shape <- st_read_elmergeo(layer_name = "urban_centers") |>
  st_transform(crs = spn) |>
  mutate(name = gsub("Bellevue", "Bellevue Downtown", name)) |>
  mutate(name = gsub("Redmond-Overlake", "Redmond Overlake", name)) |>
  mutate(name = gsub("Greater Downtown Kirkland", "Kirkland Greater Downtown", name)) |>
  select("name", "category", "acres") |>
  arrange(name)

rgc_names <- rgc_shape |> 
  select("name") |> 
  st_drop_geometry() |> 
  pull() |>
  unique() |>
  sort()

saveRDS(rgc_shape, "data/rgc_shape.rds")

# School Shapefiles -------------------------------------------------------
school_shape <- st_read("X:/DSA/shiny-uploads/planning-academy/summer_planning_academy_schools.shp") |> 
  st_transform(spn) 

school_shape <- school_shape |>
  mutate(acres = as.numeric(set_units(st_area(school_shape), "acre"))) |>
  select(name = "school", category = "district", "acres") |>
  arrange(category, name)

school_names <- school_shape |> 
  select("name") |> 
  st_drop_geometry() |> 
  pull() |>
  unique() |>
  sort()

saveRDS(school_shape, "data/school_shape.rds")

# Age Distribution --------------------------------------------------------
age_lookup <- data.frame(variable = c("B01001_002",
                                      "B01001_003", "B01001_004","B01001_005","B01001_006",
                                      "B01001_007", "B01001_008", "B01001_009", "B01001_010", "B01001_011", "B01001_012",
                                      "B01001_013", "B01001_014", "B01001_015",
                                      "B01001_016", "B01001_017", "B01001_018", "B01001_019",
                                      "B01001_020", "B01001_021", "B01001_022", "B01001_023", "B01001_024", "B01001_025",
                                      "B01001_026",
                                      "B01001_027", "B01001_028","B01001_029","B01001_030",
                                      "B01001_031", "B01001_032", "B01001_033", "B01001_034", "B01001_035", "B01001_036",
                                      "B01001_037", "B01001_038", "B01001_039",
                                      "B01001_040", "B01001_041", "B01001_042", "B01001_043",
                                      "B01001_044", "B01001_045", "B01001_046", "B01001_047", "B01001_048", "B01001_049"),
                         gender = c("Male",
                                    "Male", "Male", "Male", "Male",
                                    "Male", "Male", "Male", "Male", "Male", "Male",
                                    "Male", "Male", "Male",
                                    "Male", "Male", "Male", "Male",
                                    "Male", "Male", "Male", "Male", "Male", "Male",
                                    "Female",
                                    "Female", "Female", "Female", "Female",
                                    "Female", "Female", "Female", "Female", "Female", "Female",
                                    "Female", "Female", "Female",
                                    "Female", "Female", "Female", "Female",
                                    "Female", "Female", "Female", "Female", "Female", "Female"),
                         grouping = c("Total",
                                      "Under 17", "Under 17", "Under 17", "Under 17",
                                      "18 to 34", "18 to 34", "18 to 34", "18 to 34", "18 to 34", "18 to 34",
                                      "35 to 49", "35 to 49", "35 to 49",
                                      "50 to 64", "50 to 64", "50 to 64", "50 to 64",
                                      "65+", "65+", "65+", "65+", "65+", "65+",
                                      "Total",
                                      "Under 17", "Under 17", "Under 17", "Under 17",
                                      "18 to 34", "18 to 34", "18 to 34", "18 to 34", "18 to 34", "18 to 34",
                                      "35 to 49", "35 to 49", "35 to 49",
                                      "50 to 64", "50 to 64", "50 to 64", "50 to 64",
                                      "65+", "65+", "65+", "65+", "65+", "65+"))

# County
county <- get_acs_recs(geography="county", table.names = age_table, years = analysis_years, acs.type = 'acs5') 

county <- left_join(county, age_lookup, by=c("variable")) |>
  filter(variable != "B01001_001") |>
  select(geography="name", "estimate", "moe", "year", "gender", "grouping") |>
  group_by(geography, year, grouping) |>
  summarise(estimate = sum(estimate)) |>
  as_tibble() |>
  mutate(concept = "Population by Age Group")

totals <- county |> filter(grouping == "Total") |> select("geography", "year", "concept", total="estimate")

county <- left_join(county, totals, by=c("geography", "year", "concept")) |>
  mutate(share = estimate / total) |>
  select(-"total") |>
  mutate(geography_type = "County") |>
  mutate(geography = factor(geography, levels = county_order)) |>
  mutate(grouping = factor(grouping, levels = age_order)) |>
  mutate(year = as.character(year)) |>
  arrange(geography, grouping, year)

rm(totals)

# Blockgroup 2013 onward
bg_post_2013 <- get_acs_recs(geography="block group", table.names = age_table, years = api_years, acs.type = 'acs5') 

bg_post_2013 <- left_join(bg_post_2013, age_lookup, by=c("variable")) |>
  filter(variable != "B01001_001") |>
  select(geoid="GEOID", "estimate", "moe", "year", "gender", "grouping") |>
  group_by(geoid, year, grouping) |>
  summarise(estimate = sum(estimate)) |>
  as_tibble() |>
  mutate(concept = "Population by Age Group") |>
  mutate(geography_type = "Blockgroup") |>
  mutate(grouping = factor(grouping, levels = age_order)) |>
  mutate(year = as.character(year)) |>
  arrange(geoid, grouping, year)

# Blockgroup pre 2013
bg_pre_2013 <- NULL
for (y in pre_api_years) {
  age_bg <- file.path(acs_pre2013_bg_dir, paste0("acs_",y,"5_", age_table, "_150.xlsx"))
  working <- readxl::read_excel(age_bg, sheet=paste0("ACS_",y,"5_", age_table, "_150"), skip=7) |>
    mutate(geoid = as.character(str_remove_all(`Geographic Identifier`, "15000US"))) |>
    select(-"Geographic Identifier",-"Table ID", -"Title", -"Universe", -"Summary Level Type", -"Summary Level", -"Area Name", -"ACS Dataset") |>
    pivot_longer(!geoid, names_to = "label", values_to = "estimate") |>
    filter(!(str_detect(label, "Margin of Error"))) |>
    mutate(label = str_remove_all(label, "\\[Estimate\\] ")) |>
    mutate(label = str_remove_all(label, "\\...[0-9][0-9]")) |>
    mutate(grouping = case_when(
      # Total Population
      str_detect(label,"Total") ~ "Total",
      # Under 17
      str_detect(label,"Under 5 Years") ~ "Under 17",
      str_detect(label,"5 To 9 Years") ~ "Under 17",
      str_detect(label,"10 To 14 Years") ~ "Under 17",
      str_detect(label,"15 To 17 Years") ~ "Under 17",
      # 18 to 34
      str_detect(label,"18 And 19 Years") ~ "18 to 34",
      str_detect(label,"20 Years") ~ "18 to 34",
      str_detect(label,"21 Years") ~ "18 to 34",
      str_detect(label,"22 To 24 Years") ~ "18 to 34",
      str_detect(label,"25 To 29 Years") ~ "18 to 34",
      str_detect(label,"30 To 34 Years") ~ "18 to 34",
      # 35 to 49
      str_detect(label,"35 To 39 Years") ~ "35 to 49",
      str_detect(label,"40 To 44 Years") ~ "35 to 49",
      str_detect(label,"45 To 49 Years") ~ "35 to 49",
      # 50 to 65
      str_detect(label,"50 To 54 Years") ~ "50 to 64",
      str_detect(label,"55 To 59 Years") ~ "50 to 64",
      str_detect(label,"60 And 61 Years") ~ "50 to 64",
      str_detect(label,"62 To 64 Years") ~ "50 to 64",
      # over 65
      str_detect(label,"65 And 66 Years") ~ "65+",
      str_detect(label,"67 To 69 Years") ~ "65+",
      str_detect(label,"70 To 74 Years") ~ "65+",
      str_detect(label,"75 To 79 Years") ~ "65+",
      str_detect(label,"80 To 84 Years") ~ "65+",
      str_detect(label,"85 Years And Over") ~ "65+")) |>
    drop_na() |>
    mutate(county = substring(geoid, 1, 5)) |>
    filter(county %in% c("53033", "53035", "53053", "53061")) |>
    group_by(geoid, grouping) |>
    summarise(estimate = sum(estimate)) |>
    as_tibble() |>
    mutate(concept = "Population by Age Group") |>
    mutate(geography_type = "Blockgroup") |>
    mutate(grouping = factor(grouping, levels = age_order)) |>
    mutate(year = as.character(y)) |>
    arrange(geoid, grouping, year)
  
  if(is.null(bg_pre_2013)) {bg_pre_2013=working} else {bg_pre_2013=bind_rows(bg_pre_2013, working)}
  rm(working)
  
}

# Combine Blockgroup Data
blockgroups <- bind_rows(bg_pre_2013, bg_post_2013) |> arrange(geoid, grouping, year)
rm(bg_pre_2013, bg_post_2013)

# Regional Growth Centers
places <- NULL
for(place in c(rgc_names, "All RGCs")) {
  
  df <- geography_estimate_from_bg(geography_type = rgc_title, split_type = "percent_of_total_pop", geography_name=place)
  ifelse(is.null(places), places <- df, places <- bind_rows(places, df))
  rm(df)
  
}

# Planning Academy Schools
for(place in c(school_names, "All Schools")) {
  
  df <- geography_estimate_from_bg(geography_type = school_title, split_type = "percent_of_total_pop", geography_name=place)
  places <- bind_rows(places, df)
  rm(df)
  
}

population_by_age <- bind_rows(county, places)
ord <- unique(c(county_order, "All RGCs", rgc_names, "All Schools", school_names))
population_by_age <- population_by_age |> mutate(geography = factor(geography, levels = ord)) |> arrange(geography, grouping, year)
saveRDS(population_by_age, "data/population_by_age.rds")
rm(age_lookup, places, blockgroups, county)

# Race & Ethnicty ---------------------------------------------------------
race_lookup <- data.frame(variable = c("B03002_001",
                                       "B03002_003",
                                       "B03002_004",
                                       "B03002_005",
                                       "B03002_006",
                                       "B03002_007",
                                       "B03002_008", "B03002_009",
                                       "B03002_012"),
                          grouping = c("Total",
                                       "Non-hispanic White",
                                       "Black or African American",
                                       "American Indian and Alaska Native",
                                       "Asian",
                                       "Native Hawaiian and Other Pacific Islander", 
                                       "Other", "Other",
                                       "Hispanic or Latino"))

county <- get_acs_recs(geography="county", table.names = race_table, years = c(pre_api_years, api_years), acs.type = 'acs5') 

county <- left_join(county, race_lookup, by=c("variable")) |>
  filter(!(is.na(grouping))) |>
  select(geography="name", "estimate", "year", "grouping") |>
  group_by(geography, year, grouping) |>
  summarise(estimate = sum(estimate)) |>
  as_tibble() |>
  mutate(concept = "Population by Race & Ethnicity")

totals <- county |> filter(grouping == "Total") |> select("geography", "year", "concept", total="estimate")

county <- left_join(county, totals, by=c("geography", "year", "concept")) |>
  mutate(share = estimate / total) |>
  select(-"total") |>
  mutate(geography_type = "County") |>
  mutate(grouping = str_wrap(grouping, width=16)) |>
  mutate(geography = factor(geography, levels = county_order)) |>
  mutate(grouping = factor(grouping, levels = race_order)) |>
  mutate(year = as.character(year)) |>
  arrange(geography, grouping, year)

rm(totals)

# Blockgroup 2013 onward
bg_post_2013 <- get_acs_recs(geography="block group", table.names = race_table, years = api_years, acs.type = 'acs5') 

bg_post_2013 <- left_join(bg_post_2013, race_lookup, by=c("variable")) |>
  filter(!(is.na(grouping))) |>
  select(geoid="GEOID", "estimate", "year", "grouping") |>
  group_by(geoid, year, grouping) |>
  summarise(estimate = sum(estimate)) |>
  as_tibble() |>
  mutate(concept = "Population by Race & Ethnicity") |>
  mutate(geography_type = "Blockgroup") |>
  mutate(grouping = str_wrap(grouping, width=16)) |>
  mutate(grouping = factor(grouping, levels = race_order)) |>
  mutate(year = as.character(year)) |>
  arrange(geoid, grouping, year)

# Blockgroup pre 2013
bg_pre_2013 <- NULL
for (y in pre_api_years) {
  race_bg <- file.path(acs_pre2013_bg_dir, paste0("acs_",y,"5_", race_table, "_150.xlsx"))
  working <- readxl::read_excel(race_bg, sheet=paste0("ACS_",y,"5_", race_table, "_150"), skip=7) |>
    mutate(geoid = as.character(str_remove_all(`Geographic Identifier`, "15000US"))) |>
    select(-"Geographic Identifier",-"Table ID", -"Title", -"Universe", -"Summary Level Type", -"Summary Level", -"Area Name", -"ACS Dataset") |>
    pivot_longer(!geoid, names_to = "label", values_to = "estimate") |>
    filter(!(str_detect(label, "Margin of Error"))) |>
    mutate(label = str_remove_all(label, "\\[Estimate\\] ")) |>
    mutate(label = str_remove_all(label, "\\...[0-9][0-9]")) |>
    mutate(grouping = case_when(
      # Total Population
      str_detect(label,"Total") ~ "Total",
      str_detect(label,"White Alone") ~ "Non-hispanic White",
      str_detect(label,"Black Or African American Alone") ~ "Black or African American",
      str_detect(label,"American Indian And Alaska Native Alone") ~ "American Indian and Alaska Native",
      str_detect(label,"Asian Alone") ~ "Asian",
      str_detect(label,"Native Hawaiian And Other Pacific Islander Alone") ~ "Native Hawaiian and Other Pacific Islander",
      str_detect(label,"Some Other Race Alone") ~ "Other",
      str_detect(label,"Two Or More Races") ~ "Other",
      label == "Hispanic Or Latino:" ~ "Hispanic or Latino")) |>
    drop_na() |>
    mutate(county = substring(geoid, 1, 5)) |>
    filter(county %in% c("53033", "53035", "53053", "53061")) |>
    group_by(geoid, grouping) |>
    summarise(estimate = sum(estimate)) |>
    as_tibble() |>
    mutate(concept = "Population by Race & Ethnicity") |>
    mutate(geography_type = "Blockgroup") |>
    mutate(grouping = str_wrap(grouping, width=16)) |>
    mutate(grouping = factor(grouping, levels = race_order)) |>
    mutate(year = as.character(y)) |>
    arrange(geoid, grouping, year)
  
  if(is.null(bg_pre_2013)) {bg_pre_2013=working} else {bg_pre_2013=bind_rows(bg_pre_2013, working)}
  rm(working)
  
}

# Combine Blockgroup Data
blockgroups <- bind_rows(bg_pre_2013, bg_post_2013) |> arrange(geoid, grouping, year)
rm(bg_pre_2013, bg_post_2013)

# Regional Growth Centers
places <- NULL
for(place in c(rgc_names, "All RGCs")) {
  
  df <- geography_estimate_from_bg(geography_type = rgc_title, split_type = "percent_of_total_pop", geography_name=place)
  ifelse(is.null(places), places <- df, places <- bind_rows(places, df))
  rm(df)
  
}

# Planning Academy Schools
for(place in c(school_names, "All Schools")) {
  
  df <- geography_estimate_from_bg(geography_type = school_title, split_type = "percent_of_total_pop", geography_name=place)
  places <- bind_rows(places, df)
  rm(df)
  
}

population_by_race <- bind_rows(county, places)
ord <- unique(c(county_order, "All RGCs", rgc_names, "All Schools", school_names))
population_by_race <- population_by_race |> mutate(geography = factor(geography, levels = ord)) |> arrange(geography, grouping, year)
saveRDS(population_by_race, "data/population_by_race.rds")
rm(race_lookup, places, blockgroups, county)

# Income ------------------------------------------------------------------
income_lookup <- data.frame(variable = c("B19001_001",
                                         "B19001_002", "B19001_003","B19001_004",
                                         "B19001_005", "B19001_006", "B19001_007", 
                                         "B19001_008", "B19001_009", "B19001_010",
                                         "B19001_011", "B19001_012",
                                         "B19001_013", 
                                         "B19001_014", "B19001_015",
                                         "B19001_016", 
                                         "B19001_017"),
                            grouping = c("Total",
                                         "Less than $20,000", "Less than $20,000", "Less than $20,000",
                                         "$20,000 to $35,000", "$20,000 to $35,000", "$20,000 to $35,000",
                                         "$35,000 to $50,000", "$35,000 to $50,000", "$35,000 to $50,000",
                                         "$50,000 to $75,000", "$50,000 to $75,000",
                                         "$75,000 to $100,000", 
                                         "$100,000 to $150,000", "$100,000 to $150,000",
                                         "$150,000 to $200,000", 
                                         "$200,000 or more"))

county <- get_acs_recs(geography="county", table.names = income_table, years = c(pre_api_years, api_years), acs.type = 'acs5') 

county <- left_join(county, income_lookup, by=c("variable")) |>
  filter(!(is.na(grouping))) |>
  select(geography="name", "estimate", "year", "grouping") |>
  group_by(geography, year, grouping) |>
  summarise(estimate = sum(estimate)) |>
  as_tibble() |>
  mutate(concept = "Households by Income")

totals <- county |> filter(grouping == "Total") |>select("geography", "year", "concept", total="estimate")

county <- left_join(county, totals, by=c("geography", "year", "concept")) |>
  mutate(share = estimate / total) |>
  select(-"total") |>
  mutate(geography_type = "County") |>
  mutate(grouping = str_wrap(grouping, width=11)) |>
  mutate(geography = factor(geography, levels = county_order)) |>
  mutate(grouping = factor(grouping, levels = income_order)) |>
  mutate(year = as.character(year)) |>
  arrange(geography, grouping, year)

rm(totals)

# Blockgroup 2013 onward
bg_post_2013 <- get_acs_recs(geography="block group", table.names = income_table, years = api_years, acs.type = 'acs5') 

bg_post_2013 <- left_join(bg_post_2013, income_lookup, by=c("variable")) |>
  filter(!(is.na(grouping))) |>
  select(geoid="GEOID", "estimate", "year", "grouping") |>
  group_by(geoid, year, grouping) |>
  summarise(estimate = sum(estimate)) |>
  as_tibble() |>
  mutate(concept = "Households by Income") |>
  mutate(geography_type = "Blockgroup") |>
  mutate(grouping = str_wrap(grouping, width=11)) |>
  mutate(grouping = factor(grouping, levels = income_order)) |>
  mutate(year = as.character(year)) |>
  arrange(geoid, grouping, year)

# Blockgroup pre 2013
bg_pre_2013 <- NULL
for (y in pre_api_years) {
  income_bg <- file.path(acs_pre2013_bg_dir, paste0("acs_",y,"5_", income_table, "_150.xlsx"))
  working <- readxl::read_excel(income_bg, sheet=paste0("ACS_",y,"5_", income_table, "_150"), skip=7) |>
    mutate(geoid = as.character(str_remove_all(`Geographic Identifier`, "15000US"))) |>
    select(-"Geographic Identifier",-"Table ID", -"Title", -"Universe", -"Summary Level Type", -"Summary Level", -"Area Name", -"ACS Dataset") |>
    pivot_longer(!geoid, names_to = "label", values_to = "estimate") |>
    filter(!(str_detect(label, "Margin of Error"))) |>
    mutate(label = str_remove_all(label, "\\[Estimate\\] ")) |>
    mutate(label = str_remove_all(label, "\\...[0-9][0-9]")) |>
    mutate(grouping = case_when(
      # Total Population
      str_detect(label,"Total") ~ "Total",
      # Under $20K
      str_detect(label,"Less Than \\$10,000") ~ "Less than $20,000",
      str_detect(label,"\\$10,000 To \\$14,999") ~ "Less than $20,000",
      str_detect(label,"\\$15,000 To \\$19,999") ~ "Less than $20,000",
      # $20k to $35k
      str_detect(label,"\\$20,000 To \\$24,999") ~ "$20,000 to $35,000",
      str_detect(label,"\\$25,000 To \\$29,999") ~ "$20,000 to $35,000",
      str_detect(label,"\\$30,000 To \\$34,999") ~ "$20,000 to $35,000",
      # $35k to $50k
      str_detect(label,"\\$35,000 To \\$39,999") ~ "$35,000 to $50,000",
      str_detect(label,"\\$40,000 To \\$44,999") ~ "$35,000 to $50,000",
      str_detect(label,"\\$45,000 To \\$49,999") ~ "$35,000 to $50,000",
      # $50k to $75k
      str_detect(label,"\\$50,000 To \\$59,999") ~ "$50,000 to $75,000",
      str_detect(label,"\\$60,000 To \\$74,999") ~ "$50,000 to $75,000",
      # $75k to $100k
      str_detect(label,"\\$75,000 To \\$99,999") ~ "$75,000 to $100,000",
      # $100k to $150k
      str_detect(label,"\\$100,000 To \\$124,999") ~ "$100,000 to $150,000",
      str_detect(label,"\\$125,000 To \\$149,999") ~ "$100,000 to $150,000",
      # $150k to $200k
      str_detect(label,"\\$150,000 To \\$199,999") ~ "$150,000 to $200,000",
      # $200k or more
      str_detect(label,"\\$200,000 Or More") ~ "$200,000 or more")) |>
    mutate(county = substring(geoid, 1, 5)) |>
    filter(county %in% c("53033", "53035", "53053", "53061")) |>
    group_by(geoid, grouping) |>
    summarise(estimate = sum(estimate)) |>
    as_tibble() |>
    mutate(concept = "Households by Income") |>
    mutate(geography_type = "Blockgroup") |>
    mutate(grouping = str_wrap(grouping, width=11)) |>
    mutate(grouping = factor(grouping, levels = income_order)) |>
    mutate(year = as.character(y)) |>
    arrange(geoid, grouping, year)
  
  if(is.null(bg_pre_2013)) {bg_pre_2013=working} else {bg_pre_2013=bind_rows(bg_pre_2013, working)}
  rm(working)
  
}

# Combine Blockgroup Data
blockgroups <- bind_rows(bg_pre_2013, bg_post_2013) |> arrange(geoid, grouping, year)
rm(bg_pre_2013, bg_post_2013)

# Regional Growth Centers
places <- NULL
for(place in c(rgc_names, "All RGCs")) {
  
  df <- geography_estimate_from_bg(geography_type = rgc_title, split_type = "percent_of_occupied_housing_units", geography_name=place)
  ifelse(is.null(places), places <- df, places <- bind_rows(places, df))
  rm(df)
  
}

# Planning Academy Schools
for(place in c(school_names, "All Schools")) {
  
  df <- geography_estimate_from_bg(geography_type = school_title, split_type = "percent_of_occupied_housing_units", geography_name=place)
  places <- bind_rows(places, df)
  rm(df)
  
}

households_by_income <- bind_rows(county, places)
ord <- unique(c(county_order, "All RGCs", rgc_names, "All Schools", school_names))
households_by_income <- households_by_income |> mutate(geography = factor(geography, levels = ord)) |> arrange(geography, grouping, year)
saveRDS(households_by_income, "data/households_by_income.rds")
rm(income_lookup, places, blockgroups, county)

# Housing Tenure ----------------------------------------------------------
tenure_lookup <- data.frame(variable = c("B25003_001",
                                         "B25003_002", 
                                         "B25003_003"),
                            grouping = c("Total",
                                         "Owner", 
                                         "Renter"))

county <- get_acs_recs(geography="county", table.names = tenure_table, years = c(pre_api_years, api_years), acs.type = 'acs5') 

county <- left_join(county, tenure_lookup, by=c("variable")) |>
  filter(!(is.na(grouping))) |>
  select(geography="name", "estimate", "year", "grouping") |>
  group_by(geography, year, grouping) |>
  summarise(estimate = sum(estimate)) |>
  as_tibble() |>
  mutate(concept = "Household Tenure")

totals <- county |> filter(grouping == "Total") |> select("geography", "year", "concept", total="estimate")

county <- left_join(county, totals, by=c("geography", "year", "concept")) |>
  mutate(share = estimate / total) |>
  select(-"total") |>
  mutate(geography_type = "County") |>
  mutate(grouping = str_wrap(grouping, width=11)) |>
  mutate(geography = factor(geography, levels = county_order)) |>
  mutate(grouping = factor(grouping, levels = tenure_order)) |>
  mutate(year = as.character(year)) |>
  arrange(geography, grouping, year)

rm(totals)

# Blockgroup 2013 onward
bg_post_2013 <- get_acs_recs(geography="block group", table.names = tenure_table, years = api_years, acs.type = 'acs5') 

bg_post_2013 <- left_join(bg_post_2013, tenure_lookup, by=c("variable")) |>
  filter(!(is.na(grouping))) |>
  select(geoid="GEOID", "estimate", "year", "grouping") |>
  group_by(geoid, year, grouping) |>
  summarise(estimate = sum(estimate)) |>
  as_tibble() |>
  mutate(concept = "Household Tenure") |>
  mutate(geography_type = "Blockgroup") |>
  mutate(grouping = str_wrap(grouping, width=11)) |>
  mutate(grouping = factor(grouping, levels = tenure_order)) |>
  mutate(year = as.character(year)) |>
  arrange(geoid, grouping, year)

# Blockgroup pre 2013
bg_pre_2013 <- NULL
for (y in pre_api_years) {
  tenure_bg <- file.path(acs_pre2013_bg_dir, paste0("acs_",y,"5_", tenure_table, "_150.xlsx"))
  working <- readxl::read_excel(tenure_bg, sheet=paste0("ACS_",y,"5_", tenure_table, "_150"), skip=7) |>
    mutate(geoid = as.character(str_remove_all(`Geographic Identifier`, "15000US"))) |>
    select(-"Geographic Identifier",-"Table ID", -"Title", -"Universe", -"Summary Level Type", -"Summary Level", -"Area Name", -"ACS Dataset") |>
    pivot_longer(!geoid, names_to = "label", values_to = "estimate") |>
    filter(!(str_detect(label, "Margin of Error"))) |>
    mutate(label = str_remove_all(label, "\\[Estimate\\] ")) |>
    mutate(label = str_remove_all(label, "\\...[0-9][0-9]")) |>
    mutate(grouping = case_when(
      # Total Population
      str_detect(label,"Total") ~ "Total",
      # Owner
      str_detect(label,"Owner") ~ "Owner",
      # Renter
      str_detect(label,"Renter") ~ "Renter")) |>
    mutate(county = substring(geoid, 1, 5)) |>
    filter(county %in% c("53033", "53035", "53053", "53061")) |>
    group_by(geoid, grouping) |>
    summarise(estimate = sum(estimate)) |>
    as_tibble() |>
    mutate(concept = "Household Tenure") |>
    mutate(geography_type = "Blockgroup") |>
    mutate(grouping = str_wrap(grouping, width=11)) |>
    mutate(grouping = factor(grouping, levels = tenure_order)) |>
    mutate(year = as.character(y)) |>
    arrange(geoid, grouping, year)
  
  if(is.null(bg_pre_2013)) {bg_pre_2013=working} else {bg_pre_2013=bind_rows(bg_pre_2013, working)}
  rm(working)
  
}

# Combine Blockgroup Data
blockgroups <- bind_rows(bg_pre_2013, bg_post_2013) |> arrange(geoid, grouping, year)
rm(bg_pre_2013, bg_post_2013)

# Regional Growth Centers
places <- NULL
for(place in c(rgc_names, "All RGCs")) {
  
  df <- geography_estimate_from_bg(geography_type = rgc_title, split_type = "percent_of_occupied_housing_units", geography_name=place)
  ifelse(is.null(places), places <- df, places <- bind_rows(places, df))
  rm(df)
  
}

# Planning Academy Schools
for(place in c(school_names, "All Schools")) {
  
  df <- geography_estimate_from_bg(geography_type = school_title, split_type = "percent_of_occupied_housing_units", geography_name=place)
  places <- bind_rows(places, df)
  rm(df)
  
}

households_by_tenure <- bind_rows(county, places)
ord <- unique(c(county_order, "All RGCs", rgc_names, "All Schools", school_names))
households_by_tenure <- households_by_tenure |> mutate(geography = factor(geography, levels = ord)) |> arrange(geography, grouping, year)
saveRDS(households_by_tenure, "data/households_by_tenure.rds")
rm(tenure_lookup, places, blockgroups, county)

# Structure Type ----------------------------------------------------------
structure_lookup <- data.frame(variable = c("B25024_001",
                                            "B25024_002",
                                            "B25024_003", "B25024_004", "B25024_005", "B25024_006",
                                            "B25024_007",
                                            "B25024_008", "B25024_009",
                                            "B25024_010", "B25024_011"),
                               grouping = c("Total",
                                            "SF detached",
                                            "Moderate-low density", "Moderate-low density", "Moderate-low density", "Moderate-low density",
                                            "Moderate-high density",
                                            "High density", "High density",
                                            "Other", "Other"))

county <- get_acs_recs(geography="county", table.names = structure_table, years = c(pre_api_years, api_years), acs.type = 'acs5') 

county <- left_join(county, structure_lookup, by=c("variable")) |>
  filter(!(is.na(grouping))) |>
  select(geography="name", "estimate", "year", "grouping") |>
  group_by(geography, year, grouping) |>
  summarise(estimate = sum(estimate)) |>
  as_tibble() |>
  mutate(concept = "Housing Unit Type")

totals <- county |> filter(grouping == "Total") |> select("geography", "year", "concept", total="estimate")

county <- left_join(county, totals, by=c("geography", "year", "concept")) |>
  mutate(share = estimate / total) |>
  select(-"total") |>
  mutate(geography_type = "County") |>
  mutate(grouping = str_wrap(grouping, width=15)) |>
  mutate(geography = factor(geography, levels = county_order)) |>
  mutate(grouping = factor(grouping, levels = structure_order)) |>
  mutate(year = as.character(year)) |>
  arrange(geography, grouping, year)

rm(totals)

# Blockgroup 2013 onward
bg_post_2013 <- get_acs_recs(geography="block group", table.names = structure_table, years = api_years, acs.type = 'acs5') 

bg_post_2013 <- left_join(bg_post_2013, structure_lookup, by=c("variable")) |>
  filter(!(is.na(grouping))) |>
  select(geoid="GEOID", "estimate", "year", "grouping") |>
  group_by(geoid, year, grouping) |>
  summarise(estimate = sum(estimate)) |>
  as_tibble() |>
  mutate(concept = "Housing Unit Type") |>
  mutate(geography_type = "Blockgroup") |>
  mutate(grouping = str_wrap(grouping, width=15)) |>
  mutate(grouping = factor(grouping, levels = structure_order)) |>
  mutate(year = as.character(year)) |>
  arrange(geoid, grouping, year)

# Blockgroup pre 2013
bg_pre_2013 <- NULL
for (y in pre_api_years) {
  structure_bg <- file.path(acs_pre2013_bg_dir, paste0("acs_",y,"5_", structure_table, "_150.xlsx"))
  working <- readxl::read_excel(structure_bg, sheet=paste0("ACS_",y,"5_", structure_table, "_150"), skip=7) |>
    mutate(geoid = as.character(str_remove_all(`Geographic Identifier`, "15000US"))) |>
    select(-"Geographic Identifier",-"Table ID", -"Title", -"Universe", -"Summary Level Type", -"Summary Level", -"Area Name", -"ACS Dataset") |>
    pivot_longer(!geoid, names_to = "label", values_to = "estimate") |>
    filter(!(str_detect(label, "Margin of Error"))) |>
    mutate(label = str_remove_all(label, "\\[Estimate\\] ")) |>
    mutate(label = str_remove_all(label, "\\...[0-9][0-9]")) |>
    mutate(grouping = case_when(
      # Total
      str_detect(label,"Total") ~ "Total",
      # High Density
      str_detect(label,"20 To 49") ~ "High density",
      str_detect(label,"50 Or More") ~ "High density",
      # Low Density
      str_detect(label,"1, Detached") ~ "SF detached",
      # Moderate Low
      str_detect(label,"1, Attached") ~ "Moderate-low density",
      str_detect(label,"2") ~ "Moderate-low density",
      str_detect(label,"3 Or 4") ~ "Moderate-low density",
      str_detect(label,"5 To 9") ~ "Moderate-low density",
      # Moderate High
      str_detect(label,"10 To 19") ~ "Moderate-high density",
      # Other
      str_detect(label,"Mobile Home") ~ "Other",
      str_detect(label,"Boat, Rv, Van, Etc.") ~ "Other")) |>
    mutate(county = substring(geoid, 1, 5)) |>
    filter(county %in% c("53033", "53035", "53053", "53061")) |>
    group_by(geoid, grouping) |>
    summarise(estimate = sum(estimate)) |>
    as_tibble() |>
    mutate(concept = "Housing Unit Type") |>
    mutate(geography_type = "Blockgroup") |>
    mutate(grouping = str_wrap(grouping, width=15)) |>
    mutate(grouping = factor(grouping, levels = structure_order)) |>
    mutate(year = as.character(y)) |>
    arrange(geoid, grouping, year)
  
  if(is.null(bg_pre_2013)) {bg_pre_2013=working} else {bg_pre_2013=bind_rows(bg_pre_2013, working)}
  rm(working)
  
}

# Combine Blockgroup Data
blockgroups <- bind_rows(bg_pre_2013, bg_post_2013) |> arrange(geoid, grouping, year)
rm(bg_pre_2013, bg_post_2013)

# Regional Growth Centers
places <- NULL
for(place in c(rgc_names, "All RGCs")) {
  
  df <- geography_estimate_from_bg(geography_type = rgc_title, split_type = "percent_of_housing_units", geography_name=place)
  ifelse(is.null(places), places <- df, places <- bind_rows(places, df))
  rm(df)
  
}

# Planning Academy Schools
for(place in c(school_names, "All Schools")) {
  
  df <- geography_estimate_from_bg(geography_type = school_title, split_type = "percent_of_housing_units", geography_name=place)
  places <- bind_rows(places, df)
  rm(df)
  
}

housing_units_by_type <- bind_rows(county, places)
ord <- unique(c(county_order, "All RGCs", rgc_names, "All Schools", school_names))
housing_units_by_type <- housing_units_by_type |> mutate(geography = factor(geography, levels = ord)) |> arrange(geography, grouping, year)
saveRDS(housing_units_by_type, "data/housing_units_by_type.rds")
rm(structure_lookup, places, blockgroups, county)

# Renter Cost Burden ----------------------------------------------------------
renter_burden_lookup <- data.frame(variable = c("B25070_001",
                                                "B25070_002", "B25070_003", "B25070_004", "B25070_005", "B25070_006",
                                                "B25070_007", "B25070_008", "B25070_009",
                                                "B25070_010", 
                                                "B25070_011"),
                                   grouping = c("Total",
                                                "Not cost burdened (<30%)", "Not cost burdened (<30%)", "Not cost burdened (<30%)", "Not cost burdened (<30%)", "Not cost burdened (<30%)",
                                                "Cost burdened (30-49.9%)", "Cost burdened (30-49.9%)", "Cost burdened (30-49.9%)",
                                                "Severely cost burdened (50+%)",
                                                "Not computed"))

county <- get_acs_recs(geography="county", table.names = renter_burden_table, years = c(pre_api_years, api_years), acs.type = 'acs5') 

county <- left_join(county, renter_burden_lookup, by=c("variable")) |>
  filter(!(is.na(grouping))) |>
  select(geography="name", "estimate", "year", "grouping") |>
  group_by(geography, year, grouping) |>
  summarise(estimate = sum(estimate)) |>
  as_tibble() |>
  mutate(concept = "Renter Cost Burden")

totals <- county |> filter(grouping == "Total") |> select("geography", "year", "concept", total="estimate")

county <- left_join(county, totals, by=c("geography", "year", "concept")) |>
  mutate(share = estimate / total) |>
  select(-"total") |>
  mutate(geography_type = "County") |>
  mutate(grouping = str_wrap(grouping, width=15)) |>
  mutate(geography = factor(geography, levels = county_order)) |>
  mutate(grouping = factor(grouping, levels = burden_order)) |>
  mutate(year = as.character(year)) |>
  arrange(geography, grouping, year)

rm(totals)

# Blockgroup 2013 onward
bg_post_2013 <- get_acs_recs(geography="block group", table.names = renter_burden_table, years = api_years, acs.type = 'acs5') 

bg_post_2013 <- left_join(bg_post_2013, renter_burden_lookup, by=c("variable")) |>
  filter(!(is.na(grouping))) |>
  select(geoid="GEOID", "estimate", "year", "grouping") |>
  group_by(geoid, year, grouping) |>
  summarise(estimate = sum(estimate)) |>
  as_tibble() |>
  mutate(concept = "Renter Cost Burden") |>
  mutate(geography_type = "Blockgroup") |>
  mutate(grouping = str_wrap(grouping, width=15)) |>
  mutate(grouping = factor(grouping, levels = burden_order)) |>
  mutate(year = as.character(year)) |>
  arrange(geoid, grouping, year)

# Blockgroup pre 2013
bg_pre_2013 <- NULL
for (y in pre_api_years) {
  renter_burden_bg <- file.path(acs_pre2013_bg_dir, paste0("acs_",y,"5_", renter_burden_table, "_150.xlsx"))
  working <- readxl::read_excel(renter_burden_bg, sheet=paste0("ACS_",y,"5_", renter_burden_table, "_150"), skip=7) |>
    mutate(geoid = as.character(str_remove_all(`Geographic Identifier`, "15000US"))) |>
    select(-"Geographic Identifier",-"Table ID", -"Title", -"Universe", -"Summary Level Type", -"Summary Level", -"Area Name", -"ACS Dataset") |>
    pivot_longer(!geoid, names_to = "label", values_to = "estimate") |>
    filter(!(str_detect(label, "Margin of Error"))) |>
    mutate(label = str_remove_all(label, "\\[Estimate\\] ")) |>
    mutate(label = str_remove_all(label, "\\...[0-9][0-9]")) |>
    mutate(grouping = case_when(
      # Total
      str_detect(label,"Total") ~ "Total",
      # Not Burdened
      str_detect(label,"Less Than 10.0 Percent") ~ "Not cost burdened (<30%)",
      str_detect(label,"10.0 To 14.9 Percent") ~ "Not cost burdened (<30%)",
      str_detect(label,"15.0 To 19.9 Percent") ~ "Not cost burdened (<30%)",
      str_detect(label,"20.0 To 24.9 Percent") ~ "Not cost burdened (<30%)",
      str_detect(label,"25.0 To 29.9 Percent") ~ "Not cost burdened (<30%)",
      # Cost Burdened
      str_detect(label,"30.0 To 34.9 Percent") ~ "Cost burdened (30-49.9%)",
      str_detect(label,"35.0 To 39.9 Percent") ~ "Cost burdened (30-49.9%)",
      str_detect(label,"40.0 To 49.9 Percent") ~ "Cost burdened (30-49.9%)",
      # Severely cost burdened
      str_detect(label,"50.0 Percent Or More") ~ "Severely cost burdened (50+%)",
      # Not computed
      str_detect(label,"Not Computed") ~ "Not computed")) |>
    mutate(county = substring(geoid, 1, 5)) |>
    filter(county %in% c("53033", "53035", "53053", "53061")) |>
    group_by(geoid, grouping) |>
    summarise(estimate = sum(estimate)) |>
    as_tibble() |>
    mutate(concept = "Renter Cost Burden") |>
    mutate(geography_type = "Blockgroup") |>
    mutate(grouping = str_wrap(grouping, width=15)) |>
    mutate(grouping = factor(grouping, levels = burden_order)) |>
    mutate(year = as.character(y)) |>
    arrange(geoid, grouping, year)
  
  if(is.null(bg_pre_2013)) {bg_pre_2013=working} else {bg_pre_2013=bind_rows(bg_pre_2013, working)}
  rm(working)
  
}

# Combine Blockgroup Data
blockgroups <- bind_rows(bg_pre_2013, bg_post_2013) |> arrange(geoid, grouping, year)
rm(bg_pre_2013, bg_post_2013)

# Regional Growth Centers
places <- NULL
for(place in c(rgc_names, "All RGCs")) {
  
  df <- geography_estimate_from_bg(geography_type = rgc_title, split_type = "percent_of_occupied_housing_units", geography_name=place)
  ifelse(is.null(places), places <- df, places <- bind_rows(places, df))
  rm(df)
  
}

# Planning Academy Schools
for(place in c(school_names, "All Schools")) {
  
  df <- geography_estimate_from_bg(geography_type = school_title, split_type = "percent_of_occupied_housing_units", geography_name=place)
  places <- bind_rows(places, df)
  rm(df)
  
}

renter_cost_burden <- bind_rows(county, places) |> mutate(share = replace_na(share, 0))
rm(renter_burden_lookup, places, blockgroups, county)

# Owner Cost Burden ----------------------------------------------------------
owner_burden_lookup <- data.frame(variable = c("B25091_002",
                                               "B25091_003", "B25091_004", "B25091_005", "B25091_006", "B25091_007",
                                               "B25091_008", "B25091_009", "B25091_010",
                                               "B25091_011",
                                               "B25091_012"),
                                  grouping = c("Total",
                                               "Not cost burdened (<30%)", "Not cost burdened (<30%)", "Not cost burdened (<30%)", "Not cost burdened (<30%)", "Not cost burdened (<30%)",
                                               "Cost burdened (30-49.9%)", "Cost burdened (30-49.9%)", "Cost burdened (30-49.9%)",
                                               "Severely cost burdened (50+%)",
                                               "Not computed"))

county <- get_acs_recs(geography="county", table.names = owner_burden_table, years = c(pre_api_years, api_years), acs.type = 'acs5') 

county <- left_join(county, owner_burden_lookup, by=c("variable")) |>
  filter(!(is.na(grouping))) |>
  select(geography="name", "estimate", "year", "grouping") |>
  group_by(geography, year, grouping) |>
  summarise(estimate = sum(estimate)) |>
  as_tibble() |>
  mutate(concept = "Owner Cost Burden")

totals <- county |> filter(grouping == "Total") |> select("geography", "year", "concept", total="estimate")

county <- left_join(county, totals, by=c("geography", "year", "concept")) |>
  mutate(share = estimate / total) |>
  select(-"total") |>
  mutate(geography_type = "County") |>
  mutate(grouping = str_wrap(grouping, width=15)) |>
  mutate(geography = factor(geography, levels = county_order)) |>
  mutate(grouping = factor(grouping, levels = burden_order)) |>
  mutate(year = as.character(year)) |>
  arrange(geography, grouping, year)

rm(totals)

# Blockgroup 2013 onward
bg_post_2013 <- get_acs_recs(geography="block group", table.names = owner_burden_table, years = api_years, acs.type = 'acs5') 

bg_post_2013 <- left_join(bg_post_2013, owner_burden_lookup, by=c("variable")) |>
  filter(!(is.na(grouping))) |>
  select(geoid="GEOID", "estimate", "year", "grouping") |>
  group_by(geoid, year, grouping) |>
  summarise(estimate = sum(estimate)) |>
  as_tibble() |>
  mutate(concept = "Owner Cost Burden") |>
  mutate(geography_type = "Blockgroup") |>
  mutate(grouping = str_wrap(grouping, width=15)) |>
  mutate(grouping = factor(grouping, levels = burden_order)) |>
  mutate(year = as.character(year)) |>
  arrange(geoid, grouping, year)

# Blockgroup pre 2013
bg_pre_2013 <- NULL
for (y in pre_api_years) {
  owner_burden_bg <- file.path(acs_pre2013_bg_dir, paste0("acs_",y,"5_", owner_burden_table, "_150_v2.xlsx"))
  working <- readxl::read_excel(owner_burden_bg, sheet=paste0("ACS_",y,"5_", owner_burden_table, "_150"), skip=7) |>
    mutate(geoid = as.character(str_remove_all(`Geographic Identifier`, "15000US"))) |>
    select(-"Geographic Identifier",-"Table ID", -"Title", -"Universe", -"Summary Level Type", -"Summary Level", -"Area Name", -"ACS Dataset") |>
    pivot_longer(!geoid, names_to = "label", values_to = "estimate") |>
    filter(!(str_detect(label, "Margin of Error"))) |>
    mutate(label = str_remove_all(label, "\\[Estimate\\] ")) |>
    mutate(label = str_remove_all(label, "\\...[0-9][0-9]")) |>
    mutate(grouping = case_when(
      # Total
      str_detect(label,"Housing Units With A Mortgage") ~ "Total",
      # Not Burdened
      str_detect(label,"Less Than 10.0 Percent") ~ "Not cost burdened (<30%)",
      str_detect(label,"10.0 To 14.9 Percent") ~ "Not cost burdened (<30%)",
      str_detect(label,"15.0 To 19.9 Percent") ~ "Not cost burdened (<30%)",
      str_detect(label,"20.0 To 24.9 Percent") ~ "Not cost burdened (<30%)",
      str_detect(label,"25.0 To 29.9 Percent") ~ "Not cost burdened (<30%)",
      # Cost Burdened
      str_detect(label,"30.0 To 34.9 Percent") ~ "Cost burdened (30-49.9%)",
      str_detect(label,"35.0 To 39.9 Percent") ~ "Cost burdened (30-49.9%)",
      str_detect(label,"40.0 To 49.9 Percent") ~ "Cost burdened (30-49.9%)",
      # Severely cost burdened
      str_detect(label,"50.0 Percent Or More") ~ "Severely cost burdened (50+%)",
      # Not computed
      str_detect(label,"Not Computed") ~ "Not computed")) |>
    drop_na() |> 
    mutate(county = substring(geoid, 1, 5)) |>
    filter(county %in% c("53033", "53035", "53053", "53061")) |>
    group_by(geoid, grouping) |>
    summarise(estimate = sum(estimate)) |>
    as_tibble() |>
    mutate(concept = "Owner Cost Burden") |>
    mutate(geography_type = "Blockgroup") |>
    mutate(grouping = str_wrap(grouping, width=15)) |>
    mutate(grouping = factor(grouping, levels = burden_order)) |>
    mutate(year = as.character(y)) |>
    arrange(geoid, grouping, year)
  
  if(is.null(bg_pre_2013)) {bg_pre_2013=working} else {bg_pre_2013=bind_rows(bg_pre_2013, working)}
  rm(working)
  
}

# Combine Blockgroup Data
blockgroups <- bind_rows(bg_pre_2013, bg_post_2013) |> arrange(geoid, grouping, year)
rm(bg_pre_2013, bg_post_2013)

# Regional Growth Centers
places <- NULL
for(place in c(rgc_names, "All RGCs")) {
  
  df <- geography_estimate_from_bg(geography_type = rgc_title, split_type = "percent_of_occupied_housing_units", geography_name=place)
  ifelse(is.null(places), places <- df, places <- bind_rows(places, df))
  rm(df)
  
}

# Planning Academy Schools
for(place in c(school_names, "All Schools")) {
  
  df <- geography_estimate_from_bg(geography_type = school_title, split_type = "percent_of_occupied_housing_units", geography_name=place)
  places <- bind_rows(places, df)
  rm(df)
  
}

owner_cost_burden <- bind_rows(county, places) |> mutate(share = replace_na(share, 0))
cost_burden <- bind_rows(owner_cost_burden, renter_cost_burden)
ord <- unique(c(county_order, "All RGCs", rgc_names, "All Schools", school_names))
cost_burden <- cost_burden |> mutate(geography = factor(geography, levels = ord)) |> arrange(geography, grouping, year)
saveRDS(cost_burden, "data/cost_burden.rds")
rm(owner_burden_lookup, places, blockgroups, county, owner_cost_burden, renter_cost_burden)

# Educational Attainment --------------------------------------------------
education_lookup <- data.frame(variable = c("B15002_001",
                                            "B15002_003", "B15002_004","B15002_005","B15002_006", "B15002_007", "B15002_008", "B15002_009", "B15002_010",
                                            "B15002_011",
                                            "B15002_012", "B15002_013", "B15002_014",
                                            "B15002_015",
                                            "B15002_016", "B15002_017", "B15002_018",
                                            "B15002_020", "B15002_021","B15002_022","B15002_023", "B15002_024", "B15002_025", "B15002_026", "B15002_027",
                                            "B15002_028",
                                            "B15002_029", "B15002_030", "B15002_031",
                                            "B15002_032",
                                            "B15002_033", "B15002_034", "B15002_035"),
                               grouping = c("Total",
                                            "No high school diploma", "No high school diploma", "No high school diploma", "No high school diploma", "No high school diploma", "No high school diploma", "No high school diploma", "No high school diploma",
                                            "High school",
                                            "Some college", "Some college", "Some college",
                                            "Bachelor’s degree",
                                            "Graduate degree", "Graduate degree", "Graduate degree",
                                            "No high school diploma", "No high school diploma", "No high school diploma", "No high school diploma", "No high school diploma", "No high school diploma", "No high school diploma", "No high school diploma",
                                            "High school",
                                            "Some college", "Some college", "Some college",
                                            "Bachelor’s degree",
                                            "Graduate degree", "Graduate degree", "Graduate degree"))

county <- get_acs_recs(geography="county", table.names = education_table, years = c(pre_api_years, api_years), acs.type = 'acs5') 

county <- left_join(county, education_lookup, by=c("variable")) |>
  filter(!(is.na(grouping))) |>
  select(geography="name", "estimate", "year", "grouping") |>
  group_by(geography, year, grouping) |>
  summarise(estimate = sum(estimate)) |>
  as_tibble() |>
  mutate(concept = "Educational Attainment")

totals <- county |> filter(grouping == "Total") |> select("geography", "year", "concept", total="estimate")

county <- left_join(county, totals, by=c("geography", "year", "concept")) |>
  mutate(share = estimate / total) |>
  select(-"total") |>
  mutate(geography_type = "County") |>
  mutate(grouping = str_wrap(grouping, width=15)) |>
  mutate(geography = factor(geography, levels = county_order)) |>
  mutate(grouping = factor(grouping, levels = education_order)) |>
  mutate(year = as.character(year)) |>
  arrange(geography, grouping, year)

rm(totals)

# Blockgroup 2013 onward
bg_post_2013 <- get_acs_recs(geography="block group", table.names = education_table, years = api_years, acs.type = 'acs5') 

bg_post_2013 <- left_join(bg_post_2013, education_lookup, by=c("variable")) |>
  filter(!(is.na(grouping))) |>
  select(geoid="GEOID", "estimate", "year", "grouping") |>
  group_by(geoid, year, grouping) |>
  summarise(estimate = sum(estimate)) |>
  as_tibble() |>
  mutate(concept = "Educational Attainment") |>
  mutate(geography_type = "Blockgroup") |>
  mutate(grouping = str_wrap(grouping, width=15)) |>
  mutate(grouping = factor(grouping, levels = education_order)) |>
  mutate(year = as.character(year)) |>
  arrange(geoid, grouping, year)

# Blockgroup pre 2013
bg_pre_2013 <- NULL
for (y in pre_api_years) {
  education_bg <- file.path(acs_pre2013_bg_dir, paste0("acs_",y,"5_", education_table, "_150.xlsx"))
  working <- readxl::read_excel(education_bg, sheet=paste0("ACS_",y,"5_", education_table, "_150"), skip=7) |>
    mutate(geoid = as.character(str_remove_all(`Geographic Identifier`, "15000US"))) |>
    select(-"Geographic Identifier",-"Table ID", -"Title", -"Universe", -"Summary Level Type", -"Summary Level", -"Area Name", -"ACS Dataset") |>
    pivot_longer(!geoid, names_to = "label", values_to = "estimate") |>
    filter(!(str_detect(label, "Margin of Error"))) |>
    mutate(label = str_remove_all(label, "\\[Estimate\\] ")) |>
    mutate(label = str_remove_all(label, "\\...[0-9][0-9]")) |>
    mutate(grouping = case_when(
      # Total Population
      str_detect(label,"Total") ~ "Total",
      # No High School
      str_detect(label,"No Schooling Completed") ~ "No high school diploma",
      str_detect(label,"Nursery To 4th Grade") ~ "No high school diploma",
      str_detect(label,"5th And 6th Grade") ~ "No high school diploma",
      str_detect(label,"7th And 8th Grade") ~ "No high school diploma",
      str_detect(label,"9th Grade") ~ "No high school diploma",
      str_detect(label,"10th Grade") ~ "No high school diploma",
      str_detect(label,"11th Grade") ~ "No high school diploma",
      str_detect(label,"12th Grade, No Diploma") ~ "No high school diploma",
      # High School
      str_detect(label,"High School Graduate, Ged, Or Alternative") ~ "High school",
      # Some college
      str_detect(label,"Some College, Less Than 1 Year") ~ "Some college",
      str_detect(label,"Some College, 1 Or More Years, No Degree") ~ "Some college",
      str_detect(label,"Associate's Degree") ~ "Some college",
      # Bachelors Degree
      str_detect(label,"Bachelor's Degree") ~ "Bachelor’s degree",
      # Graduate degree
      str_detect(label,"Master's Degree") ~ "Graduate degree",
      str_detect(label,"Professional School Degree") ~ "Graduate degree",
      str_detect(label,"Doctorate Degree") ~ "Graduate degree")) |>
    drop_na() |> 
    mutate(county = substring(geoid, 1, 5)) |>
    filter(county %in% c("53033", "53035", "53053", "53061")) |>
    group_by(geoid, grouping) |>
    summarise(estimate = sum(estimate)) |>
    as_tibble() |>
    mutate(concept = "Educational Attainment") |>
    mutate(geography_type = "Blockgroup") |>
    mutate(grouping = str_wrap(grouping, width=15)) |>
    mutate(grouping = factor(grouping, levels = education_order)) |>
    mutate(year = as.character(y)) |>
    arrange(geoid, grouping, year)
  
  if(is.null(bg_pre_2013)) {bg_pre_2013=working} else {bg_pre_2013=bind_rows(bg_pre_2013, working)}
  rm(working)
  
}

# Combine Blockgroup Data
blockgroups <- bind_rows(bg_pre_2013, bg_post_2013) |> arrange(geoid, grouping, year)
rm(bg_pre_2013, bg_post_2013)

# Regional Growth Centers
places <- NULL
for(place in c(rgc_names, "All RGCs")) {
  
  df <- geography_estimate_from_bg(geography_type = rgc_title, split_type = "percent_of_occupied_housing_units", geography_name=place)
  ifelse(is.null(places), places <- df, places <- bind_rows(places, df))
  rm(df)
  
}

# Planning Academy Schools
for(place in c(school_names, "All Schools")) {
  
  df <- geography_estimate_from_bg(geography_type = school_title, split_type = "percent_of_occupied_housing_units", geography_name=place)
  places <- bind_rows(places, df)
  rm(df)
  
}

educational_attainment <- bind_rows(county, places) |> mutate(share = replace_na(share, 0))
ord <- unique(c(county_order, "All RGCs", rgc_names, "All Schools", school_names))
educational_attainment <- educational_attainment |> mutate(geography = factor(geography, levels = ord)) |> arrange(geography, grouping, year)
saveRDS(educational_attainment, "data/educational_attainment.rds")
rm(education_lookup, places, blockgroups, county)

# Mode Share to Work ------------------------------------------------------
mode_lookup <- data.frame(variable = c("B08301_001",
                                       "B08301_003", 
                                       "B08301_004",
                                       "B08301_010",
                                       "B08301_016", "B08301_017", "B08301_020",
                                       "B08301_018", 
                                       "B08301_019",
                                       "B08301_021"),
                          grouping = c("Total",
                                       "Drove Alone",
                                       "Carpooled",
                                       "Transit",
                                       "Other", "Other", "Other",
                                       "Bike",
                                       "Walk",
                                       "Work from Home"))

county <- get_acs_recs(geography="county", table.names = mode_table, years = c(pre_api_years, api_years), acs.type = 'acs5') 

county <- left_join(county, mode_lookup, by=c("variable")) |>
  filter(!(is.na(grouping))) |>
  select(geography="name", "estimate", "year", "grouping") |>
  group_by(geography, year, grouping) |>
  summarise(estimate = sum(estimate)) |>
  as_tibble() |>
  mutate(concept = "Mode to Work")

totals <- county |> filter(grouping == "Total") |> select("geography", "year", "concept", total="estimate")

county <- left_join(county, totals, by=c("geography", "year", "concept")) |>
  mutate(share = estimate / total) |>
  select(-"total") |>
  mutate(geography_type = "County") |>
  mutate(grouping = str_wrap(grouping, width=10)) |>
  mutate(geography = factor(geography, levels = county_order)) |>
  mutate(grouping = factor(grouping, levels = mode_order)) |>
  mutate(year = as.character(year)) |>
  arrange(geography, grouping, year)

rm(totals)

# Blockgroup 2013 onward
bg_post_2013 <- get_acs_recs(geography="block group", table.names = mode_table, years = api_years, acs.type = 'acs5') 

bg_post_2013 <- left_join(bg_post_2013, mode_lookup, by=c("variable")) |>
  filter(!(is.na(grouping))) |>
  select(geoid="GEOID", "estimate", "year", "grouping") |>
  group_by(geoid, year, grouping) |>
  summarise(estimate = sum(estimate)) |>
  as_tibble() |>
  mutate(concept = "Mode to Work") |>
  mutate(geography_type = "Blockgroup") |>
  mutate(grouping = str_wrap(grouping, width=10)) |>
  mutate(grouping = factor(grouping, levels = mode_order)) |>
  mutate(year = as.character(year)) |>
  arrange(geoid, grouping, year)

# Blockgroup pre 2013
bg_pre_2013 <- NULL
for (y in pre_api_years) {
  mode_bg <- file.path(acs_pre2013_bg_dir, paste0("acs_",y,"5_", mode_table, "_150.xlsx"))
  working <- readxl::read_excel(mode_bg, sheet=paste0("ACS_",y,"5_", mode_table, "_150"), skip=7) |>
    mutate(geoid = as.character(str_remove_all(`Geographic Identifier`, "15000US"))) |>
    select(-"Geographic Identifier",-"Table ID", -"Title", -"Universe", -"Summary Level Type", -"Summary Level", -"Area Name", -"ACS Dataset") |>
    pivot_longer(!geoid, names_to = "label", values_to = "estimate") |>
    filter(!(str_detect(label, "Margin of Error"))) |>
    mutate(label = str_remove_all(label, "\\[Estimate\\] ")) |>
    mutate(label = str_remove_all(label, "\\...[0-9][0-9]")) |>
    mutate(grouping = case_when(
      # Total Population
      str_detect(label,"Total") ~ "Total",
      # Drove Alone
      str_detect(label,"Drove Alone") ~ "Drove Alone",
      # Carpool
      str_detect(label,"Carpooled") ~ "Carpooled",
      # Transit
      str_detect(label,"Public Transportation \\(Excluding Taxicab\\)") ~ "Transit",
      # Bike
      str_detect(label,"Bicycle") ~ "Bike",
      # Walk
      str_detect(label,"Walked") ~ "Walk",
      # Work from Home
      str_detect(label,"Worked At Home") ~ "Work from Home",
      # Other
      str_detect(label,"Taxicab") ~ "Other",
      str_detect(label,"Motorcycle") ~ "Other",
      str_detect(label,"Other Means") ~ "Other")) |>
    drop_na() |> 
    mutate(county = substring(geoid, 1, 5)) |>
    filter(county %in% c("53033", "53035", "53053", "53061")) |>
    group_by(geoid, grouping) |>
    summarise(estimate = sum(estimate)) |>
    as_tibble() |>
    mutate(concept = "Mode to Work") |>
    mutate(geography_type = "Blockgroup") |>
    mutate(grouping = str_wrap(grouping, width=10)) |>
    mutate(grouping = factor(grouping, levels = mode_order)) |>
    mutate(year = as.character(y)) |>
    arrange(geoid, grouping, year)
  
  if(is.null(bg_pre_2013)) {bg_pre_2013=working} else {bg_pre_2013=bind_rows(bg_pre_2013, working)}
  rm(working)
  
}

# Combine Blockgroup Data
blockgroups <- bind_rows(bg_pre_2013, bg_post_2013) |> arrange(geoid, grouping, year)
rm(bg_pre_2013, bg_post_2013)

# Regional Growth Centers
places <- NULL
for(place in c(rgc_names, "All RGCs")) {
  
  df <- geography_estimate_from_bg(geography_type = rgc_title, split_type = "percent_of_occupied_housing_units", geography_name=place)
  ifelse(is.null(places), places <- df, places <- bind_rows(places, df))
  rm(df)
  
}

# Planning Academy Schools
for(place in c(school_names, "All Schools")) {
  
  df <- geography_estimate_from_bg(geography_type = school_title, split_type = "percent_of_occupied_housing_units", geography_name=place)
  places <- bind_rows(places, df)
  rm(df)
  
}

mode_to_work <- bind_rows(county, places) |> mutate(share = replace_na(share, 0))
ord <- unique(c(county_order, "All RGCs", rgc_names, "All Schools", school_names))
mode_to_work <- mode_to_work |> mutate(geography = factor(geography, levels = ord)) |> arrange(geography, grouping, year)
saveRDS(mode_to_work, "data/mode_to_work.rds")
rm(mode_lookup, places, blockgroups, county)

# Vehicles Available --------------------------------------------------------
vehicle_lookup <- data.frame(variable = c("B08201_001",
                                          "B08201_002",
                                          "B08201_003",
                                          "B08201_004",
                                          "B08201_005",
                                          "B08201_006"),
                             grouping = c("Total",
                                          "No vehicles",
                                          "1 vehicle", 
                                          "2 vehicles", 
                                          "3 vehicles", 
                                          "4 or more vehicles"))

# County
county <- get_acs_recs(geography="county", table.names = vehicle_table, years = analysis_years, acs.type = 'acs5') 

county <- left_join(county, vehicle_lookup, by=c("variable")) |>
  filter(!(is.na(grouping))) |>
  select(geography="name", "estimate", "moe", "year", "grouping") |>
  group_by(geography, year, grouping) |>
  summarise(estimate = sum(estimate)) |>
  as_tibble() |>
  mutate(concept = "Households by Vehicle Availability")

totals <- county |> filter(grouping == "Total") |> select("geography", "year", "concept", total="estimate")

county <- left_join(county, totals, by=c("geography", "year", "concept")) |>
  mutate(share = estimate / total) |>
  select(-"total") |>
  mutate(geography_type = "County") |>
  mutate(geography = factor(geography, levels = county_order)) |>
  mutate(grouping = factor(grouping, levels = vehicle_order)) |>
  mutate(year = as.character(year)) |>
  arrange(geography, grouping, year)

rm(totals)

# Census Tracts
tracts <- get_acs_recs(geography="tract", table.names = vehicle_table, years = analysis_years, acs.type = 'acs5') 

tracts <- left_join(tracts, vehicle_lookup, by=c("variable")) |>
  filter(!(is.na(grouping))) |>
  select(geoid="GEOID", "estimate", "moe", "year", "grouping") |>
  group_by(geoid, year, grouping) |>
  summarise(estimate = sum(estimate)) |>
  as_tibble() |>
  mutate(concept = "Households by Vehicle Availability") |>
  mutate(geography_type = "Tract") |>
  mutate(grouping = factor(grouping, levels = vehicle_order)) |>
  mutate(year = as.character(year)) |>
  arrange(geoid, grouping, year)

# Regional Growth Centers
places <- NULL
for(place in c(rgc_names, "All RGCs")) {
  
  df <- geography_estimate_from_bg(split_df=tract_splits, estimate_df=tracts, geography_type = rgc_title, split_type = "percent_of_occupied_housing_units", geography_name=place)
  ifelse(is.null(places), places <- df, places <- bind_rows(places, df))
  rm(df)
  
}

# Planning Academy Schools
for(place in c(school_names, "All Schools")) {
  
  df <- geography_estimate_from_bg(split_df=tract_splits, estimate_df=tracts, geography_type = school_title, split_type = "percent_of_occupied_housing_units", geography_name=place)
  places <- bind_rows(places, df)
  rm(df)
  
}

households_by_vehicles <- bind_rows(county, places)
ord <- unique(c(county_order, "All RGCs", rgc_names, "All Schools", school_names))
households_by_vehicles <- households_by_vehicles |> mutate(geography = factor(geography, levels = ord)) |> arrange(geography, grouping, year)
saveRDS(households_by_vehicles, "data/households_by_vehicles.rds")
rm(vehicle_lookup, places, tracts, county)
