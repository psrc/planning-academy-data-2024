library(tidyverse)
library(psrcelmer)
library(sf)

source("functions.R")

# Inputs ------------------------------------------

# Data Years
pre_api_years <- c(2012)
api_years <- c(2017, 2022)
analysis_years <- c(pre_api_years, api_years)

# Make sure these match the boundary definition you want the data to be based on
rgc_title <- "Regional Growth Center (4/23/2024)"

# Coordinate Reference Systems
wgs84 <- 4326
spn <- 2285

# Silence the dplyr summarize message
options(dplyr.summarise.inform = FALSE)

# Blockgroup Splits from Elmer for Centers --------------------------------------------
blockgroup_splits <- NULL
for (yr in analysis_years) {
  splits <- generate_blockgroup_splits(y = yr)
  if (is.null(blockgroup_splits)) {blockgroup_splits <- splits} else {blockgroup_splits <- bind_rows(blockgroup_splits, splits)}
  rm(splits)
}

blockgroup_splits <- blockgroup_splits |>
  select(year = "ofm_estimate_year", geoid = "data_geog", "planning_geog_type", "planning_geog", "percent_of_total_pop", "percent_of_household_pop", "percent_of_housing_units", "percent_of_occupied_housing_units")

# Blockgroup Splits for Planning Academy Schools --------------------------

# Step #1: Read in School layer
print(str_glue("Reading in layer of Summer Planning Acadmey School Boundaries"))
schools <- st_read("X:/DSA/shiny-uploads/planning-academy/summer_planning_academy_schools.shp") |> st_transform(spn)

# Step #2: Read in 2010 Blockgroups Layer
print(str_glue("Reading in 2010 Blockgroups"))
blockgrp10 <- st_read_elmergeo("blockgrp2010") |> st_transform(spn) |> select(geoid = "geoid10") |> mutate(census_year = 2010)

# Step #3: Read in 2020 Blockgroups Layer
print(str_glue("Reading in 2020 Blockgroups"))
blockgrp20 <- st_read_elmergeo("blockgrp2020") |> st_transform(spn) |> select(geoid = "geoid20") |> mutate(census_year = 2020)

# Step #4: Intersect Blockgroups with schools to get list of blockgroups that intersect each school boundary
print(str_glue("Intersection schools with 2010 Block groups"))
t10 <- st_intersection(blockgrp10, schools) |> st_drop_geometry() |> select("census_year", "geoid", "district", "school") |> distinct()

print(str_glue("Intersection schools with 2020 Block groups"))
t20 <- st_intersection(blockgrp20, schools) |> st_drop_geometry() |> select("census_year", "geoid", "district", "school") |> distinct()

print(str_glue("Combining blockgroups with schools for Census 2010 and Census 2020 boundaries"))
blockgrp_schools <- bind_rows(t10, t20) |> as_tibble()
rm(t10, t20, blockgrp10, blockgrp20)

# Step #5: Pull parcel data with blockgroup id and population metrics from Elmer
parcel_data <- NULL
for (yr in analysis_years) {
  t <- process_ofm_bg_data_for_parcels(yrs = yr)
  if (is.null(parcel_data)) {parcel_data <- t} else {parcel_data <- bind_rows(parcel_data, t)}
  rm(t)
}

# Step #6: Get Population Data by Blockgroup for each school for each year
school_bg_splits <- NULL
school_nms <- unique(schools$school)
for (yr in analysis_years) {
  
  if (yr < 2020) {census_yr <- 2010} else {census_yr <- 2020}
  if (yr < 2020) {bg_yr <- "block_group_geoid10"} else {bg_yr <- "block_group_geoid20"}
  
  for (schl in school_nms) {
    print(str_glue("Calculating blockgroup shares for {schl} for the year {yr}"))
    # Blockgroups for school and census year of analysis year
    bg <- blockgrp_schools |> filter(school == schl & census_year == census_yr) |> distinct() |> select("geoid") |> pull()
    
    # Parcels for school and analysis year 
    p <- parcel_data |> 
      filter(estimate_year == yr & .data[[bg_yr]] %in% bg) |>
      select(year = "estimate_year", "parcel_id", x = "x_coord_state_plane", y = "y_coord_state_plane", geoid = all_of(bg_yr), pop = "total_pop", hh_pop = "household_pop", hu = "housing_units", hh = "occupied_housing_units") |>
      mutate(geoid = as.character(geoid))
    
    # Blockgroup Totals for Blockgroups that intersect Schools
    bg_total <- p |>
      group_by(year, geoid) |>
      summarise(tot_pop = round(sum(pop),0), tot_hh_pop = round(sum(hh_pop),0), tot_hu = round(sum(hu),0), tot_hh = round(sum(hh),0)) |>
      as_tibble()
    
    # Blockgroup Totals for Parcels that intersect Schools
    p_lyr <- st_as_sf(p, coords = c("x", "y"), crs = spn)
    s_lyr <- schools |> filter(school == schl)
    
    bg_parcels <- st_intersection(p_lyr, s_lyr) |> 
      st_drop_geometry() |>
      group_by(year, school, geoid) |>
      summarise(pop = round(sum(pop),0), hh_pop = round(sum(hh_pop),0), hu = round(sum(hu),0), hh = round(sum(hh),0)) |>
      as_tibble() |>
      rename(planning_geog = "school")
    
    # Blockgroup shares for school
    tbl <- left_join(bg_parcels, bg_total, by=c("year", "geoid")) |>
      mutate(percent_of_total_pop = pop/tot_pop) |>
      mutate(percent_of_household_pop = hh_pop/tot_hh_pop) |>
      mutate(percent_of_housing_units = hu/tot_hu) |>
      mutate(percent_of_occupied_housing_units = hh/tot_hh) |>
      mutate(percent_of_total_pop = replace_na(percent_of_total_pop, 0)) |>
      mutate(percent_of_household_pop = replace_na(percent_of_household_pop, 0)) |>
      mutate(percent_of_housing_units = replace_na(percent_of_housing_units, 0)) |>
      mutate(percent_of_occupied_housing_units = replace_na(percent_of_occupied_housing_units,0)) |>
      mutate(planning_geog_type = "Planning Academy School 2024") |>
      select("year", "geoid", "planning_geog_type", "planning_geog", "percent_of_total_pop", "percent_of_household_pop", "percent_of_housing_units", "percent_of_occupied_housing_units")
    
    if (is.null(school_bg_splits)) {school_bg_splits <- tbl} else {school_bg_splits <- bind_rows(school_bg_splits, tbl)}
    rm(bg, p, bg_total, p_lyr, s_lyr, bg_parcels, tbl)
  }
}

# Final Blockgroup Cleanup -----------------------------------------------------------
bg_splits <- bind_rows(school_bg_splits, blockgroup_splits)
saveRDS(bg_splits, "data/blockgroup_splits.rds")
rm(school_bg_splits, blockgroup_splits)

# Tract Splits from Elmer for Centers --------------------------------------------
tract_splits <- NULL
for (yr in analysis_years) {
  splits <- generate_tract_splits(y = yr)
  if (is.null(tract_splits)) {tract_splits <- splits} else {tract_splits <- bind_rows(tract_splits, splits)}
  rm(splits)
}

tract_splits <- tract_splits |>
  select(year = "ofm_estimate_year", geoid = "data_geog", "planning_geog_type", "planning_geog", "percent_of_total_pop", "percent_of_household_pop", "percent_of_housing_units", "percent_of_occupied_housing_units")

# Tract Splits for Planning Academy Schools --------------------------

# Step #1: Read in School layer
print(str_glue("Reading in layer of Summer Planning Acadmey School Boundaries"))
schools <- st_read("X:/DSA/shiny-uploads/planning-academy/summer_planning_academy_schools.shp") |> st_transform(spn)

# Step #2: Read in 2010 Tracts Layer
print(str_glue("Reading in 2010 Tracts"))
tract10 <- st_read_elmergeo("tract2010") |> st_transform(spn) |> select(geoid = "geoid10") |> mutate(census_year = 2010)

# Step #3: Read in 2020 Tracts Layer
print(str_glue("Reading in 2020 Tracts"))
tract20 <- st_read_elmergeo("tract2020") |> st_transform(spn) |> select(geoid = "geoid20") |> mutate(census_year = 2020)

# Step #4: Intersect Tracts with schools to get list of tracts that intersect each school boundary
print(str_glue("Intersection schools with 2010 Tracts"))
t10 <- st_intersection(tract10, schools) |> st_drop_geometry() |> select("census_year", "geoid", "district", "school") |> distinct()

print(str_glue("Intersection schools with 2020 Tracts"))
t20 <- st_intersection(tract20, schools) |> st_drop_geometry() |> select("census_year", "geoid", "district", "school") |> distinct()

print(str_glue("Combining tracts with schools for Census 2010 and Census 2020 boundaries"))
tract_schools <- bind_rows(t10, t20) |> as_tibble()
rm(t10, t20, tract10, tract20)

# Step #5: Pull parcel data with tract id and population metrics from Elmer
parcel_data <- NULL
for (yr in analysis_years) {
  t <- process_ofm_tract_data_for_parcels(yrs = yr)
  if (is.null(parcel_data)) {parcel_data <- t} else {parcel_data <- bind_rows(parcel_data, t)}
  rm(t)
}

# Step #6: Get Population Data by Tract for each school for each year
school_tr_splits <- NULL
school_nms <- unique(schools$school)
for (yr in analysis_years) {
  
  if (yr < 2020) {census_yr <- 2010} else {census_yr <- 2020}
  if (yr < 2020) {tr_yr <- "tract_geoid10"} else {tr_yr <- "tract_geoid20"}
  
  for (schl in school_nms) {
    print(str_glue("Calculating tract shares for {schl} for the year {yr}"))
    # Tracts for school and census year of analysis year
    tr <- tract_schools |> filter(school == schl & census_year == census_yr) |> distinct() |> select("geoid") |> pull()
    
    # Parcels for school and analysis year 
    p <- parcel_data |> 
      filter(estimate_year == yr & .data[[tr_yr]] %in% tr) |>
      select(year = "estimate_year", "parcel_id", x = "x_coord_state_plane", y = "y_coord_state_plane", geoid = all_of(tr_yr), pop = "total_pop", hh_pop = "household_pop", hu = "housing_units", hh = "occupied_housing_units") |>
      mutate(geoid = as.character(geoid))
    
    # Tract Totals for Tracts that intersect Schools
    tr_total <- p |>
      group_by(year, geoid) |>
      summarise(tot_pop = round(sum(pop),0), tot_hh_pop = round(sum(hh_pop),0), tot_hu = round(sum(hu),0), tot_hh = round(sum(hh),0)) |>
      as_tibble()
    
    # Tract Totals for Parcels that intersect Schools
    p_lyr <- st_as_sf(p, coords = c("x", "y"), crs = spn)
    s_lyr <- schools |> filter(school == schl)
    
    tr_parcels <- st_intersection(p_lyr, s_lyr) |> 
      st_drop_geometry() |>
      group_by(year, school, geoid) |>
      summarise(pop = round(sum(pop),0), hh_pop = round(sum(hh_pop),0), hu = round(sum(hu),0), hh = round(sum(hh),0)) |>
      as_tibble() |>
      rename(planning_geog = "school")
    
    # Tract shares for school
    tbl <- left_join(tr_parcels, tr_total, by=c("year", "geoid")) |>
      mutate(percent_of_total_pop = pop/tot_pop) |>
      mutate(percent_of_household_pop = hh_pop/tot_hh_pop) |>
      mutate(percent_of_housing_units = hu/tot_hu) |>
      mutate(percent_of_occupied_housing_units = hh/tot_hh) |>
      mutate(percent_of_total_pop = replace_na(percent_of_total_pop, 0)) |>
      mutate(percent_of_household_pop = replace_na(percent_of_household_pop, 0)) |>
      mutate(percent_of_housing_units = replace_na(percent_of_housing_units, 0)) |>
      mutate(percent_of_occupied_housing_units = replace_na(percent_of_occupied_housing_units,0)) |>
      mutate(planning_geog_type = "Planning Academy School 2024") |>
      select("year", "geoid", "planning_geog_type", "planning_geog", "percent_of_total_pop", "percent_of_household_pop", "percent_of_housing_units", "percent_of_occupied_housing_units")
    
    if (is.null(school_tr_splits)) {school_tr_splits <- tbl} else {school_tr_splits <- bind_rows(school_tr_splits, tbl)}
    rm(tr, p, tr_total, p_lyr, s_lyr, tr_parcels, tbl)
  }
}

# Final Cleanup -----------------------------------------------------------
tr_splits <- bind_rows(school_tr_splits, tract_splits)
saveRDS(tr_splits, "data/tract_splits.rds")
rm(school_tr_splits, tract_splits)
