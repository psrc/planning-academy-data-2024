library(tidyverse)
library(psrcelmer)
library(sf)

# Inputs ------------------------------------------

# Data Years
analysis_years <- c(2012, 2017, 2021)

# Make sure these match the boundary definition you want the data to be based on
rgc_elmer_title <- "regional_growth_center_2024_04_23"
rgc_title <- "Regional Growth Center (4/23/2024)"
school_title <- "Planning Academy School 2024"

# Coordinate Reference Systems
wgs84 <- 4326
spn <- 2285

# Silence the dplyr summarize message
options(dplyr.summarise.inform = FALSE)

county_order <- c("Region", "King County", "Kitsap County", "Pierce County", "Snohomish County")
year_ord <- c("2022", "2021", "2017", "2012")
sector_ord <- c("Construction / Resources", "Manufacturing", "Wholesale, Transportation & Utilities",
                "Accommodation, Entertainment & Food Services", "Retail",  
                "Finance, Insurance & Real Estate", "Professional Services", "Health & Social Services", "Other Services",
                "Government", "Public Education", "Total")

# Center Shapefiles -------------------------------------------------------
rgc_shape <- st_read_elmergeo(layer_name = "urban_centers") |>
  st_transform(crs = spn) |>
  mutate(name = gsub("Bellevue", "Bellevue Downtown", name)) |>
  mutate(name = gsub("Redmond-Overlake", "Redmond Overlake", name)) |>
  mutate(name = gsub("Greater Downtown Kirkland", "Kirkland Greater Downtown", name)) |>
  select("name", "category", "acres") |>
  rename(geometry="Shape")

rgc_names <- rgc_shape |> 
  select("name") |> 
  st_drop_geometry() |> 
  pull() |>
  unique() |>
  sort()

# School Shapefile --------------------------------------------------------
schools <- st_read("X:/DSA/shiny-uploads/planning-academy/summer_planning_academy_schools.shp") |> st_transform(spn)

school_names <- schools |> 
  select("school") |> 
  st_drop_geometry() |> 
  pull() |>
  unique() |>
  sort()

# 2020 Census Blocks ------------------------------------------------------
block_shape <- st_read_elmergeo(layer_name = "block2020") |> 
  st_transform(crs = spn) |>  
  select("geoid20", "county_name") |> 
  mutate(county_name = paste0(county_name, " County")) |>
  rename(geometry="Shape") |>
  st_centroid()

# Employment Data by Census Block from LODES ---------------------------------------------
lodes_sectors <- read_csv("data/lodes-sector-lookup.csv", show_col_types = FALSE) |> select("lodes_field", employment_sector = "simplified_sector")

jobs_data <- NULL
for (y in analysis_years) {
  
  # LODES data
  print(str_glue("Processing {y} LODES based employment data and joining to block centroids"))
  j <- read_csv(paste0("X:/DSA/shiny-uploads/planning-academy/wa_wac_S000_JT00_", y, ".csv"), show_col_types = FALSE) |>
    select("w_geocode", "C000", contains("CNS")) |>
    rename(geoid20 = "w_geocode") |>
    mutate(geoid20 = as.character(geoid20))
  
  block_jobs <- left_join(block_shape, j, by="geoid20") |> drop_na()
  
  # Summarize by School Boundary
  for (schl in school_names) {
    print(str_glue("Sumarzing block job data for {schl}."))
    
    # Blocks that intersect Schools
    s_lyr <- schools |> filter(school == schl)
    
    j_lyr <- st_intersection(block_jobs, s_lyr) |> 
      st_drop_geometry() |>
      select(-"county_name", -"district") |>
      pivot_longer(!c(geoid20, school), names_to = "lodes_field", values_to = "estimate") |>
      as_tibble() |>
      group_by(school, lodes_field) |>
      summarise(estimate = round(sum(estimate),-1)) |>
      as_tibble() |>
      rename(geography = "school") 
    
    j_lyr <- left_join(j_lyr, lodes_sectors, by=c("lodes_field")) |>
      group_by(geography, employment_sector) |>
      summarise(estimate = round(sum(estimate), -1)) |>
      as_tibble() |>
      rename(grouping = "employment_sector") |>
      mutate(year = y, concept = "Employment", geography_type = school_title)
    
    total <- j_lyr |> filter(grouping == "Total") |> select("geography", total="estimate")
    
    j_lyr <- left_join(j_lyr, total, by=c("geography")) |> mutate(share = estimate/total) |> select(-"total")
    
    if (is.null(jobs_data)) {jobs_data <- j_lyr} else {jobs_data <- bind_rows(jobs_data, j_lyr)}
    rm(j_lyr, s_lyr, total)
  }
  
  # Summarize for All Schools
  j_lyr <- jobs_data |>
    filter(geography_type == school_title & year == y) |>
    group_by(year, grouping, concept, geography_type) |>
    summarise(estimate = round(sum(estimate), -1)) |>
    as_tibble() |>
    mutate(geography = "All Schools")
  
  total <- j_lyr |> filter(grouping == "Total") |> select("geography", total="estimate")
  
  j_lyr <- left_join(j_lyr, total, by=c("geography")) |> mutate(share = estimate/total) |> select(-"total")
  
  jobs_data <- bind_rows(jobs_data, j_lyr)
  rm(j_lyr, total)
    
  # Summarize by Center Boundary
  for (center in rgc_names) {
    print(str_glue("Sumarzing block job data for {center}."))
      
    # Blocks that intersect Centers
    s_lyr <- rgc_shape |> filter(name == center)
      
    j_lyr <- st_intersection(block_jobs, s_lyr) |> 
      st_drop_geometry() |>
      select(-"county_name", -"category", -"acres") |>
      pivot_longer(!c(geoid20, name), names_to = "lodes_field", values_to = "estimate") |>
      as_tibble() |>
      group_by(name, lodes_field) |>
      summarise(estimate = round(sum(estimate),-1)) |>
      as_tibble() |>
      rename(geography = "name") 
      
    j_lyr <- left_join(j_lyr, lodes_sectors, by=c("lodes_field")) |>
      group_by(geography, employment_sector) |>
      summarise(estimate = round(sum(estimate), -1)) |>
      as_tibble() |>
      rename(grouping = "employment_sector") |>
      mutate(year = y, concept = "Employment", geography_type = rgc_title)
      
    total <- j_lyr |> filter(grouping == "Total") |> select("geography", total="estimate")
      
    j_lyr <- left_join(j_lyr, total, by=c("geography")) |> mutate(share = estimate/total) |> select(-"total")
      
    jobs_data <- bind_rows(jobs_data, j_lyr)
    rm(j_lyr, s_lyr, total)
  }
  
  # Summarize for All Centers
  j_lyr <- jobs_data |>
    filter(geography_type == rgc_title & year == y) |>
    group_by(year, grouping, concept, geography_type) |>
    summarise(estimate = round(sum(estimate), -1)) |>
    as_tibble() |>
    mutate(geography = "All RGCs")
  
  total <- j_lyr |> filter(grouping == "Total") |> select("geography", total="estimate")
  
  j_lyr <- left_join(j_lyr, total, by=c("geography")) |> mutate(share = estimate/total) |> select(-"total")
  
  jobs_data <- bind_rows(jobs_data, j_lyr)
  rm(j_lyr, total)
  
  # Summarize by County
  j_lyr <- block_jobs |> 
    st_drop_geometry() |>
    select(-"geoid20") |>
    pivot_longer(!c(county_name), names_to = "lodes_field", values_to = "estimate") |>
    as_tibble() |>
    group_by(county_name, lodes_field) |>
    summarise(estimate = round(sum(estimate),-1)) |>
    as_tibble() |>
    rename(geography = "county_name") 
  
  j_lyr <- left_join(j_lyr, lodes_sectors, by=c("lodes_field")) |>
    group_by(geography, employment_sector) |>
    summarise(estimate = round(sum(estimate), -1)) |>
    as_tibble() |>
    rename(grouping = "employment_sector") |>
    mutate(year = y, concept = "Employment", geography_type = "County")
  
  total <- j_lyr |> filter(grouping == "Total") |> select("geography", total="estimate")
  
  j_lyr <- left_join(j_lyr, total, by=c("geography")) |> mutate(share = estimate/total) |> select(-"total")
  jobs_data <- bind_rows(jobs_data, j_lyr)
  rm(j_lyr, total)
  
  # Summarize by Region
  j_lyr <- block_jobs |> 
    st_drop_geometry() |>
    select(-"geoid20") |>
    mutate(county_name = "Region") |>
    pivot_longer(!c(county_name), names_to = "lodes_field", values_to = "estimate") |>
    as_tibble() |>
    group_by(county_name, lodes_field) |>
    summarise(estimate = round(sum(estimate),-1)) |>
    as_tibble() |>
    rename(geography = "county_name") 
  
  j_lyr <- left_join(j_lyr, lodes_sectors, by=c("lodes_field")) |>
    group_by(geography, employment_sector) |>
    summarise(estimate = round(sum(estimate), -1)) |>
    as_tibble() |>
    rename(grouping = "employment_sector") |>
    mutate(year = y, concept = "Employment", geography_type = "Region")
  
  total <- j_lyr |> filter(grouping == "Total") |> select("geography", total="estimate")
  
  j_lyr <- left_join(j_lyr, total, by=c("geography")) |> mutate(share = estimate/total) |> select(-"total")
  jobs_data <- bind_rows(jobs_data, j_lyr)
  rm(j_lyr, total, j, block_jobs)
  
}

# Final Cleanup -----------------------------------------------------------
ord <- unique(c(county_order, "All RGCs", rgc_names, "All Schools", school_names))
final_data <- jobs_data |> 
  mutate(year = factor(year, levels = year_ord)) |>
  mutate(geography = factor(geography, levels = ord)) |> 
  mutate(grouping = factor(grouping, levels = sector_ord)) |>
  arrange(geography, grouping, year)

saveRDS(final_data, "data/jobs_data.rds")
