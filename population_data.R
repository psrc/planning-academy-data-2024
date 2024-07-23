library(tidyverse)
library(psrcelmer)
library(sf)

# Inputs ------------------------------------------

# Data Years
pre_api_years <- c(2012)
api_years <- c(2017, 2022)
analysis_years <- c(pre_api_years, api_years)

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
year_ord <- c("2022", "2017", "2012")
metric_ord <- c("Population", "Households", "Housing Units")

# Center Shapefiles -------------------------------------------------------
rgc_shape <- st_read_elmergeo(layer_name = "urban_centers") |>
  st_transform(crs = spn) |>
  mutate(name = gsub("Bellevue", "Bellevue Downtown", name)) |>
  mutate(name = gsub("Redmond-Overlake", "Redmond Overlake", name)) |>
  mutate(name = gsub("Greater Downtown Kirkland", "Kirkland Greater Downtown", name)) |>
  select("name", "category", "acres")

rgc_names <- rgc_shape |> 
  select("name") |> 
  st_drop_geometry() |> 
  pull() |>
  unique() |>
  sort()

# Parcelized Population data from Elmer ---------------------------------------------
population_data <- NULL
for (y in analysis_years) {
  
  # Parcel population
  print(str_glue("Loading {y} OFM based parcelized estimates of total population"))
  if (y >= 2020) {ofm_vintage <- y} else {ofm_vintage <- 2020}
  q <- paste0("SELECT parcel_dim_id, estimate_year, total_pop, household_pop, housing_units, occupied_housing_units from ofm.parcelized_saep_facts WHERE ofm_vintage = ", ofm_vintage, " AND estimate_year = ", y, "")
  p <- get_query(sql = q)
  
  # Parcel Dimensions
  if (y >=2018) {parcel_yr <- 2018} else {parcel_yr <- 2014}
  print(str_glue("Loading {parcel_yr} parcel dimensions from Elmer"))
  q <- paste0("SELECT parcel_dim_id, parcel_id, county_name, ", rgc_elmer_title, ", x_coord_state_plane, y_coord_state_plane from small_areas.parcel_dim WHERE base_year = ", parcel_yr, " ")
  d <- get_query(sql = q) 
  
  # Add 2010 and 2020 Blockgroup IDs to Parcels
  p <- left_join(p, d, by="parcel_dim_id")
  
  if (is.null(population_data)) {population_data <- p} else {population_data <- bind_rows(population_data, p)}
  
  rm(q, p, d)
  
}

# Population data by Center -----------------------------------------------
rgc <- population_data |>
  select(year = "estimate_year", geography = all_of(rgc_elmer_title), population = "total_pop", "housing_units", households = "occupied_housing_units") |>
  filter(geography != "Not in Center") |>
  mutate(geography = gsub("Bellevue", "Bellevue Downtown", geography)) |>
  mutate(geography = gsub("Redmond-Overlake", "Redmond Overlake", geography)) |>
  mutate(geography = gsub("Greater Downtown Kirkland", "Kirkland Greater Downtown", geography)) |>
  group_by(year, geography) |>
  summarise(population = round(sum(population),0), housing_units = round(sum(housing_units),0), households = round(sum(households),0)) |>
  as_tibble() |>
  pivot_longer(!c(year, geography), names_to = "grouping", values_to = "estimate") |>
  mutate(concept = "People, Housing and Households", share = 0, geography_type = rgc_title) |>
  as_tibble()

all_rgcs <- rgc |>
  group_by(year, grouping, concept, geography_type) |>
  summarise(estimate = sum(estimate), share = sum(share)) |>
  as_tibble() |>
  mutate(geography ="All RGCs")

rgc <- bind_rows(rgc, all_rgcs)
rm(all_rgcs)

# Population data by County ----------------------------------------------------
county <- population_data |>
  select(year = "estimate_year", geography = "county_name", population = "total_pop", "housing_units", households = "occupied_housing_units") |>
  mutate(geography = paste0(geography, " County")) |>
  group_by(year, geography) |>
  summarise(population = round(sum(population),0), housing_units = round(sum(housing_units),0), households = round(sum(households),0)) |>
  as_tibble() |>
  pivot_longer(!c(year, geography), names_to = "grouping", values_to = "estimate") |>
  mutate(concept = "People, Housing and Households", share = 0, geography_type = "County") |>
  as_tibble()

region <- county |>
  group_by(year, grouping, concept, geography_type) |>
  summarise(estimate = sum(estimate), share = sum(share)) |>
  as_tibble() |>
  mutate(geography ="Region", geography_type = "Region")

county <- bind_rows(county, region)
rm(region)

# Population data by Planning Academy Schools --------------------------
print(str_glue("Reading in layer of Summer Planning Acadmey School Boundaries"))
schools <- st_read("X:/DSA/shiny-uploads/planning-academy/summer_planning_academy_schools.shp") |> st_transform(spn)

school_names <- schools |> 
  select("school") |> 
  st_drop_geometry() |> 
  pull() |>
  unique() |>
  sort()

print(str_glue("Creating a parcel layer to intersection with school boundries"))
p_lyr <- population_data |> select(year = "estimate_year", "parcel_id", x = "x_coord_state_plane", y = "y_coord_state_plane", population = "total_pop", "housing_units", households = "occupied_housing_units")
p_lyr <- st_as_sf(p_lyr, coords = c("x", "y"), crs = spn)

# Step #6: Get Population Data by Blockgroup for each school for each year
school_data <- NULL
school_nms <- unique(schools$school)
for (schl in school_nms) {
  print(str_glue("Sumarzing parcel population and housing data for {schl}."))
    
  # Parcels that intersect Schools
  s_lyr <- schools |> filter(school == schl)
    
  m_lyr <- st_intersection(p_lyr, s_lyr) |> 
    st_drop_geometry() |>
    group_by(year, school) |>
    summarise(population = round(sum(population),0), housing_units = round(sum(housing_units),0), households = round(sum(households),0)) |>
    as_tibble() |>
    rename(geography = "school") |>
    pivot_longer(!c(year, geography), names_to = "grouping", values_to = "estimate") |>
    mutate(concept = "People, Housing and Households", share = 0, geography_type = school_title) |>
    as_tibble()
    
  if (is.null(school_data)) {school_data <- m_lyr} else {school_data <- bind_rows(school_data, m_lyr)}
  rm(m_lyr, s_lyr)
}

all_schools <- school_data |>
  group_by(year, grouping, concept, geography_type) |>
  summarise(estimate = sum(estimate), share = sum(share)) |>
  as_tibble() |>
  mutate(geography ="All Schools")

school_data <- bind_rows(school_data, all_schools)
rm(all_schools)

# Final Cleanup -----------------------------------------------------------
final_data <- bind_rows(county, rgc, school_data)
ord <- unique(c(county_order, "All RGCs", rgc_names, "All Schools", school_names))
final_data <- final_data |> 
  mutate(grouping = str_replace_all(grouping, "population", "Population")) |>
  mutate(grouping = str_replace_all(grouping, "households", "Households")) |>
  mutate(grouping = str_replace_all(grouping, "housing_units", "Housing Units")) |>
  mutate(year = factor(year, levels = year_ord)) |>
  mutate(geography = factor(geography, levels = ord)) |> 
  mutate(grouping = factor(grouping, levels = metric_ord)) |>
  arrange(geography, grouping, year)

saveRDS(final_data, "data/population-housing.rds")
