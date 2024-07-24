# Libraries -----------------------------------------------------------------
library(tidyverse)
library(tidytransit)
library(psrcelmer)
library(sf)

source("functions.R")

# Inputs ------------------------------------------------------------------
wgs84 <- 4326
spn <- 2285

rgc_title <- "Regional Growth Center (4/23/2024)"
school_title <- "Planning Academy School 2024"

gtfs_years <- c(2024)
gtfs_service <- "spring"

transit_ord <- c("All Transit Stops", "Bus", "Bus Rapid Transit", "Commuter Rail", "Ferry", "Light Rail or Streetcar")
county_order <- c("Region", "King County", "Kitsap County", "Pierce County", "Snohomish County")

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

# GTFS Data ---------------------------------------------------------------
transit_stops <- NULL
  for(y in gtfs_years) {
    s <- transit_stops_by_mode(year = y, service_change = gtfs_service)
    if(is.null(transit_stops)) {transit_stops <- s} else {transit_stops <- bind_rows(transit_stops, s)}
    rm(s)
}

transit_stop_lyr <- transit_stops |> 
  st_as_sf(coords = c("stop_lon", "stop_lat"), crs=wgs84) |> 
  st_transform(spn) |>
  mutate(type_name = case_when(
    type_name == "ST Express" ~ "Bus",
    type_name == "BRT" ~ "Bus Rapid Transit",
    type_name %in% c("Streetcar", "Light Rail") ~ "Light Rail or Streetcar",
    type_name %in% c("Passenger Ferry", "Auto Ferry") ~ "Ferry",
    type_name == "Commuter Rail" ~ "Commuter Rail",
    type_name == "Bus" ~ "Bus"))
    
transit_modes <- transit_stop_lyr |> st_drop_geometry() |> select(mode = "type_name") |> distinct() |> arrange(mode)

# Stops by School -----------------------------------------------
school_stop_data <- NULL
# Summarize by School Boundary
for (schl in school_names) {
  print(str_glue("Sumarzing stops for {schl}."))
  
  # Stops that intersect Schools
  s_lyr <- schools |> filter(school == schl)
  
  j_lyr <- st_intersection(transit_stop_lyr, s_lyr) |> 
    st_drop_geometry() |>
    select("school", mode = "type_name") |>
    mutate(stops = 1) |>
    group_by(mode) |>
    summarise(stops = round(sum(stops),0)) |>
    as_tibble()
  
  s <- left_join(transit_modes, j_lyr, by=c("mode")) |> mutate(geography = schl) |> mutate(stops = replace_na(stops, 0))
  
  total <- s |> group_by(geography) |> summarise(stops = sum(stops)) |> as_tibble() |> mutate(mode = "All Transit Stops")
  
  s <- bind_rows(s, total) |> mutate(mode = factor(mode, levels = transit_ord)) |> arrange(mode) 
  
  if (is.null(school_stop_data)) {school_stop_data <- s} else {school_stop_data <- bind_rows(school_stop_data, s)}
  rm(j_lyr, s_lyr, s, total)
}

# Summarize for All Schools
s <- school_stop_data |>
  group_by(mode) |>
  summarise(stops = round(sum(stops), 0)) |>
  as_tibble() |>
  mutate(geography = "All Schools")

school_stop_data <- bind_rows(school_stop_data, s) |> 
  mutate(geography_type = school_title) |>
  select("geography", "geography_type", "mode", "stops")
rm(s)

# Stops by Center ---------------------------------------------------------
center_stop_data <- NULL
# Summarize by Center Boundary
for (rgc in rgc_names) {
  print(str_glue("Sumarzing stops for {rgc}."))
  
  # Stops that intersect Centers
  s_lyr <- rgc_shape |> filter(name == rgc)
  
  j_lyr <- st_intersection(transit_stop_lyr, s_lyr) |> 
    st_drop_geometry() |>
    select("name", mode = "type_name") |>
    mutate(stops = 1) |>
    group_by(mode) |>
    summarise(stops = round(sum(stops),0)) |>
    as_tibble()
  
  s <- left_join(transit_modes, j_lyr, by=c("mode")) |> mutate(geography = rgc) |> mutate(stops = replace_na(stops, 0))
  
  total <- s |> group_by(geography) |> summarise(stops = sum(stops)) |> as_tibble() |> mutate(mode = "All Transit Stops")
  
  s <- bind_rows(s, total) |> mutate(mode = factor(mode, levels = transit_ord)) |> arrange(mode) |> mutate(geography_type = rgc_title)
  
  if (is.null(center_stop_data)) {center_stop_data <- s} else {center_stop_data <- bind_rows(center_stop_data, s)}
  rm(j_lyr, s_lyr, s, total)
}

# Summarize for All Centers
s <- center_stop_data |>
  group_by(mode) |>
  summarise(stops = round(sum(stops), 0)) |>
  as_tibble() |>
  mutate(geography = "All RGCs", geography_type = rgc_title)

center_stop_data <- bind_rows(center_stop_data, s) |> 
  mutate(geography_type = rgc_title) |>
  select("geography", "geography_type", "mode", "stops")
rm(s)

ord <- unique(c(county_order, "All RGCs", rgc_names, "All Schools", school_names))
transit_stop_data <- bind_rows(school_stop_data, center_stop_data) |>
  mutate(geography = factor(geography, levels = ord)) |> 
  arrange(geography, mode)

rm(center_stop_data, school_stop_data)
saveRDS(transit_stop_data, "data/transit_stop_data.rds")

transit_stop_lyr <- transit_stop_lyr |> st_transform(wgs84)
saveRDS(transit_stop_lyr, "data/transit_stop_lyr.rds")

