
generate_blockgroup_splits <- function(y) {
  
  if (y >=2020) {
    ofm_vin <- y
    geog_yr <- 'blockgroup20'
    
  } else {
    ofm_vin <- 2020
    geog_yr <- 'blockgroup10'}
  
  if (y >=2018) {parcel_yr <- 2018} else {parcel_yr <- 2014}
  
  # Regional Growth Centers 
  print(str_glue("Getting Blockgroup splits from Elmer for {geog_yr} for {rgc_title} for the year {y}"))
  q <- paste0("SELECT * FROM general.get_geography_splits('", geog_yr , "', '", rgc_title, "', ", y, ", ", ofm_vin,", ", parcel_yr,")")
  
  splits <- get_query(sql = q, db_name = "Elmer") |> 
    filter(planning_geog != "not in regional growth center") |> 
    mutate(planning_geog = str_replace_all(planning_geog, "Greater Downtown Kirkland", "Kirkland Greater Downtown")) |>
    mutate(planning_geog = str_replace_all(planning_geog, "Redmond-Overlake", "Redmond Overlake")) |>
    mutate(planning_geog = str_replace_all(planning_geog, "Bellevue", "Bellevue Downtown"))
  
  return(splits)
  
}

generate_tract_splits <- function(y) {
  
  if (y >=2020) {
    ofm_vin <- y
    geog_yr <- 'tract20'
    
  } else {
    ofm_vin <- 2020
    geog_yr <- 'tract10'}
  
  if (y >=2018) {parcel_yr <- 2018} else {parcel_yr <- 2014}
  
  # Regional Growth Centers 
  print(str_glue("Getting tract splits from Elmer for {geog_yr} for {rgc_title} for the year {y}"))
  q <- paste0("SELECT * FROM general.get_geography_splits('", geog_yr , "', '", rgc_title, "', ", y, ", ", ofm_vin,", ", parcel_yr,")")
  
  splits <- get_query(sql = q, db_name = "Elmer") |> 
    filter(planning_geog != "not in regional growth center") |> 
    mutate(planning_geog = str_replace_all(planning_geog, "Greater Downtown Kirkland", "Kirkland Greater Downtown")) |>
    mutate(planning_geog = str_replace_all(planning_geog, "Redmond-Overlake", "Redmond Overlake")) |>
    mutate(planning_geog = str_replace_all(planning_geog, "Bellevue", "Bellevue Downtown"))
  
  return(splits)
  
}

process_ofm_bg_data_for_parcels <- function(yrs) {
  
  centers_population_housing <- NULL
  for (y in yrs) {
    
    # Parcel population
    print(str_glue("Loading {y} OFM based parcelized estimates of total population"))
    if (y >= 2020) {ofm_vintage <- y} else {ofm_vintage <- 2020}
    q <- paste0("SELECT parcel_dim_id, estimate_year, total_pop, household_pop, housing_units, occupied_housing_units from ofm.parcelized_saep_facts WHERE ofm_vintage = ", ofm_vintage, " AND estimate_year = ", y, "")
    p <- get_query(sql = q)
    
    # Parcel Dimensions
    if (y >=2018) {parcel_yr <- 2018} else {parcel_yr <- 2014}
    print(str_glue("Loading {parcel_yr} parcel dimensions from Elmer"))
    q <- paste0("SELECT parcel_dim_id, parcel_id, block_group_geoid10, block_group_geoid20, x_coord_state_plane, y_coord_state_plane from small_areas.parcel_dim WHERE base_year = ", parcel_yr, " ")
    d <- get_query(sql = q) 
    
    # Add 2010 and 2020 Blockgroup IDs to Parcels
    p <- left_join(p, d, by="parcel_dim_id")
    
    if (is.null(centers_population_housing)) {centers_population_housing <- p} else {centers_population_housing <- bind_rows(centers_population_housing, p)}
    
  }
  
  return(centers_population_housing)

}

process_ofm_tract_data_for_parcels <- function(yrs) {
  
  centers_population_housing <- NULL
  for (y in yrs) {
    
    # Parcel population
    print(str_glue("Loading {y} OFM based parcelized estimates of total population"))
    if (y >= 2020) {ofm_vintage <- y} else {ofm_vintage <- 2020}
    q <- paste0("SELECT parcel_dim_id, estimate_year, total_pop, household_pop, housing_units, occupied_housing_units from ofm.parcelized_saep_facts WHERE ofm_vintage = ", ofm_vintage, " AND estimate_year = ", y, "")
    p <- get_query(sql = q)
    
    # Parcel Dimensions
    if (y >=2018) {parcel_yr <- 2018} else {parcel_yr <- 2014}
    print(str_glue("Loading {parcel_yr} parcel dimensions from Elmer"))
    q <- paste0("SELECT parcel_dim_id, parcel_id, tract_geoid10, tract_geoid20, x_coord_state_plane, y_coord_state_plane from small_areas.parcel_dim WHERE base_year = ", parcel_yr, " ")
    d <- get_query(sql = q) 
    
    # Add 2010 and 2020 Tract IDs to Parcels
    p <- left_join(p, d, by="parcel_dim_id")
    
    if (is.null(centers_population_housing)) {centers_population_housing <- p} else {centers_population_housing <- bind_rows(centers_population_housing, p)}
    
  }
  
  return(centers_population_housing)
  
}

geography_estimate_from_bg <- function(split_df=blockgroup_splits, estimate_df=blockgroups, geography_type, split_type, geography_name) {
  
  if (geography_name %in% c("All RGCs", "All Schools")) {
    
    t <- split_df |> filter(planning_geog_type == geography_type) |> mutate(planning_geog = geography_name)
    
  } else {
    
    t <- split_df |> filter(planning_geog_type == geography_type & planning_geog == geography_name)
    
  }
  
  # Filter Blockgroup Splits to Geography
  t <- t |> select("year", "geoid", name = "planning_geog", share = all_of(split_type)) |> mutate(year = as.character(year))
  
  d <- estimate_df |> select("year","geoid", "grouping", "concept","estimate")
  
  c <- left_join(t, d, by=c("year", "geoid"), relationship = "many-to-many") |>
    mutate(place_estimate = round(estimate*share,0)) |>
    group_by(year, name, grouping, concept) |>
    summarise(estimate = sum(place_estimate)) |>
    as_tibble() |>
    rename(geography="name") |>
    mutate(geography_type = geography_type)
  
  totals <- c |>
    filter(grouping == "Total") |>
    select("geography", "year", "concept", total="estimate")
  
  c <- left_join(c, totals, by=c("geography", "year", "concept")) |>
    mutate(share = estimate / total) |>
    select(-"total")
  
  return(c)
  
}

transit_stops_by_mode <- function(year, service_change) {
  
  hct_file <- "data/hct_ids.csv"
  hct <- read_csv(hct_file, show_col_types = FALSE) 
  
  if (tolower(service_change)=="spring") {data_month = "05"} else (data_month = "10")
  
  options(dplyr.summarise.inform = FALSE)
  gtfs_file <- paste0("X:/DSA/GTFS/",tolower(service_change),"/",as.character(year),".zip")
  
  # Open Regional GTFS File and load into memory
  print(str_glue("Opening the {service_change} {year} GTFS archive."))
  gtfs <- read_gtfs(path=gtfs_file, files = c("trips","stops","stop_times", "routes", "shapes"))
  
  # Load Stops
  print(str_glue("Getting the {service_change} {year} stops into a tibble." ))
  stops <- as_tibble(gtfs$stops) |> 
    mutate(stop_id = str_to_lower(stop_id)) |>
    select("stop_id", "stop_name", "stop_lat", "stop_lon")
  
  # Load Routes, add HCT modes and update names and agencies
  print(str_glue("Getting the {service_change} {year} routes into a tibble." ))
  routes <- as_tibble(gtfs$routes) |> 
    mutate(route_id = str_to_lower(route_id)) |>
    select("route_id", "agency_id","route_short_name", "route_long_name", "route_type")
  
  print(str_glue("Adding High-Capacity Transit codes to the {service_change} {year} routes"))
  routes <- left_join(routes, hct, by="route_id") |>
    mutate(type_code = case_when(
      is.na(type_code) ~ route_type,
      !(is.na(type_code)) ~ type_code)) |>
    mutate(route_name = case_when(
      is.na(route_name) ~ route_short_name,
      !(is.na(route_name)) ~ route_name)) |>
    mutate(type_name = case_when(
      is.na(type_name) ~ "Bus",
      !(is.na(type_name)) ~ type_name)) |>
    mutate(agency_name = case_when(
      !(is.na(agency_name)) ~ agency_name,
      is.na(agency_name) & str_detect(route_id, "ct") ~ "Community Transit",
      is.na(agency_name) & str_detect(route_id, "et") ~ "Everett Transit",
      is.na(agency_name) & str_detect(route_id, "kc") ~ "King County Metro",
      is.na(agency_name) & str_detect(route_id, "kt") ~ "Kitsap Transit",
      is.na(agency_name) & str_detect(route_id, "pt") ~ "Pierce Transit",
      is.na(agency_name) & str_detect(route_id, "st") ~ "Sound Transit")) |>
    select("route_id", "route_name", "type_name", "type_code", "agency_name")
  
  # Trips are used to get route id onto stop times
  print(str_glue("Getting the {service_change} {year} trips into a tibble to add route ID to stop times." ))
  trips <- as_tibble(gtfs$trips) |> 
    mutate(route_id = str_to_lower(route_id)) |>
    select("trip_id", "route_id")
  
  trips <- left_join(trips, routes, by=c("route_id"))
  
  # Clean Up Stop Times to get routes and mode by stops served
  print(str_glue("Getting the {service_change} {year} stop times into a tibble to add route information." ))
  stoptimes <- as_tibble(gtfs$stop_times) |>
    mutate(stop_id = str_to_lower(stop_id)) |>
    select("trip_id", "stop_id")
  
  # Get Mode and agency from trips to stops
  print(str_glue("Getting unique stop list by modes for the {service_change} {year}." ))
  stops_by_mode <- left_join(stoptimes, trips, by=c("trip_id")) |>
    select("stop_id", "type_code", "type_name", "agency_name") |>
    distinct()
  
  stops_by_mode <- left_join(stops_by_mode, stops, by=c("stop_id")) |>
    mutate(date=mdy(paste0(data_month,"-01-",year)))
  
  print(str_glue("All Done."))
  
  return(stops_by_mode)
  
}

transit_routes_by_mode <- function(year, service_change) {
  
  hct_file <- "data/hct_ids.csv"
  hct <- read_csv(hct_file, show_col_types = FALSE) 
  
  if (tolower(service_change)=="spring") {data_month = "05"} else (data_month = "10")
  
  options(dplyr.summarise.inform = FALSE)
  gtfs_file <- paste0("X:/DSA/GTFS/",tolower(service_change),"/",as.character(year),".zip")
  
  # Open Regional GTFS File and load into memory
  print(str_glue("Opening the {service_change} {year} GTFS archive."))
  gtfs <- read_gtfs(path=gtfs_file, files = c("trips","stops","stop_times", "routes", "shapes"))
  
  # Load Routes, add HCT modes and update names and agencies
  print(str_glue("Getting the {service_change} {year} routes into a tibble." ))
  routes <- as_tibble(gtfs$routes) |> 
    mutate(route_id = str_to_lower(route_id)) |>
    select("route_id", "agency_id","route_short_name", "route_long_name", "route_type")
  
  print(str_glue("Adding High-Capacity Transit codes to the {service_change} {year} routes"))
  routes <- left_join(routes, hct, by="route_id") |>
    mutate(type_code = case_when(
      is.na(type_code) ~ route_type,
      !(is.na(type_code)) ~ type_code)) |>
    mutate(route_name = case_when(
      is.na(route_name) ~ route_short_name,
      !(is.na(route_name)) ~ route_name)) |>
    mutate(type_name = case_when(
      is.na(type_name) ~ "Bus",
      !(is.na(type_name)) ~ type_name)) |>
    mutate(agency_name = case_when(
      !(is.na(agency_name)) ~ agency_name,
      is.na(agency_name) & str_detect(route_id, "ct") ~ "Community Transit",
      is.na(agency_name) & str_detect(route_id, "et") ~ "Everett Transit",
      is.na(agency_name) & str_detect(route_id, "kc") ~ "King County Metro",
      is.na(agency_name) & str_detect(route_id, "kt") ~ "Kitsap Transit",
      is.na(agency_name) & str_detect(route_id, "pt") ~ "Pierce Transit",
      is.na(agency_name) & str_detect(route_id, "st") ~ "Sound Transit")) |>
    select("route_id", "route_name", "type_name", "type_code", "agency_name")
  
  # Load Route Shapes to get Mode information on layers
  route_lyr <- shapes_as_sf(gtfs$shapes)
  
  # Trips are used to get route id onto shapes
  print(str_glue("Getting the {service_change} {year} trips into a tibble to add route ID to stop times." ))
  trips <- as_tibble(gtfs$trips) |> 
    mutate(route_id = str_to_lower(route_id)) |>
    select("route_id", "shape_id") |>
    distinct()
  
  route_lyr <- left_join(route_lyr, trips, by=c("shape_id"))
  
  # Get Mode and agency from routes to shapes
  print(str_glue("Getting route details onto shapes for the {service_change} {year}." ))
  route_lyr <- left_join(route_lyr, routes, by=c("route_id")) |> mutate(date=mdy(paste0(data_month,"-01-",year)))
  
  print(str_glue("All Done."))
  
  return(route_lyr)
  
}
