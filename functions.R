
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
