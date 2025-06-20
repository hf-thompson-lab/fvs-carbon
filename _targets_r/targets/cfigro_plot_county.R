tar_target(cfigro_plot_county, {
  massgis_county <- vect("data/raw/MassGIS/counties/COUNTIES_POLY.shp")
  
  tmp_countycd <- fia_tbl(fiadb, "COUNTY", \(.data, con) {
    .data |>
      filter(STATECD == 25) # Massachusetts has STATECD == 25
  }) |>
    mutate(COUNTY = toupper(COUNTYNM)) |>
    select(COUNTY, COUNTYCD, STATECD)
  
  tblDWSPCFIPlotsComplete |>
    # Remove rows without lat/lon
    filter(!is.na(GPSLatitude) & !is.na(GPSLongitude)) |>
    # select an arbitrary row with lat/lon for each plot
    # we assume that lat/lon is consistent over time
    group_by(MasterPlotID) |>
    filter(row_number() == 1) |>
    ungroup() |>
    # Reshape the data for terra::vect()
    select(MasterPlotID, GPSLatitude, GPSLongitude) |>
    rename(
      lat = GPSLatitude,
      lon = GPSLongitude
    ) |>
    vect(crs = "EPSG:4326") |>
    # Match the projection of the first tile
    project(massgis_county) |>
    intersect(massgis_county) |>
    as.data.frame() |>
    select(MasterPlotID, COUNTY) |>
    left_join(
      tmp_countycd,
      by = join_by(COUNTY)
    )
})
