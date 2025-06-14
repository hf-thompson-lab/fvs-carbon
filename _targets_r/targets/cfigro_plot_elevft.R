tar_target(cfigro_plot_elevft, {
  # Files for tile NN are named
  # MassGIS/LidarElevation_YYYYtoZZZZ_NN/Lidar_Elevation_YYYYtoZZZZ_NN.img
  # We'll keep this as a list and extract() each tile separately - this is
  # much, MUCH faster than merging tiles with terra::merge() or terra::m
  massgis_dem_tiles <- lapply(
    list.files("data/raw/MassGIS", pattern = "Lidar_Elevation_*"),
    \(tile) { rast(file.path("data/raw/MassGIS", tile, paste0(tile, ".img"))) }
  )
  
  tmp_plot_location <- tblDWSPCFIPlotsComplete |>
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
    project(massgis_dem_tiles[[1]])
  
  # Build a dataframe with two columns:
  # MasterPlotID, ELEVFT
  # with one row per MasterPlotID
  as.data.frame(tmp_plot_location) |>
    mutate(
      # Call extract() on each available tile; for plots that overlap
      # with that tile, we'll get back elevation, otherwise NA.
      # Coalesce across tiles to get the non-NA elevation for
      # each plot.
      ELEVFT = do.call(
        coalesce,
        lapply(massgis_dem_tiles, \(x) extract(x, tmp_plot_location)$FEET)
      )
    )
})
