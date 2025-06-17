tar_target(cfigro_plot, {
  qryDWSPCFIPlotVisitTreeDetail |>
    cfi_with_visit_info(tblDWSPCFIPlotVisitsComplete) |>
    cfi_with_tree_info(tblDWSPCFITreesComplete) |>
    cfi_with_plot_info(tblDWSPCFIPlotsComplete) |>
    cfi_abp(cfiabp_trees) |>
    group_by(MasterPlotID) |>
    mutate(
      FIRST_YEAR = VisitYear,
      LAST_YEAR = max(VisitYear, na.rm = TRUE)
    ) |>
    arrange(VisitCycle) |>
    filter(row_number() == 1) |>
    ungroup() |>
    left_join(
      cfigro_plot_elevft,
      by = join_by(MasterPlotID)
    ) |>
    left_join(
      cfigro_plot_county,
      by = join_by(MasterPlotID)
    ) |>
    mutate(
      STAND_CN = MasterPlotVisitID,
      STAND_ID = MasterPlotID,
      INV_YEAR = VisitYear,
      REGION = 9,
      FOREST = 22,
      LONGITUDE = GPSLongitude,
      LATITUDE = GPSLatitude,
      AGE = NA,
      ASPECT = Aspect,
      SLOPE = Slope,
      ELEVFT = ELEVFT,
      BASAL_AREA_FACTOR = 0 - conv_unit(1, "acre", "ft2") / (pi * 52.7^2),
      INV_PLOT_SIZE = conv_unit(1, "acre", "ft2") / (pi * 6^2),
      BRK_DBH = 6, # ABP uses 6" for break diameter; see ABP 2025
      NUM_PLOTS = 1,
      STATE = STATECD,
      COUNTY = COUNTYCD
    ) |>
    select(
      STAND_CN, STAND_ID, INV_YEAR, REGION, STATE, COUNTY, FOREST,
      LONGITUDE, LATITUDE, ASPECT, SLOPE, ELEVFT, AGE,
      BASAL_AREA_FACTOR, INV_PLOT_SIZE, BRK_DBH, NUM_PLOTS,
      FIRST_YEAR, LAST_YEAR
    )
})
