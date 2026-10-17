tar_target(cfigro_estab, {
  cycle_length <- 10
  plots_per_acre <- 5
  survey_period <- 30 # 1990 - 2020
  minimum_sane_height_for_a_5inch_dbh_tree <- 10
  
  # Trees to use for modeling height estimation
  qryDWSPCFIPlotVisitTreeDetail |>
    filter(VisitTreeStatusCode == 1) |>
    left_join(
      tblDWSPCFIPlotVisitsComplete |>
        select(MasterPlotID, VisitCycle, MasterPlotVisitID),
      by = join_by(MasterPlotVisitID)
    ) |>
    left_join( 
      tblDWSPCFITreesComplete |> select(MasterTreeID, SpeciesCode),
      by = join_by(MasterTreeID)
    ) |>
    inner_join(
      cfiabp_trees |> distinct(MasterPlotID, VisitCycle, MasterTreeID, StatusB),
      by = join_by(MasterPlotID, VisitCycle, MasterTreeID)
    ) |>
    filter(VisitCycle >= 1970) |>
    filter(StatusB == "R") |> 
    # Remove suspicious heights
    mutate(VisitTreeTotalHeight = if_else(VisitTreeTotalHeight < 10, NA, VisitTreeTotalHeight)) |>
    group_by(SpeciesCode) |>
    mutate(SPECIES_MIN_HT = min(VisitTreeTotalHeight, na.rm = TRUE), .groups = "keep") |>
    ungroup() |>
    # 1970 and 1980 have huge influxes of trees that we're dealing with
    # separately, so don't double count those.
    # filter recruits to after 1980.
    filter(VisitCycle >= 1990) |>
    group_by(MasterPlotID, SpeciesCode) |>
    summarize(
      # Stems per acre per 10-year interval
      DENSITY = n() * plots_per_acre * cycle_length / survey_period,
      HEIGHT = min(VisitTreeTotalHeight, na.rm = TRUE),
      SPECIES_MIN_HT = min(SPECIES_MIN_HT)
    ) |>
    ungroup() |>
    mutate(HEIGHT = if_else(is.infinite(HEIGHT), SPECIES_MIN_HT, HEIGHT)) |>
    filter(!is.na(HEIGHT) & !is.infinite(HEIGHT)) |>
    left_join(
      tblDWSPCFIPlotVisitsComplete |>
        filter(VisitCycle == 1970) |>
        select(MasterPlotID, STAND_CN = MasterPlotVisitID),
      by = join_by(MasterPlotID)
    ) |>
    left_join(
      species_crosswalk |> select(SPCD, FVS_SPCD),
      by = join_by(SpeciesCode == SPCD)
    ) |>
    filter(!is.na(FVS_SPCD)) |>
    select(
      STAND_CN,
      SPECIES = FVS_SPCD,
      DENSITY,
      HEIGHT
    )
})
