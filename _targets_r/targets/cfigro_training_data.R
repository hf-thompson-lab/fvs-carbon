tar_target(cfigro_training_data, {
  tmp_top_species <- c("Acer rubrum", "Pinus strobus", "Quercus rubra")
  
  tmp_trees <- qryDWSPCFIPlotVisitTreeDetail |>
    cfi_with_visit_info(tblDWSPCFIPlotVisitsComplete) |>
    cfi_with_tree_info(tblDWSPCFITreesComplete) |>
    cfi_with_plot_info(tblDWSPCFIPlotsComplete) |>
    cfi_abp(cfiabp_trees) |>
    filter(VisitCycle == 1970) |>
    filter(cfi_status_live(VisitTreeStatusCode)) |> # Only Live Trees
    left_join(
      species_crosswalk |> select(SPCD, SCIENTIFIC_NAME),
      by = join_by(SpeciesCode == SPCD)
    ) |>
    mutate(
      TPA = conv_unit(5, "hectare", "acre"),
      BA = TPA * pi * conv_unit(VisitTreeDIAM / 2, "in", "m")^2,
    )
  
  tmp_by_plot <- tmp_trees |>
    group_by(MasterPlotID) |>
    summarize(
      TPA = sum(TPA, na.rm = TRUE),
      BA = sum(BA, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Top species by plot
  # (only considering top plots)
  tmp_plot_species <- tmp_trees |>
    group_by(MasterPlotID, SCIENTIFIC_NAME) |>
    summarize(BA = sum(BA, na.rm = TRUE), .groups = "keep") |>
    ungroup() |>
    left_join(
      tmp_by_plot |>
        select(MasterPlotID, BA_Plot = BA),
      by = join_by(MasterPlotID)
    ) |>
    mutate(BA_Frac = BA / BA_Plot) |>
    filter(SCIENTIFIC_NAME %in% tmp_top_species) |>
    mutate(
      SCIENTIFIC_NAME = sub(" ", "_", SCIENTIFIC_NAME)
    ) |>
    pivot_wider(
      id_cols = MasterPlotID,
      names_from = SCIENTIFIC_NAME,
      values_from = BA_Frac,
      values_fill = 0
    ) |>
    mutate(MasterPlotID = as.character(MasterPlotID))
  
  cfigro_none_proj_vs_meas |>
    filter(Year == 2020) |>
    select(MasterPlotID = StandID, BAI_Residual, Projected_BA, Projected_Tph) |>
    left_join(
      cfigro_plot |>
        filter(INV_YEAR == 1970) |>
        mutate(MasterPlotID = as.character(STAND_ID)) |>
        select(MasterPlotID, LAT = LATITUDE, ASPECT, SLOPE, ELEV = ELEVFT),
      by = join_by(MasterPlotID)
    ) |>
    left_join(
      tmp_plot_species,
      by = join_by(MasterPlotID)
    )
})
