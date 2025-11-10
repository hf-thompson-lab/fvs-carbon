tar_target(cficop_hvst_tpa, {
  qryDWSPCFIPlotVisitTreeDetail |>
    cfi_with_visit_info(tblDWSPCFIPlotVisitsComplete) |>
    cfi_with_tree_info(tblDWSPCFITreesComplete) |>
    cfi_with_plot_info(tblDWSPCFIPlotsComplete) |>
    cfi_abp(cfiabp_trees) |>
    mutate(
      YearCut = VisitCycle - YearsSinceLastCut
    ) |>
    filter(
      dbh_prior >= 15.24 # TPA below 6" can't really be trusted
    ) |>
    # Replace na's in status to keep them from propagating
    replace_na(list(StatusB = "X", Status6 = "X")) |>
    group_by(MasterPlotID, YearCut, SpeciesCode) |>
    summarize(
      tpa_harvested = sum(if_else(StatusB == "C" | Status6 == "B", 1, 0)) * 5,
      tpa_remaining = sum(if_else(StatusB == "L" | Status6 == "L", 1, 0)) * 5,
      .groups = "keep"
    ) |>
    ungroup() |>
    filter(tpa_harvested > 0) |>
    left_join(
      cfigro_plot |> select(STAND_ID, STAND_CN),
      by = join_by(MasterPlotID == STAND_ID)
    ) |>
    left_join(
      species_crosswalk |> select(SPCD, FVS_SPCD),
      by = join_by(SpeciesCode == SPCD)
    ) |>
  
  # For prescription-based harvest, our schema is:
  # STAND_CN, TREE_CN, PREV_TRE_CN, YEAR, PRESCRIPTION
  # For DBH-based harvest, our schema is:
  # STAND_CN, YEAR, DBH_MIN, DBH_MAX, SPCD, TPA, BA
  # For Q-Factor harvest, our schema is:
  # STAND_CN, YEAR, DBH_MIN, DBH_MAX, SPCD, QFA, RESIDUAL
    mutate(
      YEAR = YearCut,
      DBH_MIN = 6,
      DBH_MAX = 99,
      SPCD = FVS_SPCD,
      QFA = 1.4,
      TPA = tpa_remaining
    ) |>
    select(STAND_CN, YEAR, DBH_MIN, DBH_MAX, SPCD, QFA, TPA)
})
