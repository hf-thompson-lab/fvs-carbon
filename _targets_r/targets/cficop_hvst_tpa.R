tar_target(cficop_hvst_tpa, {
  qryDWSPCFIPlotVisitTreeDetail |>
    cfi_with_visit_info(tblDWSPCFIPlotVisitsComplete) |>
    cfi_with_tree_info(tblDWSPCFITreesComplete) |>
    cfi_with_plot_info(tblDWSPCFIPlotsComplete) |>
    cfi_abp(cfiabp_trees) |>
    mutate(
      YearCut = VisitCycle - YearsSinceLastCut,
      diameter_class = as.integer(conv_unit(dbh_prior, "cm", "in") / 2) * 2
    ) |>
    filter(
      diameter_class >= 6 # TPA below 6" can't really be trusted
    ) |>
    # Replace na's in status to keep them from propagating
    replace_na(list(StatusB = "X", Status6 = "X")) |>
    group_by(MasterPlotVisitID, YearCut, SpeciesCode, diameter_class) |>
    summarize(
      tpa_harvested = sum(if_else(StatusB == "C" | Status6 == "B", 1, 0)) * 5,
      tpa_remaining = sum(if_else(StatusB == "L" | Status6 == "L", 1, 0)) * 5,
      .groups = "keep"
    ) |>
    ungroup() |>
    filter(tpa_harvested > 0) |>
  # For prescription-based harvest, our schema is:
  # STAND_CN, TREE_CN, PREV_TRE_CN, YEAR, PRESCRIPTION
  # For DBH-based harvest, our schema is:
  # STAND_CN, YEAR, DBH_MIN, DBH_MAX, SPCD, TPA
    mutate(
      STAND_CN = MasterPlotVisitID,
      YEAR = YearCut,
      DBH_MIN = diameter_class,
      DBH_MAX = diameter_class + 2,
      SPCD = SpeciesCode,
      TPA = tpa_remaining
    ) |>
    select(STAND_CN, YEAR, DBH_MIN, DBH_MAX, SPCD, TPA)
})
