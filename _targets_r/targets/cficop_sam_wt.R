tar_target(cficop_sam_wt, {
  qryDWSPCFIPlotVisitTreeDetail |>
    cfi_with_visit_info(tblDWSPCFIPlotVisitsComplete) |>
    cfi_with_tree_info(tblDWSPCFITreesComplete) |>
    cfi_with_plot_info(tblDWSPCFIPlotsComplete) |>
    cfi_abp(cfiabp_trees) |>
    group_by(MasterPlotID) |>
    arrange(VisitCycle, MasterTreeID) |>
    filter(row_number() == 1) |>
    ungroup() |>
    left_join(
      tblSuppPlotForestrySubBasins,
      by = join_by(DWSPForestrySubWatershed == MasterForestryBasinID)
    ) |>
    group_by(EQDISTRICT, EQSUBDISTRICT) |>
    mutate(
      TotalAcres = max(DWSPOwnedAcres, na.rm = TRUE),
      NumPlots = n(),
      AcresPerPlot = TotalAcres / NumPlots,
      .groups = "keep"
    ) |>
    ungroup() |>
    select(
      EQDISTRICT,
      EQSUBDISTRICT,
      MasterPlotID,
      TotalAcres,
      NumPlots,
      AcresPerPlot
    )
})
