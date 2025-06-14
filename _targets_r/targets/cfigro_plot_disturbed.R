tar_target(cfigro_plot_disturbed, {
  qryDWSPCFIPlotVisitTreeDetail |>
    cfi_with_visit_info(tblDWSPCFIPlotVisitsComplete) |>
    cfi_with_tree_info(tblDWSPCFITreesComplete) |>
    cfi_abp(cfiabp_trees) |>
    cfi_disturbed(tblDWSPCFIPlotVisitDisturbances)
})
