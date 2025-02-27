tar_target(nrs_plots_prsc, {
  nrs_plots_prsc_all |>
    semi_join(
      nrs_trees_prsc_removed |> distinct(STATECD, COUNTYCD, PLOT),
      by = join_by(STATECD, COUNTYCD, PLOT)
    )
})
