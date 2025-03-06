tar_target(nrs_plots_prsc, {
  nrs_plots_prsc_all |>
    semi_join(
      nrs_trees_prsc_removed |> distinct(STATECD, COUNTYCD, PLOT),
      by = join_by(STATECD, COUNTYCD, PLOT)
    ) |>
    # Remove inventories prior to the pre-harvest inventory
    semi_join(
      nrs_plots_prsc_stats_all |> filter(PRE_PRE_HARVEST == FALSE),
      by = join_by(STATECD, COUNTYCD, PLOT, INVYR)
    )
})
