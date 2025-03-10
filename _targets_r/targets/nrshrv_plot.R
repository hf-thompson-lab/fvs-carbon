tar_target(nrshrv_plot, {
  nrshrv_plot_all |>
    semi_join(
      nrshrv_tree_removed |> distinct(STATECD, COUNTYCD, PLOT),
      by = join_by(STATECD, COUNTYCD, PLOT)
    ) |>
    # Remove inventories prior to the pre-harvest inventory
    semi_join(
      nrshrv_plot_stats_all |> filter(PRE_PRE_HARVEST == FALSE),
      by = join_by(STATECD, COUNTYCD, PLOT, INVYR)
    )
})
