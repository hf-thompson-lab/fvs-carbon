tar_target(nrshrv_plot_stats, {
  nrshrv_plot_stats_all |>
    semi_join(
      nrshrv_plot,
      by = join_by(STATECD, COUNTYCD, PLOT, INVYR)
    ) |>
    # Renumber INVNUM due to removed inventories
    group_by(STATECD, COUNTYCD, PLOT) |>
    arrange(INVYR) |>
    mutate(INVNUM = row_number()) |>
    ungroup()
})
