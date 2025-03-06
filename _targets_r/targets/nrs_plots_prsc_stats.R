tar_target(nrs_plots_prsc_stats, {
  nrs_plots_prsc_stats_all |>
    semi_join(
      nrs_plots_prsc,
      by = join_by(STATECD, COUNTYCD, PLOT, INVYR)
    ) |>
    # Renumber INVNUM due to removed inventories
    group_by(STATECD, COUNTYCD, PLOT) |>
    arrange(INVYR) |>
    mutate(INVNUM = row_number()) |>
    ungroup()
})
