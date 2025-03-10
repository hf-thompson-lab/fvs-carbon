tar_target(nrshrv_tree_harvested, {
  fia_trees_filtered(
      fiadb,
      nrshrv_plot_stats_all |> filter(HARVEST == 1),
      filter = \(.data, con) {
        .data |>
          filter(STATUSCD == 3) |>
          select(CN, PREV_TRE_CN, STATECD, COUNTYCD, PLOT, SUBP, TREE, INVYR, STATUSCD)
      }
    ) |>
    left_join(
      nrshrv_plot_stats_all |> select(STATECD, COUNTYCD, PLOT, INVYR, INVNUM),
      by = join_by(STATECD, COUNTYCD, PLOT, INVYR)
    )
})
