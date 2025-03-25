tar_target(nrshrv_tree_removed, {
  tree_in_pre_harvest_plots <- fia_trees_filtered(
      fiadb,
      nrshrv_plot_stats_all |> filter(PRE_HARVEST == 1),
      filter = \(.data, con) {
        .data |>
          filter(STATUSCD == 1 | STATUSCD == 2) |> # live or dead trees
          select(CN, STATECD, COUNTYCD, PLOT, SUBP, TREE, INVYR, MORTYR, STATUSCD)  
    }) |>
    left_join(
      nrshrv_plot_stats_all |> select(STATECD, COUNTYCD, PLOT, INVYR, INVNUM),
      by = join_by(STATECD, COUNTYCD, PLOT, INVYR)
    )
  
  tree_in_harvested_plots <- fia_trees_filtered(
      fiadb,
      nrshrv_plot_stats_all |> filter(HARVEST == 1),
      filter = \(.data, con) {
        .data |>
          filter(STATUSCD == 1 | STATUSCD == 2) |> # live and dead trees are still there
          select(STATECD, COUNTYCD, PLOT, SUBP, TREE, INVYR)
      }
    ) |>
    left_join(
      nrshrv_plot_stats_all |> select(STATECD, COUNTYCD, PLOT, INVYR, INVNUM),
      by = join_by(STATECD, COUNTYCD, PLOT, INVYR)
    ) |>
    # Move back a timestep so we can join the future onto the past
    mutate(
      INVNUM = INVNUM - 1
    )
  
  tree_in_pre_harvest_plots |>
    anti_join(
      tree_in_harvested_plots,
      by = join_by(STATECD, COUNTYCD, PLOT, SUBP, TREE, INVNUM)
    )
})
