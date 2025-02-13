tar_target(nrs_grm_ingrowth, {
  fia_trees_filtered(fiadb, nrs_plots_grown, \(.data, con) {
    .data |>
      filter_trees_ingrowth(con) |>
      # Trees can be marked as ingrown multiple times;
      # Pick only the first ingrowth record for each tree
      group_by(STATECD, COUNTYCD, PLOT, SUBP, TREE) |>
      filter(INVYR == min(INVYR, na.rm = TRUE)) |>
      ungroup()
  })
})
