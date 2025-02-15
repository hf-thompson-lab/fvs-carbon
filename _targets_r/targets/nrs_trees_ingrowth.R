tar_target(nrs_trees_ingrowth, {
  multi_ingrowth <- nrs_trees_ingrowth_all |>
    group_by(STATECD, COUNTYCD, PLOT, SUBP, TREE) |>
    arrange(INVYR) |>
    filter(
      n() > 1,
      row_number() > 1
    ) |>
    select(CN)
  nrs_trees_ingrowth_all |>
    anti_join(multi_ingrowth, by = join_by(CN))
})
