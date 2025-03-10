tar_target(nrsgro_tree_grm, {
  multi_ingrowth <- nrsgro_tree_grm_all |>
    group_by(STATECD, COUNTYCD, PLOT, SUBP, TREE) |>
    arrange(INVYR) |>
    filter(
      n() > 1,
      row_number() > 1
    ) |>
    select(CN)
  nrsgro_tree_grm_all |>
    anti_join(multi_ingrowth, by = join_by(CN))
})
