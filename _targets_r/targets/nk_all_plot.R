tar_target(nk_all_plot, {
  fia_conds(fiadb, nk_table_1_expanded) |>
      rename(FIA_INVYR=INVYR) |>
      right_join(
        nk_table_1_expanded |>
          select(STATECD, COUNTYCD, PLOT, INVYR) |>
          rename(NK_INVYR=INVYR),
        by=join_by(STATECD, COUNTYCD, PLOT)
      ) |>
      mutate(INVYR_MATCHES=ifelse(NK_INVYR==FIA_INVYR, 1, 0))
})
