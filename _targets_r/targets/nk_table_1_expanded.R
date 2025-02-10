tar_target(nk_table_1_expanded, {
  nk_table_1 |>
    mutate(STATECD = as.integer(substr(`FIA plot code`, 1, 2))) |>
    mutate(INVYR = as.integer(substr(`FIA plot code`, 3, 6))) |>
    mutate(UNITCD = as.integer(substr(`FIA plot code`, 7, 8))) |>
    mutate(COUNTYCD = as.integer(substr(`FIA plot code`, 9, 11))) |>
    mutate(PLOT = as.integer(substr(`FIA plot code`, 12, 16)))
})
