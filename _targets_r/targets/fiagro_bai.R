tar_target(fiagro_bai, {
  prev_plot_tmp <- fiagro_plot |>
    group_by(STATECD, COUNTYCD, PLOT) |>
    filter(INVYR == min(INVYR)) |>
    ungroup() |>
    left_join(
      fiagro_plot_stats |> select(STATECD, COUNTYCD, PLOT, INVYR, BALIVE),
      by = join_by(STATECD, COUNTYCD, PLOT, INVYR)
    ) |>
    select(STATECD, COUNTYCD, PLOT, LAT, LON, ELEV, REMPER, MEASYEAR, BALIVE) |>
    rename(
      PREV_MEASYEAR = MEASYEAR,
      PREV_BALIVE = BALIVE
    )
  
  fiagro_plot_stats |>
    group_by(STATECD, COUNTYCD, PLOT) |>
    filter(INVYR == max(INVYR)) |>
    ungroup() |>
    left_join(prev_plot_tmp, by = join_by(STATECD, COUNTYCD, PLOT)) |>
    mutate(
      BALIVE = conv_multiunit(BALIVE, "ft2 / acre", "m2 / hectare"),
      PREV_BALIVE = conv_multiunit(PREV_BALIVE, "ft2 / acre", "m2 / hectare"),
      BA_DELTA = BALIVE - PREV_BALIVE,
      BAI = BA_DELTA / (MEASYEAR - PREV_MEASYEAR)
    )  
})
