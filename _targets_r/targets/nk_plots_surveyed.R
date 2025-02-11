tar_target(nk_plots_surveyed, {
  fia_plots(fiadb, nk_plots_grown) |>
    filter(INVYR >= 1999) |>
    left_join(nk_plot_crosswalk |> select(!MEASYEAR), by = join_by(STATECD, COUNTYCD, PLOT)) |>
    mutate(
      STAND_CN = CN,
      STAND_ID = FVS_STAND_ID,
      FIRST_YEAR = MEASYEAR,
      LAST_YEAR = MEASYEAR
    ) |>
    select(STATECD, COUNTYCD, PLOT, FIRST_YEAR, LAST_YEAR, STAND_ID, STAND_CN)
})
