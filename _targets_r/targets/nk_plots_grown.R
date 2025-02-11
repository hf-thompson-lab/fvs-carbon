tar_target(nk_plots_grown, {
  fia_plots_filtered(fiadb, nk_plot_crosswalk, \(.data, con) {
    .data |>
      filter_plots_forested(con) |> # Forested implies not skipped
      filter_plots_undisturbed(con) |>
      filter_plots_untreated(con)
  }) |>
    # Bring in other plot IDs
    filter(INVYR >= 1999) |>
    left_join(nk_plot_crosswalk |> select(!MEASYEAR), by = join_by(STATECD, COUNTYCD, PLOT)) |>
    group_by(STATECD, COUNTYCD, PLOT) |>
    # Find the first and last measurement year for each stand
    summarize(
      FIRST_YEAR = min(MEASYEAR, na.rm = TRUE),
      LAST_YEAR = max(MEASYEAR, na.rm = TRUE),
      STAND_CN = max(STAND_CN, na.rm = TRUE),
      STAND_ID = max(FVS_STAND_ID, na.rm = TRUE),
      .groups = "keep"
    ) |>
    ungroup()
})
