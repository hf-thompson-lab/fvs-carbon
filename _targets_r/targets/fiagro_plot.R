tar_target(fiagro_plot, {
  fia_plots_filtered(
    fiadb, filter = \(.data, con) {
      .data |>
        filter(INVYR >= 1999) |> # Do this first, it impacts later filters
        filter(STATECD %in% fiagro_states$STATECD) |>
        filter_plots_modern(con) |>
        filter_plots_trees(con) |>
        filter_plots_long_measurement(con) |>
        filter_plots_forested(con) |>
        filter_plots_undisturbed(con) |>
        filter_plots_untreated(con)
    }) |>
    # Filter down to just the first and last inventory
    group_by(STATECD, COUNTYCD, PLOT) |>
    filter(INVYR == min(INVYR) | INVYR == max(INVYR)) |>
    ungroup()
})
