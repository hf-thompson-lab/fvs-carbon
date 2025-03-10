tar_target(nrshrv_plot_all, {
  fia_plots_filtered(fiadb, filter = \(.data, con) {
    .data |>
      filter(INVYR >= 1999) |>
      filter_plots_fvsne(con) |>
      filter_plots_trees(con) |>
      filter_plots_undisturbed(con) |>
      filter_plots_unfertilized(con) |>
      filter_plots_measured_pre_post_harvest(con) |>
      filter_plots_single_harvest(con)
  })
})
