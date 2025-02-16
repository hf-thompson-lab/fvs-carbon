tar_target(nrs_plots_grown, {
  # fvs_spcds: FIA SPCD if the species is supported by FVS
  fvs_spcds <- species_crosswalk |>
    filter(!is.na(FVS_SPCD)) |>
    select(SPCD)
  
  fia_plots_filtered(
    fiadb, filter = \(.data, con) {
      .data |>
        filter(INVYR >= 1999) |> # Do this first, it impacts later filters
        filter_plots_fvsne(con) |>
        filter_plots_modern(con) |>
        filter_plots_trees(con) |>
        filter_plots_long_measurement(con) |>
        filter_plots_forested(con) |>
        filter_plots_undisturbed(con) |>
        filter_plots_untreated(con) |>
        filter_plots_ba_frac(con, fvs_spcds, 0.9)
    })
})
