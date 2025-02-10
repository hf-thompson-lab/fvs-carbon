nk_extract_regen <- function(nk_regen, mgmt_id) {
  nk_regen |>
    select(FVS_SPCD, {{mgmt_id}}) |>
    rename(SPECIES = FVS_SPCD, DENSITY = {{mgmt_id}}) |>
    filter(!is.na(DENSITY)) |>
    mutate(STAND_CN = NA, YEAR = NA)
}

nk_generate_plots_grown <- function(fiadb, nk_matching_plot) {
  fia_plots_filtered(fiadb, nk_matching_plot, \(.data, con) {
    .data |>
      filter_plots_forested(con) |> # Forested implies not skipped
      filter_plots_undisturbed(con) |>
      filter_plots_untreated(con)
  })
}
