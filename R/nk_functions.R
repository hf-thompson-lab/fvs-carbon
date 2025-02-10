nk_generate_regen <- function(nk_table_4, species_crosswalk) {
  # Values in NK are seedlings per hectare; FVS needs seedlings per acre.
  # Convert using hectares_per_acre
  hectares_per_acre <- conv_unit(1, "acre", "hectare")

  # table4 has species in columns and management scenarios in rows.
  # We wish to pivot to have species in rows and management scenarios in columns.
  nk_table_4 |>
    pivot_longer(cols = !`Management scenario`) |>
    pivot_wider(names_from = `Management scenario`) |>
    rename(SCIENTIFIC_NAME = name) |>
    left_join(species_crosswalk, by = join_by(SCIENTIFIC_NAME)) |>
    mutate(Clearcut = round(Clearcut * hectares_per_acre)) |>
    mutate(Shelterwood = round(Shelterwood * hectares_per_acre)) |>
    mutate(`ITS_Low Retention` = round(`ITS_Low Retention` * hectares_per_acre)) |>
    mutate(`ITS_High Retention` = round(`ITS_High Retention` * hectares_per_acre)) |>
    mutate(Background = round(Background * hectares_per_acre))
}

nk_extract_regen <- function(nk_regen, mgmt_id) {
  nk_regen |>
    select(FVS_SPCD, {{mgmt_id}}) |>
    rename(SPECIES = FVS_SPCD, DENSITY = {{mgmt_id}}) |>
    filter(!is.na(DENSITY)) |>
    mutate(STAND_CN = NA, YEAR = NA)
}

nk_project_grow <- function(
    fiadb,
    stand_type,
    nk_to_fia,
    nk_regen
) {
  fvsbin_dir <- "/fvs/fvsbin"
  fvs_variant <- "fvsne"
  data_dir <- "data/fvs"
  title <- paste0("NKBy", stringr::str_to_title(stand_type))
  mgmt_id <- "NONE"
  
  project_dir <- file.path(data_dir, paste0("FVS_", title, "_", mgmt_id))
  if (!dir.exists(project_dir)) {
    dir.create(project_dir)
  }

  nk_background_regen <- nk_extract_regen(nk_regen, "Background")

  nk_grow_only_stands <- nk_to_fia |>
    rename(
      STAND_ID = `FIA plot code`,
      FIRST_YEAR = MEASYEAR
    ) |>
    mutate(LAST_YEAR = 2165) |>
    select(STAND_ID, STAND_CN, FIRST_YEAR, LAST_YEAR)

  fvs_run(
    fvsbin_dir,
    fvs_variant,
    project_dir,
    fiadb,
    stand_type,
    title,
    mgmt_id,
    nk_grow_only_stands,
    nk_background_regen
  )
}

nk_generate_plots_grown <- function(fiadb, nk_matching_plot) {
  fia_plots_filtered(fiadb, nk_matching_plot, \(.data, con) {
    .data |>
      filter_plots_forested(con) |> # Forested implies not skipped
      filter_plots_undisturbed(con) |>
      filter_plots_untreated(con)
  })
}
