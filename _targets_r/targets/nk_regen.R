tar_target(nk_regen, {
  # Values in NK are seedlings per hectare; FVS needs seedlings per acre.
  # Convert using hectares_per_acre
  hectares_per_acre <- conv_unit(1, "acre", "hectare")
  
  # nk_table_4 has species in columns and management scenarios in rows.
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
})
