tar_target(nrsgro_ca10_regen, {
  # fvs_run wants establishment in the form:
  # STAND_CN
  # SPECIES (FVS_SPCD)
  # DENSITY (TPA)
  # HEIGHT (FT)
  tmp_plot_cn <- nrsgro_plot |>
    group_by(STATECD, COUNTYCD, PLOT) |>
    arrange(INVYR) |>
    filter(row_number() == 1) |>
    ungroup() |>
    filter_add_stand_id() |>
    select(STAND_ID, CN) |>
    rename(STAND_CN = CN)
  
  nrsgro_cal10_regen <- nrsgro_estab_rate |>
    filter_add_stand_id() |>
    left_join(
      tmp_plot_cn |> select(STAND_ID, STAND_CN),
      by = join_by(STAND_ID)
    ) |>
    select(STAND_CN, STATECD, COUNTYCD, PLOT, SPCD, RATE_PER_ACRE) |>
    mutate(RATE_PER_ACRE = floor(RATE_PER_ACRE * 10)) |> # 10-year timesteps
    rename(DENSITY = RATE_PER_ACRE) |>
    left_join(
      nrsgro_estab_height |>
        select(STATECD, COUNTYCD, PLOT, SPCD, HT),
      by = join_by(STATECD, COUNTYCD, PLOT, SPCD)
    ) |>
    mutate(HT = floor(HT)) |>
    rename(HEIGHT = HT) |>
    left_join(
      species_crosswalk |>
        select(SPCD, FVS_SPCD),
      by = join_by(SPCD)
    ) |>
    rename(SPECIES = FVS_SPCD) |>
    filter(!is.na(SPECIES) & !is.na(DENSITY) & !is.na(HEIGHT)) |>
    select(STAND_CN, SPECIES, DENSITY, HEIGHT)
})
