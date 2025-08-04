tar_target(cfigro_training_data, {
  tmp_top_species <- c("Acer rubrum", "Pinus strobus", "Quercus rubra")
  
  tmp_species_cmp <- fvs_species_composition(cfigro_none) |>
    filter(Year == 2020) |>
    mutate(SpeciesCode = as.numeric(SpeciesFIA)) |>
    # Average across random seeds
    group_by(StandID, Year, SpeciesCode) |>
    summarize(BA = mean(BA), TPA = mean(TPA), .groups = "drop") |>
    # Combine black oak and red oak
    mutate(
      SpeciesCode = if_else(SpeciesCode == 837, 833, SpeciesCode)
    ) |>
    group_by(StandID, Year, SpeciesCode) |>
    summarize(BA = sum(BA), TPA = sum(TPA), .groups = "drop") |>
    left_join(
      species_crosswalk |> select(SPCD, SCIENTIFIC_NAME),
      by = join_by(SpeciesCode == SPCD)
    ) |>
    mutate(
      TPH = conv_unit(TPA, "hectare", "acre"),
      BA = conv_multiunit(BA, "ft2 / acre", "m2 / hectare"),
    )
  
  tmp_by_plot <- tmp_species_cmp |>
    group_by(StandID) |>
    summarize(
      TPA = sum(TPA),
      BA = sum(BA),
      .groups = "drop"
    )
  
  # Top species by plot
  # (only considering top plots)
  tmp_plot_species <- tmp_species_cmp |>
    group_by(StandID, SCIENTIFIC_NAME) |>
    summarize(BA = sum(BA), .groups = "drop") |>
    ungroup() |>
    left_join(
      tmp_by_plot |>
        select(StandID, BA_Plot = BA),
      by = join_by(StandID)
    ) |>
    mutate(BA_Frac = BA / BA_Plot) |>
    filter(SCIENTIFIC_NAME %in% tmp_top_species) |>
    mutate(
      SCIENTIFIC_NAME = sub(" ", "_", SCIENTIFIC_NAME)
    ) |>
    pivot_wider(
      id_cols = StandID,
      names_from = SCIENTIFIC_NAME,
      values_from = BA_Frac,
      values_fill = 0
    )
  
  cfigro_none_proj_vs_meas |>
    filter(Year == 2020) |>
    select(StandID, BAI_Residual, Projected_BA, Projected_Tph) |>
    left_join(
      cfigro_plot |>
        filter(INV_YEAR == 1970) |>
        mutate(StandID = as.character(STAND_ID)) |>
        select(StandID, LAT = LATITUDE, ASPECT, SLOPE, ELEV = ELEVFT),
      by = join_by(StandID)
    ) |>
    left_join(
      tmp_plot_species,
      by = join_by(StandID)
    )
})
