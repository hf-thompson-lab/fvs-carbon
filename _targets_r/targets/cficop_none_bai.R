tar_target(cficop_none_bai, {
  tmp_top_species <- c("Acer rubrum", "Pinus strobus", "Quercus rubra")
  
  tmp_species_cmp <- fvs_species_composition(cficop_none) |>
    filter(Year == 2020) |>
    mutate(SpeciesCode = as.numeric(SpeciesFIA)) |>
    # Combine black oak and red oak
    mutate(
      SpeciesCode = if_else(SpeciesCode == 837, 833, SpeciesCode)
    ) |>
    group_by(StandID, Year, SpeciesCode, random_seed) |>
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
    group_by(StandID, random_seed) |>
    summarize(
      TPA = sum(TPA),
      BA = sum(BA),
      .groups = "drop"
    )
  
  # Top species by plot
  # (only considering tmp_top_species)
  tmp_plot_species <- tmp_species_cmp |>
    group_by(StandID, SCIENTIFIC_NAME, random_seed) |>
    summarize(BA = sum(BA), .groups = "drop") |>
    ungroup() |>
    left_join(
      tmp_by_plot |>
        select(StandID, random_seed, BA_Plot = BA),
      by = join_by(StandID, random_seed)
    ) |>
    mutate(BA_Frac = BA / BA_Plot) |>
    filter(SCIENTIFIC_NAME %in% tmp_top_species) |>
    mutate(
      SCIENTIFIC_NAME = sub(" ", "_", SCIENTIFIC_NAME)
    ) |>
    pivot_wider(
      id_cols = c("StandID", "random_seed"),
      names_from = SCIENTIFIC_NAME,
      values_from = BA_Frac,
      values_fill = 0
    )
  
  # cficop_none_summary aggregates over random_seed; we need random_seed
  # in the output.
  tmp_none_summary <- fvs_read_output(cficop_none, "FVS_Summary2_East")
  
  tmp_plot_starting_ba <- tmp_none_summary |>
    filter(Year == 1970) |>
    mutate(
      Starting_BA = conv_multiunit(BA, "ft2 / acre", "m2 / hectare"),
      Starting_Tph = conv_unit(Tpa, "hectare", "acre")
    ) |>
    select(StandID, random_seed, Starting_BA, Starting_Tph)
  
  tmp_plot_projected_ba <- tmp_none_summary |>
    filter(Year == 2020) |>
    mutate(
      Projected_BA = conv_multiunit(BA, "ft2 / acre", "m2 / hectare"),
      Projected_Tph = conv_unit(Tpa, "hectare", "acre")
    ) |>
    select(StandID, random_seed, Projected_BA, Projected_Tph)
  
  # cficop_none_carbon aggregates over random_seed; we need random_seed
  # in the output.
  tmp_none_carbon <- fvs_read_output(cficop_none, "FVS_Carbon")
  
  tmp_plot_starting_carbon <- tmp_none_carbon |>
    filter(Year == 1970) |>
    rename(Starting_Carbon = Aboveground_Total_Live) |>
    select(StandID, random_seed, Starting_Carbon)
  
  tmp_plot_projected_carbon <- tmp_none_carbon |>
    filter(Year == 2020) |>
    rename(Projected_Carbon = Aboveground_Total_Live) |>
    select(StandID, random_seed, Projected_Carbon)
  
  cficop_none_bai <- tmp_plot_species |>
    left_join(
      cfigro_plot |>
        filter(INV_YEAR == 1970) |>
        mutate(StandID = as.character(STAND_ID)) |>
        select(StandID, LAT = LATITUDE, ASPECT, SLOPE, ELEV = ELEVFT),
      by = join_by(StandID)
    ) |>
    left_join(
      tmp_plot_starting_ba,
      by = join_by(StandID, random_seed)
    ) |>
    left_join(
      tmp_plot_projected_ba,
      by = join_by(StandID, random_seed)
    ) |>
    left_join(
      tmp_plot_starting_carbon,
      by = join_by(StandID, random_seed)
    ) |>
    left_join(
      tmp_plot_projected_carbon,
      by = join_by(StandID, random_seed)
    ) |>
    mutate(
      BAI_Error = cfigro_bai_model$predict_newdata(pick(everything()))$response,
      Carbon_Flux_Error = cfigro_aglcf_model$predict_newdata(pick(everything()))$response
    ) |>
    mutate(
      Projected_BAI = (Projected_BA - Starting_BA) / (2020 - 1970),
      Adjusted_BAI = Projected_BAI - BAI_Error,
      Projected_AGLCF = (Projected_Carbon - Starting_Carbon) / (2020 - 1970),
      Adjusted_AGLCF = Projected_AGLCF - Carbon_Flux_Error
    )
})
