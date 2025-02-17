tar_target(nrs_trees_spcd, {
  # mcs == Most Common Species
  mcs_plot <- nrs_trees_growth |>
    left_join(species_crosswalk, by = join_by(SPCD)) |>
    filter(!is.na(FVS_SPCD)) |>
    group_by(STATECD, COUNTYCD, PLOT, GENUS, SPCD) |>
    summarize(COUNT = n(), .groups = "keep") |>
    ungroup() |>
    group_by(STATECD, COUNTYCD, PLOT, GENUS) |>
    filter(COUNT == max(COUNT)) |>
    filter(SPCD == min(SPCD)) |> # break ties by SPCD
    ungroup() |>
    select(!COUNT) |>
    rename(SPCD_PLOT = SPCD)
  
  mcs_county <- nrs_trees_growth |>
    left_join(species_crosswalk, by = join_by(SPCD)) |>
    filter(!is.na(FVS_SPCD)) |>
    group_by(STATECD, COUNTYCD, GENUS, SPCD) |>
    summarize(COUNT = n(), .groups = "keep") |>
    ungroup() |>
    group_by(STATECD, COUNTYCD, GENUS) |>
    filter(COUNT == max(COUNT)) |>
    filter(SPCD == min(SPCD)) |> # break ties by SPCD
    ungroup() |>
    select(!COUNT) |>
    rename(SPCD_COUNTY = SPCD)
  
  mcs_state <- nrs_trees_growth |>
    left_join(species_crosswalk, by = join_by(SPCD)) |>
    filter(!is.na(FVS_SPCD)) |>
    group_by(STATECD, GENUS, SPCD) |>
    summarize(COUNT = n(), .groups = "keep") |>
    ungroup() |>
    group_by(STATECD, GENUS) |>
    filter(COUNT == max(COUNT)) |>
    filter(SPCD == min(SPCD)) |> # break ties by SPCD
    ungroup() |>
    select(!COUNT) |>
    rename(SPCD_STATE = SPCD)
  
  mcs_overall <- nrs_trees_growth |>
    left_join(species_crosswalk, by = join_by(SPCD)) |>
    filter(!is.na(FVS_SPCD)) |>
    group_by(GENUS, SPCD) |>
    summarize(COUNT = n(), .groups = "keep") |>
    ungroup() |>
    group_by(GENUS) |>
    filter(COUNT == max(COUNT)) |>
    filter(SPCD == min(SPCD)) |> # break ties by SPCD
    ungroup() |>
    select(!COUNT) |>
    rename(SPCD_OVERALL = SPCD)
  
  # Note that this leaves Crataegus (Hawthorn) as spp. - apparently FIA has
  # passed on trying to identify species in Crataegus.
  nrs_trees_growth |>
    left_join(species_crosswalk |> select(SPCD, GENUS), by = join_by(SPCD)) |>
    left_join(mcs_plot, by = join_by(STATECD, COUNTYCD, PLOT, GENUS)) |>
    left_join(mcs_county, by = join_by(STATECD, COUNTYCD, GENUS)) |>
    left_join(mcs_state, by = join_by(STATECD, GENUS)) |>
    left_join(mcs_overall, by = join_by(GENUS)) |>
    rename(SPCD_ORIG = SPCD) |>
    mutate(SPCD = coalesce(SPCD_PLOT, SPCD_COUNTY, SPCD_STATE, SPCD_OVERALL, SPCD_ORIG)) |>
    select(SPCD | !any_of(names(species_crosswalk))) |>
    left_join(species_crosswalk, by = join_by(SPCD))
})
