tar_target(nrs_estab_height, {
  min_tree_count <- 3
  max_tree_count <- 9
  
  ecosubcd_mixin <- nrs_plots_grown |>
    group_by(STATECD, COUNTYCD, PLOT) |>
    arrange(MEASYEAR) |>
    filter(row_number() == 1) |>
    ungroup() |>
    select(STATECD, COUNTYCD, PLOT, ECOSUBCD) |>
    mutate(ECOCD = substr(ECOSUBCD, 1, nchar(ECOSUBCD) - 1))
  
  trees_for_ht_estimation <- nrs_trees_spcd |>
    filter(2.5 < DIA & DIA < 6) |>
    group_by(STATECD, COUNTYCD, PLOT, SUBP, TREE) |>
    arrange(abs(DIA - 3)) |>
    filter(row_number() == 1) |>
    ungroup() |>
    # Overcome a data issue: some trees have no HT, but have PREV_HT
    mutate(HT = coalesce(HT, PREV_HT), DIA = coalesce(DIA, PREV_DIA)) |>
    filter(!is.na(HT) & !is.na(DIA)) |>
    left_join(ecosubcd_mixin, by = join_by(STATECD, COUNTYCD, PLOT))
  
  nrs_estab_height_plot <- trees_for_ht_estimation |>
    group_by(STATECD, COUNTYCD, PLOT, SPCD) |>
    arrange(abs(DIA - 3)) |>
    filter(row_number() <= max_tree_count) |>
    mutate(HT_PLOT = HT * 3 / DIA) |>
    filter(n() >= min_tree_count) |>
    summarize(HT_PLOT = mean(HT_PLOT), .groups = "keep") |>
    ungroup()
  
  nrs_estab_height_ecosubcd <- trees_for_ht_estimation |>
    group_by(ECOSUBCD, SPCD) |>
    arrange(abs(DIA - 3)) |>
    filter(row_number() <= max_tree_count) |>
    mutate(HT_ECOSUBCD = HT * 3 / DIA) |>
    filter(n() >= min_tree_count) |>
    summarize(HT_ECOSUBCD = mean(HT_ECOSUBCD), .groups = "keep") |>
    ungroup()
  
  nrs_estab_height_ecocd <- trees_for_ht_estimation |>
    group_by(ECOCD, SPCD) |>
    arrange(abs(DIA - 3)) |>
    filter(row_number() <= max_tree_count) |>
    mutate(HT_ECOCD = HT * 3 / DIA) |>
    filter(n() >= min_tree_count) |>
    summarize(HT_ECOCD = mean(HT_ECOCD), .groups = "keep") |>
    ungroup()
  
  nrs_estab_height_ne <- trees_for_ht_estimation |>
    group_by(SPCD) |>
    arrange(abs(DIA - 3)) |>
    filter(row_number() <= max_tree_count) |>
    mutate(HT_NE = HT * 3 / DIA) |>
    filter(n() >= min_tree_count) |>
    summarize(HT_NE = mean(HT_NE), .groups = "keep") |>
    ungroup()
  
  # height_min ignores the minimum number of trees,
  # and will produce values even for a single tree
  # anywhere in the region. It is a last-ditch catch-all.
  nrs_estab_height_min <- trees_for_ht_estimation |>
    group_by(SPCD) |>
    arrange(abs(DIA - 3)) |> # closest to 3" first
    filter(row_number() <= max_tree_count) |> 
    mutate(HT_MIN = HT * 3 / DIA) |>
    summarize(HT_MIN = mean(HT_MIN)) |>
    ungroup()
  
  nrs_trees_spcd |> # unfiltered tree growth
    semi_join(nrs_trees_ingrowth, by = join_by(STATECD, COUNTYCD, PLOT, SUBP, TREE)) |>
    distinct(STATECD, COUNTYCD, PLOT, SPCD) |>
    left_join(ecosubcd_mixin, by = join_by(STATECD, COUNTYCD, PLOT)) |>
    left_join(nrs_estab_height_plot, by = join_by(STATECD, COUNTYCD, PLOT, SPCD)) |>
    left_join(nrs_estab_height_ecosubcd, by = join_by(ECOSUBCD, SPCD)) |>
    left_join(nrs_estab_height_ecocd, by = join_by(ECOCD, SPCD)) |>
    left_join(nrs_estab_height_ne, by = join_by(SPCD)) |>
    left_join(nrs_estab_height_min, by = join_by(SPCD)) |>
    mutate(HT = coalesce(HT_PLOT, HT_ECOSUBCD, HT_ECOCD, HT_NE, HT_MIN))
})
