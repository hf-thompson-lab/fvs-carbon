tar_target(nrsgro_estab_rate, {
  time_mixin <- nrsgro_plot_stats |>
    distinct(STATECD, COUNTYCD, PLOT, MEASYEAR) |>
    group_by(STATECD, COUNTYCD, PLOT) |>
    summarize(
      FIRST_YEAR = min(MEASYEAR),
      LAST_YEAR = max(MEASYEAR),
      .groups = "keep"
    ) |>
    ungroup()
  
  nrsgro_tree_spcd |>
    semi_join(nrsgro_tree_grm, by = join_by(STATECD, COUNTYCD, PLOT, SUBP, TREE, INVYR)) |>
    left_join(time_mixin, by = join_by(STATECD, COUNTYCD, PLOT)) |>
    group_by(STATECD, COUNTYCD, PLOT, SPCD, FIRST_YEAR, LAST_YEAR) |>
    summarize(
      COUNT = n(),
      # Don't use TPA_UNADJ; microplot multipliers are inaccurate here.
      # Reason: once a tree reaches 5" DBH, its peers on the rest of the subplot
      # are inventoried in the TREE table, so multiplying by the microplot
      # multiplier would replicate microplot trees across the subplots AND
      # count the trees on subplots, which is double-counting, which is wrong.
      COUNT_PER_ACRE = COUNT * 6,
      .groups = "keep"
    ) |>
    ungroup() |>
    left_join(species_crosswalk, by = join_by(SPCD)) |>
    group_by(JENKINS_SPGRPCD) |>
    mutate(
      NUM_PLOTS = n(),
      GROUP_NAME = paste0(JENKINS_SPGRP_NAME, ", n=", NUM_PLOTS)
    ) |>
    ungroup() |>
    mutate(
      COUNT_PER_HECTARE = conv_unit(COUNT_PER_ACRE, "hectare", "acre"),
      RATE = COUNT / (LAST_YEAR - FIRST_YEAR),
      RATE_PER_ACRE = COUNT_PER_ACRE / (LAST_YEAR - FIRST_YEAR),
      RATE_PER_HECTARE = COUNT_PER_HECTARE / (LAST_YEAR - FIRST_YEAR)
    )
})
