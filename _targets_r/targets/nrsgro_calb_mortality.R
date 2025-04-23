tar_target(nrsgro_calb_mortality, {
  max_tree_id <- nrsgro_tree_history |>
    group_by(STATECD, COUNTYCD, PLOT, SUBP, INVYR) |>
    summarize(MAX_TREE = max(TREE, na.rm = TRUE), .groups = "keep") |>
    ungroup()
  
  prev_plot_cn <- nrsgro_plot |>
    group_by(STATECD, COUNTYCD, PLOT) |>
    arrange(INVYR) |>
    filter(row_number() == 1) |>
    ungroup() |>
    select(STATECD, COUNTYCD, PLOT, CN) |>
    rename(PREV_PLT_CN = CN)
  
  cond_mixin <- fia_conds(
    fiadb,
    nrsgro_tree_dead |>
      filter(INVNUM == 2) |>
      distinct(STATECD, COUNTYCD, PLOT, INVYR)
    ) |>
    select(
      STATECD, COUNTYCD, PLOT, CONDID, INVYR, SLOPE, ASPECT
    )
  
  prev_tree_mixin <- fia_trees_by_cn(
    fiadb,
    nrsgro_tree_dead |>
      filter(INVNUM == 2) |>
      select(PREV_TRE_CN) |>
      rename(CN = PREV_TRE_CN)
    ) |>
    select(CN, DIA, HT, TPA_UNADJ) |>
    rename(
      PREV_TRE_CN = CN,
      PREV_DIA = DIA,
      PREV_HT = HT,
      PREV_TPA_UNADJ = TPA_UNADJ
    )
  
  nrsgro_tree_dead |>
    filter(INVNUM == 2) |>
    left_join(
      max_tree_id,
      by = join_by(STATECD, COUNTYCD, PLOT, SUBP, INVYR)
    ) |>
    group_by(STATECD, COUNTYCD, PLOT, SUBP) |>
    arrange(TREE) |>
    mutate(
      TREE_ID = MAX_TREE + row_number()
    ) |>
    ungroup() |>
    left_join(prev_tree_mixin, by = join_by(PREV_TRE_CN)) |>
    left_join(prev_plot_cn, by = join_by(STATECD, COUNTYCD, PLOT)) |>
    left_join(cond_mixin, by = join_by(STATECD, COUNTYCD, PLOT, CONDID, INVYR)) |>
    filter_add_stand_id() |>
    mutate(
      STAND_CN = PREV_PLT_CN,
      STAND_ID = STAND_ID,
      PLOT_CN = PREV_PLT_CN,
      PLOT_ID = SUBP,
      STANDPLOT_CN = paste0(STAND_CN, "_", SUBP),
      STANDPLOT_ID = paste0(STAND_ID, "_", SUBP),
      TREE_CN = CN,
      TREE_ID = TREE_ID,
      TREE_COUNT = coalesce(TPA_UNADJ, PREV_TPA_UNADJ),
      SPECIES = sprintf("%03d", SPCD),
      HISTORY = if_else(PREV_STATUS_CD == 1, "6", "8"),
      DIAMETER = coalesce(DIA, PREV_DIA),
      HT = coalesce(HT, PREV_HT),
      SLOPE = SLOPE,
      ASPECT = ASPECT
    ) |>
    select(
      STAND_CN,
      STAND_ID,
      PLOT_CN,
      PLOT_ID,
      STANDPLOT_CN,
      STANDPLOT_ID,
      TREE_CN,
      TREE_ID,
      TREE_COUNT,
      SPECIES,
      HISTORY,
      DIAMETER,
      HT,
      SLOPE,
      ASPECT
    )
  
})
