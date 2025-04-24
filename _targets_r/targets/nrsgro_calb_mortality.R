tar_target(nrsgro_calb_mortality, {
  tmp_prev_plot <- nrsgro_plot |>
    group_by(STATECD, COUNTYCD, PLOT) |>
    arrange(INVYR) |>
    filter(row_number() == 1) |>
    ungroup() |>
    select(STATECD, COUNTYCD, PLOT, INVYR, CN) |>
    rename(PREV_PLT_CN = CN)
  
  tmp_max_tree_id <- fia_trees_filtered(fiadb, tmp_prev_plot, \(.data, con) {
    .data |>
      group_by(STATECD, COUNTYCD, PLOT, SUBP, INVYR) |>
      summarize(MAX_TREE = max(TREE, na.rm = TRUE), .groups = "keep") |>
      ungroup() |>
      select(-INVYR)
  })
  
  tmp_next_plot <- nrsgro_plot |>
    group_by(STATECD, COUNTYCD, PLOT) |>
    arrange(INVYR) |>
    filter(row_number() == 2) |>
    ungroup() |>
    select(STATECD, COUNTYCD, PLOT, INVYR)
  
  tmp_trees_dead <- fia_trees_filtered(fiadb, tmp_next_plot, \(.data, con) {
      .data |>
        filter(STATUSCD == 2) |> # dead trees
        select(
          CN, PREV_TRE_CN,
          STATECD, COUNTYCD, PLOT, CONDID, SUBP, TREE, INVYR,
          SPCD, STATUSCD, DIA, HT, TPA_UNADJ
        )
    })
  
  tmp_cond_mixin <- fia_conds(
      fiadb,
      tmp_trees_dead |>
        distinct(STATECD, COUNTYCD, PLOT, INVYR)
    ) |>
    select(
      STATECD, COUNTYCD, PLOT, CONDID, INVYR, SLOPE, ASPECT
    )
  
  tmp_prev_tree <- fia_trees_by_cn(
      fiadb,
      tmp_trees_dead |>
        select(PREV_TRE_CN) |>
        rename(CN = PREV_TRE_CN)
    ) |>
    select(CN, STATUSCD, DIA, HT, TPA_UNADJ) |>
    rename(
      PREV_STATUS_CD = STATUSCD,
      PREV_TRE_CN = CN,
      PREV_DIA = DIA,
      PREV_HT = HT,
      PREV_TPA_UNADJ = TPA_UNADJ
    )
  
  nrsgro_calb_mortality <- tmp_trees_dead |>
    left_join(
      tmp_max_tree_id,
      by = join_by(STATECD, COUNTYCD, PLOT, SUBP)
    ) |>
    group_by(STATECD, COUNTYCD, PLOT, SUBP) |>
    arrange(TREE) |>
    mutate(
      TREE_ID = MAX_TREE + row_number()
    ) |>
    ungroup() |>
    left_join(tmp_prev_tree, by = join_by(PREV_TRE_CN)) |>
    left_join(tmp_prev_plot |> select(-INVYR), by = join_by(STATECD, COUNTYCD, PLOT)) |>
    left_join(tmp_cond_mixin, by = join_by(STATECD, COUNTYCD, PLOT, CONDID, INVYR)) |>
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
      HISTORY = if_else(PREV_STATUS_CD == 1, 6, 8),
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
