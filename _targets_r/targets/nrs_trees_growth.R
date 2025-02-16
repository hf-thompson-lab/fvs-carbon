tar_target(nrs_trees_growth, {
  # FIA.TREE.CN - sequence number
  # FIA.TREE.TREE - TREE number
  # FIA.TREE.CONDID - condition class; 1 = live tree
  # FIA.TREE.SPCD - species code
  # FIA.TREE.DIA - current diameter
  # FIA.TREE.DIAHTCD - where diameter was taken, 1 = DBH
  # FIA.TREE.HT - height
  # FIA.TREE.SPGRPCD is broader than SPCD, group by that instead
  # FIA.TREE.CCLCD - Crown Class code
  # FIA.TREE.TPA_UNADJ - Trees Per Acre (Unadjusted)
  # We want ingrowth for all the plots in plot_grow_only
  
  prev_tre_mixin <- nrs_trees_history |>
    select(CN, MEASYEAR, DIA, HT) |>
    rename(
      PREV_TRE_CN = CN,
      PREV_MEASYEAR = MEASYEAR,
      PREV_DIA = DIA,
      PREV_HT = HT
    )
  
  nrs_trees_history |>
    # Self-join to previous tree to get growth increment
    left_join(prev_tre_mixin, by = join_by(PREV_TRE_CN)) |>
    mutate(
      DIA_DELTA = DIA - PREV_DIA,
      AGE_DELTA = MEASYEAR - PREV_MEASYEAR,
      ANN_DIA_DELTA = DIA_DELTA / AGE_DELTA,
      HT_DELTA = HT - PREV_HT,
      ANN_HT_DELTA = HT_DELTA / AGE_DELTA
    ) |>
    select(
      CN, PREV_TRE_CN,
      STATECD, COUNTYCD, PLOT, SUBP, TREE, SPCD, CCLCD,
      MEASYEAR, PREV_MEASYEAR, DIA, PREV_DIA, HT, PREV_HT,
      AGE_DELTA, DIA_DELTA, ANN_DIA_DELTA, HT_DELTA, ANN_HT_DELTA
    ) |>
    arrange(STATECD, COUNTYCD, PLOT, SUBP, TREE, MEASYEAR)
})
