tar_target(cficop_hvst_tpa, {
  tmp_harvested <- qryDWSPCFIPlotVisitTreeDetail |>
    cfi_with_visit_info(tblDWSPCFIPlotVisitsComplete) |>
    cfi_with_tree_info(tblDWSPCFITreesComplete) |>
    cfi_with_plot_info(tblDWSPCFIPlotsComplete) |>
    cfi_abp(cfiabp_trees) |>
    mutate(
      YearCut = VisitCycle - YearsSinceLastCut
    ) |>
    filter(
      !is.na(YearCut) & !is.na(dbh_prior)
    ) |>
    # When harvest and inventory occur in the same year,
    # harvest might occur before or after the inventory.
    # To simplify things, we'll make all harvest happen
    # before the inventory.
    # Select the post-harvest inventory as the VisitCycle
    mutate(
      VisitCycle = ceiling(YearCut / 10) * 10,
      DBH_MIN = floor(conv_unit(dbh_prior, "cm", "in") / 6) * 6
    ) |>
    group_by(MasterPlotID, VisitCycle, SpeciesCode, DBH_MIN) |>
    summarize(YearCut = max(YearCut), .groups = "drop")
  
  cficop_hvst_tpa <- qryDWSPCFIPlotVisitTreeDetail |>
    cfi_with_visit_info(tblDWSPCFIPlotVisitsComplete) |>
    cfi_with_tree_info(tblDWSPCFITreesComplete) |>
    cfi_with_plot_info(tblDWSPCFIPlotsComplete) |>
    cfi_abp(cfiabp_trees) |>
    filter(cfi_status_live(VisitTreeStatusCode)) |>
    filter(!is.na(dbh_prior)) |>
    mutate(
      DBH_MIN = floor(conv_unit(dbh_prior, "cm", "in") / 6) * 6
    ) |>
    semi_join(
      tmp_harvested,
      by = join_by(MasterPlotID, VisitCycle, SpeciesCode, DBH_MIN)
    ) |>
    group_by(MasterPlotID, VisitCycle, SpeciesCode, DBH_MIN) |>
    summarize(
      TPA = n() * 5, # Each plot is 1/5 acre
      .groups = "drop"
    ) |>
    # Add back in the things that were harvested down to 0 TPA
    # This will also add YearCut to the things harvested to >0 TPA
    full_join(
      tmp_harvested,
      by = join_by(MasterPlotID, VisitCycle, SpeciesCode, DBH_MIN)
    ) |>
    # The harvest blocks added back will have TPA == NA
    mutate(
      DBH_MAX = DBH_MIN + 6,
      TPA = if_else(is.na(TPA), 0, TPA),
      BA = 0
    ) |>
    # For prescription-based harvest, our schema is:
    # STAND_CN, TREE_CN, PREV_TRE_CN, YEAR, PRESCRIPTION
    # For DBH-based harvest, our schema is:
    # STAND_CN, YEAR, DBH_MIN, DBH_MAX, SPCD, TPA, BA
    mutate(
      YEAR = YearCut
    ) |>
    left_join(
      species_crosswalk |> select(SPCD, FVS_SPCD),
      by = join_by(SpeciesCode == SPCD)
    ) |>
    select(
      STAND_CN = MasterPlotID,
      YEAR = YearCut,
      DBH_MIN,
      DBH_MAX,
      SPCD = FVS_SPCD,
      TPA,
      BA
    )
})
