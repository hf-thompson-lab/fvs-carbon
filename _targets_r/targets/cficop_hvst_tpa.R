tar_target(cficop_hvst_tpa, {
  # Find the trees left after each harvest
  cficop_hvst_tpa <- cficop_hvst_observed_remainders |>
    mutate(
      DBH_MIN = floor(conv_unit(dbh_prior, "cm", "in") / 5) * 5
    ) |>
    group_by(MasterPlotID, VisitCycle, FVS_SPCD, DBH_MIN, HarvestYear) |>
    summarize(
      TPA = n() * 5, # Each plot is 1/5 acre
      .groups = "drop"
    ) |>
    # Add back in the things that were harvested down to 0 TPA
    # This will also add HarvestYear to the things harvested to >0 TPA
    full_join(
      cficop_hvst_observed_harvest |>
        mutate(DBH_MIN = floor(conv_unit(dbh_prior, "cm", "in") / 5) * 5) |>
        distinct(MasterPlotID, VisitCycle, FVS_SPCD, DBH_MIN, HarvestYear),
      by = join_by(MasterPlotID, VisitCycle, FVS_SPCD, DBH_MIN)
    ) |>
    # Never harvest things with DBH < 5"
    filter(
      DBH_MIN > 0
    ) |>
    mutate(
      HarvestYear = coalesce(HarvestYear.x, HarvestYear.y),
      DBH_MAX = DBH_MIN + 5,
      # Harvests added by the full_join will nave TPA==NA;
      # there were not trees of this species left in this size range,
      # so set the residual TPA to 0.
      TPA = coalesce(TPA, 0),
      BA = 0 # We're not using BA
    ) |>
    # For prescription-based harvest, our schema is:
    # STAND_CN, TREE_CN, PREV_TRE_CN, YEAR, PRESCRIPTION
    # For DBH-based harvest, our schema is:
    # STAND_CN, YEAR, DBH_MIN, DBH_MAX, SPCD, TPA, BA
    left_join(
      cfigro_plot |>
        filter(INV_YEAR == 1970) |>
        select(STAND_CN, STAND_ID),
      by = join_by(MasterPlotID == STAND_ID)
    ) |>
    select(
      STAND_CN,
      YEAR = HarvestYear,
      DBH_MIN,
      DBH_MAX,
      SPCD = FVS_SPCD,
      TPA,
      BA
    ) |>
    # Arrange this for humans to be able to digest
    arrange(YEAR, SPCD, DBH_MIN)
})
