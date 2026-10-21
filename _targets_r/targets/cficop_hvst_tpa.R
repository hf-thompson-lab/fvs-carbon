tar_target(cficop_hvst_tpa, {
  # Find the trees left after each harvest
  cficop_hvst_tpa <- cficop_hvst_observed_remainders |>
    filter(DBH > 5) |>
    group_by(MasterPlotID, VisitCycle, FVS_SPCD, HarvestYear) |>
    summarize(
      # TPA is (fractional) trees per acre
      # BA is square feet per acre
      # DBH is diameter in inches
      # So compute each tree's BA, multiply by its TPA, then sum
      BA_remainder = sum(TPA * pi * (DBH / 12 / 2)^2),
      # Ignored for now
      TPA_remainder = sum(TPA),
      .groups = "drop"
    ) |>
    # Add back in the things that were harvested down to 0 TPA
    # This will also add HarvestYear to the things harvested to >0 TPA
    full_join(
      cficop_hvst_observed_harvest |>
        group_by(MasterPlotID, VisitCycle, FVS_SPCD, HarvestYear) |>
        summarize(
          TPA_harvest = n() * 5,
          .groups = "drop"
        ),
      by = join_by(MasterPlotID, VisitCycle, FVS_SPCD)
    ) |>
    mutate(
      HarvestYear = coalesce(HarvestYear.x, HarvestYear.y),
      TPA_remainder = coalesce(TPA_remainder, 0),
      TPA_harvest = coalesce(TPA_harvest, 0),
      DBH_MIN = 5,
      DBH_MAX = 999,
      PERCENT = TPA_harvest / (TPA_harvest + TPA_remainder),
      # FVS under-harvests by about 40%, so goose PERCENT
      PERCENT = if_else(PERCENT + 0.2 > 1, 1, PERCENT + 0.2),
      TPA = 0, # CUTEFF says how many trees to actually harvest
      BA = NA # We're not using BA
    ) |>
    # For prescription-based harvest, our schema is:
    # STAND_CN, TREE_CN, PREV_TRE_CN, YEAR, PRESCRIPTION
    # For DBH-based harvest, our schema is:
    # STAND_CN, YEAR, DBH_MIN, DBH_MAX, CUTEFF, SPCD, TPA, BA
    left_join(
      cfigro_plot |>
        filter(INV_YEAR == 1970) |>
        select(STAND_CN, STAND_ID),
      by = join_by(MasterPlotID == STAND_ID)
    ) |>
    # Remove records for cohorts that weren't harvested
    filter(
      !is.na(HarvestYear)
    ) |>
    select(
      STAND_CN,
      YEAR = HarvestYear,
      DBH_MIN,
      DBH_MAX,
      PERCENT,
      SPCD = FVS_SPCD,
      TPA,
      BA
    ) |>
    # Arrange this for humans to be able to digest
    arrange(YEAR, SPCD, DBH_MIN)
})
