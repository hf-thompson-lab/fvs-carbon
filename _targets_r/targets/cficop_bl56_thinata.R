tar_target(cficop_bl56_thinata, {
  cficop_bl56_thinata <- cficop_none_treelist |>
    mutate(SpeciesFIA = as.integer(SpeciesFIA)) |>
    mutate(
      wood_type = case_when(
        SpeciesFIA == 125 | # Red Pine
          SpeciesFIA == 129 ~ # Eastern White Pine
          "Red/White Pine",
        SpeciesFIA == 833 | # Northern Red Oak
          SpeciesFIA == 318 | # Sugar Maple
          SpeciesFIA == 541 | # White Ash
          SpeciesFIA == 762 ~ # Black Cherry
          "High-Value Hardwood",
        .default = "Other"
      )
    ) |>
    filter(wood_type == "Red/White Pine" | wood_type == "High-Value Hardwood") |>
    # We're only harvesting 14" and bigger
    filter(DBH >= 14) |>
    # This finds the year when EITHER the valuable softwood or hardwood
    # reaches 1500 bd ft / acre; it's probably valid to harvest when the
    # COMBINED bd ft / acred reaches 1500 bd ft / acre
    group_by(Year, StandID, wood_type) |>
    summarize(
      BoardFeet = sum(BdFt * TPA),
      .groups = "drop"
    ) |>
    filter(BoardFeet > 1500) |>
    group_by(StandID) |>
    # First year over 1500 bd ft / acre is the harvest year
    slice_min(Year) |>
    ungroup() |>
    mutate(MasterPlotID = as.numeric(StandID)) |>
    select(MasterPlotID, HarvestYear = Year) |>
    # Harvest in the HarvestYear, and 20 years later
    mutate(
      HarvestPlus20 = HarvestYear + 20,
      HarvestPlus40 = HarvestYear + 40
    ) |>
    pivot_longer(!MasterPlotID, values_to = "HarvestYear") |>
    select(MasterPlotID, HarvestYear) |>
    filter(HarvestYear <= 2020) |>
    mutate(
      TARGET_TPA = 5, # MA Guidelines; since CUTEFF = 0.5, this can probably be 0
      DBH_MIN = 14, # MA Guidelines
      DBH_MAX = 999, # No upper bound
      PERCENT = 0.5 # MA guidelines
    ) |>
    # For ThinATA, our schema is:
    # STAND_CN, YEAR, TARGET_TPA, PERCENT, DBH_MIN, DBH_MAX
    # Not Implemented: HEIGHT_MIN, HEIGHT_MAX
    # Get STAND_CN
    left_join(
      cfigro_plot |>
        filter(INV_YEAR == 1970) |>
        select(STAND_CN, STAND_ID),
      by = join_by(MasterPlotID == STAND_ID)
    ) |>
    select(
      STAND_CN,
      YEAR = HarvestYear,
      TARGET_TPA,
      PERCENT,
      DBH_MIN,
      DBH_MAX
    ) |>
    # Arrange this for humans to be able to digest
    arrange(STAND_CN, YEAR)
})
