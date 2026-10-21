tar_target(cficop_hvst_observed_remainders, {
  # Go by the trees that FVS can see; it's more reliable than the CFI inventory
  cficop_hvst_observed_remainders <- fvs_read_output(cficop_srvy, "FVS_TreeList") |>
    filter(DBH >= 5) |>
    mutate(
      # DG is unreliable; don't try to guess DBH at time of harvest
      dbh_harvest = DBH,
      DBH_MIN = floor(DBH / 5) * 5,
      MasterPlotID = as.integer(StandID),
      VisitCycle = Year,
      SpeciesCode = as.integer(SpeciesFIA)
    ) |>
    inner_join(
      # Which sizes of which species are harvested on which plots, in
      # which visit cycles? Keep only those ones.
      cficop_hvst_observed_harvest |>
        distinct(MasterPlotID, VisitCycle, SpeciesCode, HarvestYear),
      by = join_by(MasterPlotID, VisitCycle, SpeciesCode)
    ) |>
    left_join(
      species_crosswalk |> select(SPCD, GENUS, SPECIES, FVS_SPCD),
      by = join_by(SpeciesCode == SPCD)
    ) |>
    mutate(
      # Judgment: basal_area intended to be left in harvest is better
      # represented by ba PRIOR to harvest, than POST harvest. We don't
      # have BA at the time of harvest, which would be ideal.
      basal_area = (conv_unit(DBH, "in", "m") / 2)^2 * pi
    )
})
