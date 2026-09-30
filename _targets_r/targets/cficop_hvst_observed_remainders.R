tar_target(cficop_hvst_observed_remainders, {
  cficop_hvst_observed_remainders <- qryDWSPCFIPlotVisitTreeDetail |>
    cfi_with_visit_info(tblDWSPCFIPlotVisitsComplete) |>
    cfi_with_tree_info(tblDWSPCFITreesComplete) |>
    cfi_with_plot_info(tblDWSPCFIPlotsComplete) |>
    cfi_abp(cfiabp_trees) |>
    # Deal with species that FVS doesn't handle
    mutate(SpeciesCode = replace_values(
      SpeciesCode,
      320 ~ 317, # norway maple -> sugar maple
      402 ~ 403, # bitternut hickory -> pignut hickory
      740 ~ 743  # unknown aspen -> bigtooth aspen
    )) |>
    left_join(
      species_crosswalk |> select(SPCD, GENUS, SPECIES, FVS_SPCD),
      by = join_by(SpeciesCode == SPCD)
    ) |>
    filter(!is.na(FVS_SPCD)) |>
    # Live trees, not recruits
    filter(StatusB == "L") |>
    mutate(
      dbh_harvest = coalesce(dbh_prior, dbhcm),
      DBH_MIN = floor(conv_unit(dbh_harvest, "cm", "in") / 5) * 5
    ) |>
    filter(!is.na(dbh_harvest)) |>
    inner_join(
      # Which sizes of which species are harvested on which plots, in
      # which visit cycles? Keep only those ones.
      cficop_hvst_observed_harvest |>
        distinct(MasterPlotID, VisitCycle, SpeciesCode, DBH_MIN, HarvestYear),
      by = join_by(MasterPlotID, VisitCycle, SpeciesCode, DBH_MIN)
    ) |>
    mutate(
      # Judgment: basal_area intended to be left in harvest is better
      # represented by ba PRIOR to harvest, than POST harvest. We don't
      # have BA at the time of harvest, which would be ideal.
      basal_area = (conv_unit(dbh_prior, "cm", "m") / 2)^2 * pi
    )
})
