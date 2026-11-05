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
    # At this point we have fully-decorated tree records;
    # filter to trees that are either Live or Dead, but
    # not Cut or Recruit
    filter(StatusB %in% c("L", "D")) |>
    # Accept dbh_prior or dbhcm for dbh at the time of harvest; ignore
    # trees whose diameter can't be determined.
    mutate(
      DBH = conv_unit(dbhcm, "cm", "in"),
      dbh_harvest = coalesce(dbhcm, dbh_prior),
      DBH_MIN = floor(conv_unit(dbh_harvest, "cm", "in") / 5) * 5
    ) |>
    filter(!is.na(dbh_harvest)) |>
    mutate(
      basal_area = (conv_unit(dbh_harvest, "cm", "m") / 2)^2 * pi,
      # HarvestYear isn't reset if the plot isn't harvested
      HarvestYear = if_else(
        CutSinceLastVisit,
        VisitCycle - YearsSinceLastCut,
        NA
      ),
      PreHarvestVisitCycle = floor(HarvestYear / 10) * 10
    )
})
