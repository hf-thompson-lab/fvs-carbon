tar_target(cficop_hvst_observed_harvest, {
  cficop_hvst_observed_harvest <- qryDWSPCFIPlotVisitTreeDetail |>
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
    # At this point we have fully-decorated tree records
    # Replace na's in status to keep them from propagating
    replace_na(list(StatusB = "X", Status6 = "X")) |>
    # If either of StatusB or Status6 is "C", they both are, but
    # this shows the intent
    filter(StatusB == "C" | Status6 == "C") |>
    # Some trees don't have a value for pre-harvest dbh; ignore them
    filter(!is.na(dbh_prior)) |>
    mutate(
      basal_area = (conv_unit(dbh_prior, "cm", "m") / 2)^2 * pi,
      HarvestYear = VisitCycle - YearsSinceLastCut,
      PreHarvestVisitCycle = floor(HarvestYear / 10) * 10
    )
})
