tar_target(cficop_hvst_estab_post_harvest, {
   tmp_post_harvest_plots <- cficop_hvst_observed_harvest |>
    distinct(MasterPlotID, VisitCycle) |>
    # Add in the post-post-harvest visit cycle
    mutate(VisitCycleAfter = VisitCycle + 10) |>
    pivot_longer(cols = c(VisitCycle, VisitCycleAfter), values_to = "VisitCycle") |>
    # This will have introduced duplicates if a plot was harvested in two
    # consecutive VisitCycles
    distinct(MasterPlotID, VisitCycle) |>
    # Don't do 1980; those are captured above
    filter(VisitCycle != 1980)
  
  cficop_hvst_estab_post_harvest <- qryDWSPCFIPlotVisitTreeDetail |>
    cfi_with_visit_info(tblDWSPCFIPlotVisitsComplete) |>
    cfi_with_tree_info(tblDWSPCFITreesComplete) |>
    cfi_with_plot_info(tblDWSPCFIPlotsComplete) |>
    cfi_abp(cfiabp_trees) |>
    # Only recruits
    filter(StatusB == "R") |>
    # Only trees that establish in post-harvest plots
    semi_join(
      tmp_post_harvest_plots,
      by = join_by(MasterPlotID, VisitCycle)
    ) |>
    # Deal with species that FVS doesn't handle
    mutate(SpeciesCode = replace_values(
      SpeciesCode,
      320 ~ 317, # norway maple -> sugar maple
      402 ~ 403, # bitternut hickory -> pignut hickory
      740 ~ 743  # unknown aspen -> bigtooth aspen
    )) |>
    left_join(
      species_crosswalk |> select(SPCD, FVS_SPCD),
      by = join_by(SpeciesCode == SPCD)
    ) |>
    filter(!is.na(FVS_SPCD)) |>
    # Tree height - some trees have it, some don't.
    # Impute mean height of those that have it for missing values
    group_by(FVS_SPCD) |>
    mutate(
      # A height of 0 is a missing value; fix that.
      VisitTreeTotalHeight = if_else(
        VisitTreeTotalHeight == 0, NA, VisitTreeTotalHeight
      ),
      VisitTreeTotalHeight = floor(if_else(
        is.na(VisitTreeTotalHeight),
        mean(VisitTreeTotalHeight, na.rm = TRUE),
        VisitTreeTotalHeight
      )),
      DENSITY = 5  # trees per acre for one tree on a 1/5 acre plot
    ) |>
    # Replase NaN HEIGHTs with 0 to avoid errors
    replace_na(list(VisitTreeTotalHeight = 0)) |> 
    ungroup() |>
  # DO NOT CONSOLIDATE REDUNDANT RECORDS - it makes FVS crash
  #  # Consolidate redundant records
  #  group_by(MasterPlotID, VisitCycle, FVS_SPCD, VisitTreeTotalHeight) |>
  #  summarize(DENSITY = sum(DENSITY), .groups = "drop") |>
  #  ungroup() |>
    # We need STAND_CN for the appropriate stand
    # We need STAND_CN for the 1970 stand
    left_join(
      cfigro_plot |>
        filter(INV_YEAR == 1970) |>
        select(STAND_ID, STAND_CN),
      by = join_by(MasterPlotID == STAND_ID)
    ) |>
    select(
      STAND_ID = MasterPlotID,
      STAND_CN,
      YEAR = VisitCycle,
      SPECIES = FVS_SPCD,
      DENSITY,
      HEIGHT = VisitTreeTotalHeight
    )
})
