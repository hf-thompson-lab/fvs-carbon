tar_target(cficop_hvst_estab, {
  # Find 1970 trees; we'll remove these from 1980
  tmp_trees_1970 <- qryDWSPCFIPlotVisitTreeDetail |>
    cfi_with_visit_info(tblDWSPCFIPlotVisitsComplete) |>
    cfi_with_tree_info(tblDWSPCFITreesComplete) |>
    cfi_with_plot_info(tblDWSPCFIPlotsComplete) |>
    cfi_abp(cfiabp_trees) |>
    filter(
      VisitCycle == 1970
    ) |>
    select(
      MasterPlotID, MasterTreeID
    )
  
  # Find 1980 trees, less those that were around in 1970
  tmp_estab_1980 <- qryDWSPCFIPlotVisitTreeDetail |>
    cfi_with_visit_info(tblDWSPCFIPlotVisitsComplete) |>
    cfi_with_tree_info(tblDWSPCFITreesComplete) |>
    cfi_with_plot_info(tblDWSPCFIPlotsComplete) |>
    cfi_abp(cfiabp_trees) |>
    # Live trees and recruits
    filter(StatusB == "L" | StatusB == "R" | Status6 == "L" | Status6 == "R") |>
    # Only trees that were around in 1980
    filter(
      VisitCycle == 1980
    ) |>
    # Not trees that were around in 1970
    anti_join(
      tmp_trees_1970,
      by = join_by(MasterPlotID, MasterTreeID)
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
      VisitTreeTotalHeight = if_else(
        is.na(VisitTreeTotalHeight) | VisitTreeTotalHeight == 0,
        mean(VisitTreeTotalHeight, na.rm = TRUE),
        VisitTreeTotalHeight
      ),
      DENSITY = 5  # trees per acre for one tree on a 1/5 acre plot
    ) |>
    ungroup() |>
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
  
  # Add the 1980 establishment to the establishment from Fred Hunt's
  # thesis to create establishment for the harvest scenarios
  cficop_hvst_estab <- cficop_dflt_estab |>
    mutate(
      STAND_ID = NA,
      HEIGHT = 0
    ) |>
    union_all(
      tmp_estab_1980
    )
})
