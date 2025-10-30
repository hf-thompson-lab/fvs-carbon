tar_target(cficop_hvst_estab, {
  # Find 1970 trees
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
  
  tmp_estab_1980 <- qryDWSPCFIPlotVisitTreeDetail |>
    cfi_with_visit_info(tblDWSPCFIPlotVisitsComplete) |>
    cfi_with_tree_info(tblDWSPCFITreesComplete) |>
    cfi_with_plot_info(tblDWSPCFIPlotsComplete) |>
    cfi_abp(cfiabp_trees) |>
    # Remove any trees that were around in 1970
    anti_join(
      tmp_trees_1970,
      by = join_by(MasterPlotID, MasterTreeID)
    ) |>
    left_join(
      species_crosswalk |> select(SPCD, FVS_SPCD),
      by = join_by(SpeciesCode == SPCD)
    ) |>
    # Live trees and recruits
    filter(cfi_status_live(VisitTreeStatusCode)) |>
    mutate(
      diameter_class = as.integer(VisitTreeDIAM / 2) * 2
    ) |>
    filter(
      VisitCycle == 1980
    ) |>
    group_by(FVS_SPCD) |>
    # Impute mean height for missing values
    mutate(
      VisitTreeTotalHeight = if_else(
        is.na(VisitTreeTotalHeight) | VisitTreeTotalHeight == 0,
        mean(VisitTreeTotalHeight, na.rm = TRUE),
        VisitTreeTotalHeight
      ),
      DENSITY = 5  # trees per acre
    ) |>
    left_join(
      cfigro_plot |> select(STAND_ID, STAND_CN),
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
  
  cficop_hvst_estab <- cficop_dflt_estab |>
    mutate(
      STAND_ID = NA,
      HEIGHT = 0
    ) |>
    union_all(
      tmp_estab_1980
    )
})
