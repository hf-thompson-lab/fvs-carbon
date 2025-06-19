tar_target(cfigro_trees, {
  qryDWSPCFIPlotVisitTreeDetail |>
    cfi_with_visit_info(tblDWSPCFIPlotVisitsComplete) |>
    cfi_with_tree_info(tblDWSPCFITreesComplete) |>
    cfi_with_plot_info(tblDWSPCFIPlotsComplete) |>
    cfi_abp(cfiabp_trees) |>
    group_by(MasterPlotID, MasterTreeID) |>
    arrange(VisitCycle) |>
    mutate(
      PreviousStatus6 = lag(Status6),
      PreviousDIAM = lag(VisitTreeDIAM),
      PreviousHeight = lag(VisitTreeTotalHeight)
    ) |>
    ungroup() |>
    cfi_topocode() |>
    cfi_history() |>
    left_join(
      species_crosswalk |> select(SPCD, FVS_SPCD),
      by = join_by(SpeciesCode == SPCD)
    ) |>
    mutate(
      STAND_CN = as.character(MasterPlotVisitID),
      STAND_ID = MasterPlotID,
      TREE_CN = as.character(VisitTreeNumberDetail),
      TREE_ID = MasterTreeID,
      PLOT_ID = 1, # CFI does not use subplots, so all PLOT_IDs are 1
      INV_YEAR = VisitYear,
      TREE_COUNT = 1, #conv_unit(1, "acre", "ft2") / (pi * 52.7^2),
      HISTORY = HISTORY,
      SPECIES = FVS_SPCD,
      DIAMETER = VisitTreeDIAM,
      HT = VisitTreeTotalHeight,
      # HTTOPK = VisitTreeTotalHeight, # Height to top kill; we don't ahve that
      # CRRATIO            WAS USED - not available
      # DAMAGE1            WAS USED - not available
      # SEVERITY1          WAS USED - not available
      # DAMAGE2            WAS USED - not available
      # SEVERITY2          WAS USED - not available
      # DAMAGE3            WAS USED - not available
      # SEVERITY3          WAS USED - not avialable
      # TREEVALUE          WAS USED - X - tree removal priority
      # PRESCRIPTION       WAS USED - X - tree removal group
      # AGE                WAS USED - X
      SLOPE = Slope,
      ASPECT = Aspect,
      # PV_CODE            WAS USED - ecoregion? - not used
      TOPOCODE = TOPOCODE, # See EssentialFVS 5.4.1.2
      # SITEPREP           WAS USED - ?
      DG = PreviousDIAM,
      HTG = PreviousHeight
    ) |>
    select(
      STAND_CN, STAND_ID, TREE_CN, TREE_ID, PLOT_ID,
      TREE_COUNT, HISTORY, SPECIES, DIAMETER, HT,
      SLOPE, ASPECT, TOPOCODE, DG, HTG
    )
})
