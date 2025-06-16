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
      PreviousDbhCM = lag(dbhcm),
      PreviousStemsHa = lag(stems.ha.All),
      PreviousHeight = lag(VisitTreeTotalHeight)
    ) |>
    ungroup() |>
    cfi_topocode() |>
    cfi_history() |>
    mutate(
      STAND_CN = MasterPlotID, # Could use MasterPlotVisitID, but why make life harder?
      STAND_ID = MasterPlotID,
      TREE_CN = MasterTreeID, # Could use VisitTreeNumberDetail, but why make life harder?
      TREE_ID = MasterTreeID,
      TREE_COUNT = conv_unit(stems.ha.All, "acre", "hectare"),
      HISTORY = HISTORY,
      SPECIES = SpeciesCode, # appears to be FIA Species
      DIAMETER = conv_unit(dbhcm, "cm", "in"),
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
      DG = conv_unit(PreviousDbhCM, "cm", "in"),
      HTG = PreviousHeight
    ) |>
    select(
      STAND_CN, STAND_ID, TREE_CN, TREE_ID,
      TREE_COUNT, HISTORY, SPECIES, DIAMETER, HT,
      SLOPE, ASPECT, TOPOCODE, DG, HTG
    )
})
