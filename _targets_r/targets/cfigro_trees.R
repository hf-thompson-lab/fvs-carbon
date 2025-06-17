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
    filter(VisitCycle == min(VisitCycle, na.rm = TRUE)) |>
    ungroup() |>
    cfi_topocode() |>
    cfi_history() |>
    left_join(
      species_crosswalk |> select(SPCD, FVS_SPCD),
      by = join_by(SpeciesCode == SPCD)
    ) |>
    mutate(
      STAND_CN = MasterPlotVisitID,
      STAND_ID = MasterPlotID,
      TREE_CN = VisitTreeNumberDetail,
      TREE_ID = MasterTreeID,
      PLOT_ID = 1, # CFI does not use subplots, so all PLOT_IDs are 1
      INV_YEAR = VisitYear,
      TREE_COUNT = conv_unit(stems.ha.All, "acre", "hectare"),
      HISTORY = HISTORY,
      SPECIES = FVS_SPCD,
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
      STAND_CN, STAND_ID, TREE_CN, TREE_ID, PLOT_ID,
      TREE_COUNT, HISTORY, SPECIES, DIAMETER, HT,
      SLOPE, ASPECT, TOPOCODE, DG, HTG
    )
})
