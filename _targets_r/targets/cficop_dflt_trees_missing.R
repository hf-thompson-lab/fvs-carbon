tar_target(cficop_dflt_trees_missing, {
  # Grab FVS modeled growth for recruits from CFICOP_NONE -
  # this has all the plots with no harvest or disturbance.
  # Note that we want all the random seeds; they'll give a more
  # informative distribution of DBH and DG
  tmp_dg_estab <- fvs_read_output(cficop_none, "FVS_TreeList") |>
    # Big enough to have been in the previous inventory
    filter((DBH - DG) >= 6) |>
    # Group into 6" diameter ranges
    mutate(DBH_MIN = 6 * floor(DBH / 6)) |>
    group_by(SpeciesFVS, DBH_MIN) |>
    summarise(
      DG = mean(DG * TPA),
      .groups = "drop"
    )
  
  # Find recruits that were missed in 1970
  cficop_dflt_trees_missing <- qryDWSPCFIPlotVisitTreeDetail |>
    cfi_with_visit_info(tblDWSPCFIPlotVisitsComplete) |>
    cfi_with_tree_info(tblDWSPCFITreesComplete) |>
    cfi_with_plot_info(tblDWSPCFIPlotsComplete) |>
    cfi_abp(cfiabp_trees) |>
    cfi_topocode() |>
    # Only recruits
    filter(StatusB == "R") |>
    filter(VisitCycle >= 1980) |>
    # Deal with species that FVS doesn't handle
    mutate(
      SpeciesCode = replace_values(
        SpeciesCode,
        320 ~ 317, # norway maple -> sugar maple
        402 ~ 403, # bitternut hickory -> pignut hickory
        740 ~ 743  # unknown aspen -> bigtooth aspen
      ),
      DBH = conv_unit(dbhcm, "cm", "in"),
      DBH_MIN = 6 * floor(DBH / 6)
    ) |>
    left_join(
      species_crosswalk |> select(SPCD, FVS_SPCD),
      by = join_by(SpeciesCode == SPCD)
    ) |>
    filter(!is.na(FVS_SPCD)) |>
    # Back-project their diameter
    left_join(
      tmp_dg_estab,
      by = join_by(FVS_SPCD == SpeciesFVS, DBH_MIN)
    ) |>
    # Some species, e.g. Red Pine ("RN") don't get DG;
    # fill in a non-crazy default (0.1"/year)
    mutate(
      DG = coalesce(DG, 1),
      cycles_since_1970 = (VisitCycle - 1970) / 10,
      DIAMETER = DBH - cycles_since_1970 * DG,
      # Diameter in decimal inches
      DIAMETER = floor(DIAMETER *  10) / 10
    ) |>
    filter(DIAMETER >= 6) |>
    mutate(
      STAND_CN = as.character(MasterPlotVisitID),
      STAND_ID = MasterPlotID,
      TREE_CN = as.character(VisitTreeNumberDetail),
      TREE_ID = MasterTreeID,
      PLOT_ID = 1, # CFI does not use subplots, so all PLOT_IDs are 1
      INV_YEAR = 1970,
      TREE_COUNT = 1,
      HISTORY = 1, # Live tree
      SPECIES = FVS_SPCD,
      DIAMETER,
      HT = NA,
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
      DG = NA,
      HTG = NA
    ) |>
    select(
      STAND_CN, STAND_ID, TREE_CN, TREE_ID, PLOT_ID,
      TREE_COUNT, HISTORY, SPECIES, DIAMETER, HT,
      SLOPE, ASPECT, TOPOCODE, DG, HTG
    )
})
