tar_target(cfigro_none_death, {
  # All the trees we'll give to FVS for calibration
  # There are 3 sets of trees:
  # 1. Trees that are live (1) in 1970 that are standing (2) or fallen (3) dead in 1980
  # 2. Trees that are dead in 1970 and still dead in 1980
  # 3. Trees that are not present in 1970 then establish and die in 1980
  tmp_trees_1970 <- qryDWSPCFIPlotVisitTreeDetail |>
    left_join(
      tblDWSPCFIPlotVisitsComplete |>
        select(MasterPlotID, VisitCycle, MasterPlotVisitID),
      by = join_by(MasterPlotVisitID)
    ) |>
    left_join( 
      tblDWSPCFITreesComplete |> select(MasterTreeID, SpeciesCode),
      by = join_by(MasterTreeID)
    ) |>
    inner_join(
      cfiabp_trees |> distinct(MasterPlotID, VisitCycle, MasterTreeID, StatusB),
      by = join_by(MasterPlotID, VisitCycle, MasterTreeID)
    ) |>
    filter(VisitCycle == 1970) |>
    mutate(
      TREE_CN1 = as.character(VisitTreeNumberDetail),
      STATUS1 = VisitTreeStatusCode,
      DIAM1 = if_else(VisitTreeDIAM > 0, VisitTreeDIAM, NA),
      HT1 = if_else(VisitTreeTotalHeight > 0, VisitTreeTotalHeight, NA)
    ) |>
    select(MasterTreeID, TREE_CN1, STATUS1, DIAM1, HT1)
  
  tmp_trees_1980 <- qryDWSPCFIPlotVisitTreeDetail |>
    filter(cfi_status_dead(VisitTreeStatusCode)) |>
    left_join(
      tblDWSPCFIPlotVisitsComplete |>
        select(MasterPlotID, VisitCycle, MasterPlotVisitID),
      by = join_by(MasterPlotVisitID)
    ) |>
    left_join( 
      tblDWSPCFITreesComplete |> select(MasterTreeID, SpeciesCode),
      by = join_by(MasterTreeID)
    ) |>
    inner_join(
      cfiabp_trees |> distinct(MasterPlotID, VisitCycle, MasterTreeID, StatusB),
      by = join_by(MasterPlotID, VisitCycle, MasterTreeID)
    ) |>
    filter(VisitCycle == 1980) |>
    left_join(species_crosswalk |> select(SPCD, FVS_SPCD), by = join_by(SpeciesCode == SPCD)) |>
    mutate(
      SPECIES = FVS_SPCD,
      TREE_CN2 = as.character(VisitTreeNumberDetail),
      STATUS2 = VisitTreeStatusCode,
      DIAM2 = if_else(VisitTreeDIAM > 0, VisitTreeDIAM, NA),
      HT2 = if_else(VisitTreeTotalHeight > 0, VisitTreeTotalHeight, NA)
    ) |>
    select(MasterPlotID, MasterTreeID, SPECIES, TREE_CN2, STATUS2, DIAM2, HT2)
  
  tmp_trees_death <- tmp_trees_1980 |>
    left_join(tmp_trees_1970, by = join_by(MasterTreeID))
  
  tmp_stand_cn <- tblDWSPCFIPlotVisitsComplete |>
    filter(VisitCycle == 1970) |>
    mutate(STAND_CN = as.character(MasterPlotVisitID)) |>
    select(MasterPlotID, STAND_CN)
  
  tmp_stand_info <- tblDWSPCFIPlotsComplete |>
    select(MasterPlotID, SLOPE = Slope, ASPECT = Aspect) |>
    left_join(tmp_stand_cn, by = join_by(MasterPlotID))
  
  cfigro_none_death <- tmp_trees_death |>
    left_join(tmp_stand_info, by = join_by(MasterPlotID)) |>
    mutate(
      STAND_ID = MasterPlotID,
      PLOT_CN = STAND_CN,
      PLOT_ID = 1,
      STANDPLOT_CN = paste0(STAND_CN, "_", PLOT_CN),
      STANDPLOT_ID = paste0(STAND_ID, "_", PLOT_ID),
      TREE_CN = coalesce(TREE_CN1, TREE_CN2),
      TREE_ID = MasterTreeID,
      TREE_COUNT = 1,
      HISTORY = case_when(
        is.na(STATUS1) & cfi_status_dead(STATUS2) ~ 6,
        cfi_status_live(STATUS1) & cfi_status_dead(STATUS2) ~ 6,
        cfi_status_dead(STATUS1) & cfi_status_dead(STATUS2) ~ 8
      ),
      DIAMETER = coalesce(DIAM2, DIAM1),
      HT = coalesce(HT2, HT1)
    ) |>
    filter(!is.na(HISTORY) & !is.na(SPECIES)) |>
    select(
      STAND_CN,
      STAND_ID,
      PLOT_CN,
      PLOT_ID,
      STANDPLOT_CN,
      STANDPLOT_ID,
      TREE_CN,
      TREE_ID,
      TREE_COUNT,
      SPECIES,
      HISTORY,
      DIAMETER,
      HT,
      SLOPE,
      ASPECT
    )
})
