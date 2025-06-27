tar_target(cfigro_none_growth, {
  # Trees to use for modeling height estimation
  cfigro_none_growth <- qryDWSPCFIPlotVisitTreeDetail |>
    filter(VisitTreeStatusCode == 1) |>
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
    filter(VisitCycle %in% c(1970, 1980)) |>
    group_by(MasterPlotID, MasterTreeID) |>
    arrange(VisitCycle) |>
    mutate(
      PREV_STATUSCD = lag(VisitTreeStatusCode),
      STAND_CN = as.character(lag(MasterPlotVisitID)),
      TREE_CN = as.character(lag(VisitTreeNumberDetail))
    ) |>
    ungroup() |>
    filter(VisitCycle == 1980 & PREV_STATUSCD == 1) |>
    select(
      STAND_CN,
      TREE_CN,
      DG = VisitTreeDIAM,
      HTG = VisitTreeTotalHeight
    )
})
