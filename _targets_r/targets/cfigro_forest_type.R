tar_target(cfigro_forest_type, {
  stocking_table <- fiafg_table_5f |>
    pivot_longer(-"Species", names_to = "dbh", values_to = "stocking") |>
    mutate(dbh = as.numeric(dbh)) |>
    rename(SpeciesCode = Species, min_dbh = dbh) |>
    group_by(SpeciesCode) |>
    arrange(rev(min_dbh)) |>
    mutate(max_dbh = replace_na(lag(min_dbh), Inf))
  
  qryDWSPCFIPlotVisitTreeDetail |>
    cfi_with_visit_info(tblDWSPCFIPlotVisitsComplete) |>
    cfi_with_tree_info(tblDWSPCFITreesComplete) |>
    cfi_with_plot_info(tblDWSPCFIPlotsComplete) |>
    filter(VisitTreeStatusCode == 1) |> # Only live trees
    filter(VisitCycle == 1970) |>
    left_join(
      stocking_table,
      by = join_by(
        SpeciesCode,
        VisitTreeDIAM >= min_dbh,
        VisitTreeDIAM < max_dbh
      )
    ) |>
    group_by(MasterPlotID, SpeciesCode) |>
    summarize(
      stocking = sum(stocking, na.rm = TRUE) * 5, # Each tree is 5 trees per acre
      .groups = "keep"
    ) |>
    ungroup() |>
    left_join(
      species_crosswalk |>
        select(SPCD, SCIENTIFIC_NAME, HARD_SOFT),
      by = join_by(SpeciesCode == SPCD)
    ) |>
    group_by(MasterPlotID) |>
    mutate(
      total_stocking = sum(stocking),
      hardwood_pct = sum(if_else(HARD_SOFT == "HARDWOOD", stocking, 0)) / total_stocking,
      softwood_pct = sum(if_else(HARD_SOFT == "SOFTWOOD", stocking, 0)) / total_stocking,
    ) |>
    ungroup() |>
    mutate(stocking_pct = stocking / total_stocking) |>
    #filter(stocking_pct >= 0.1) |>
    pivot_wider(
      id_cols = c("MasterPlotID", "total_stocking", "softwood_pct"),
      names_from = "SCIENTIFIC_NAME",
      names_sort = TRUE,
      values_from = "stocking_pct",
      values_fill = 0
    ) |>
    mutate(
      FORTYPCD = case_when(
        # If softwoods are 50% or more of the stocking, it is a softwood type.
        
        # White/Red/Jack Pine group
        `Pinus resinosa` >= 0.5 ~ 102, # Red pine
        `Pinus strobus` >= 0.5 ~ 103, # Pure eastern white pine
        `Tsuga canadensis` >= 0.5 ~ 105, # Pure eastern hemlock
        # Spruce / Fir group
        `Picea rubens` >= 0.5 ~ 123, # red spruce
        
        # less pure versions of softwood stands
        # these come after the pure versions, so the pure versions are caught first
        `Pinus strobus` + `Tsuga canadensis` >= 0.5 ~ 104, # Eastern white pine / eastern hemlock
        `Pinus resinosa` >= 0.20 & softwood_pct >= 0.5 ~ 102, # Red pine, even if mixed
        `Pinus rigida` >= 0.20 & softwood_pct >= 0.5 ~ 167, # Pitch pine, even if mixed
        `Pinus strobus` >= 0.20 & softwood_pct >= 0.5 ~ 103, # Eastern white pine, even if mixed
        `Tsuga canadensis` >= 0.20 & softwood_pct >= 0.5 ~ 105, # Eastern hemlock, even if mixed
        
        # For the Eastern United States, when the pine and/or redcedar component is
        # between 25% and 49% of the stocking, there are mixed hardwood-pine forest types.
        # Therefore we consider hardwood groups to only be when softwood_pct < 0.25
        
        # Oak / Hickory group
        `Quercus prinus` >= 0.5 & softwood_pct < 0.25 ~ 502, # Chestnut oak
        `Quercus alba` >= 0.5 & softwood_pct < 0.25 ~ 503, # White oak/red oak/hickory
        `Quercus rubra` >= 0.5 & softwood_pct < 0.25 ~ 503, # White oak/red oak/hickory
        `Quercus velutina` >= 0.5 & softwood_pct < 0.25 ~ 515, # Chestnut oak/black oak/scarlet oak
        `Fraxinus americana` >= 0.5 & softwood_pct < 0.25 ~ 516, # Cherry/white ash/yellow-poplar
        # MAPLE/BEECH/BIRCH GROUP
        `Acer saccharum` >= 0.5 & softwood_pct < 0.25 ~ 801, # Sugar maple/beech/yellow birch
        `Betula alleghaniensis` >= 0.5 & softwood_pct < 0.25 ~ 801, # Sugar maple/beech/yellow birch
        `Acer rubrum` >= 0.5 & softwood_pct < 0.25 ~ 809, # Red maple/upland
        # ASPEN/BIRCH GROUP
        `Betula papyrifera` >= 0.5 & softwood_pct < 0.25 ~ 902, # Paper birch
        
        # less pure versions of hardwood stands
        # these come after the pure versions, so the pure versions are caught first
        `Quercus alba` >= 0.20 & softwood_pct < 0.25 ~ 503, # White oak/red oak/hickory
        `Quercus rubra` >= 0.20 & softwood_pct < 0.25 ~ 503, # White oak/red oak/hickory
        `Quercus velutina` >= 0.20 & softwood_pct < 0.25 ~ 515, # Chestnut oak/black oak/scarlet oak
        `Acer rubrum` >= 0.20 & softwood_pct < 0.25 ~ 809, # Red maple/upland
        `Acer saccharum` >= 0.20 & softwood_pct < 0.25 ~ 801, # Sugar maple/beech/yellow birch
        `Fraxinus americana` >= 0.20 & softwood_pct < 0.25 ~ 516, # Cherry/white ash/yellow-poplar
        `Betula papyrifera` >= 0.20 & softwood_pct < 0.25 ~ 902, # Paper birch
        `Betula alleghaniensis` >= 0.20 & softwood_pct < 0.25 ~ 801, # Sugar maple/beech/yellow birch
        
        # Mixed hard and softwood stands
        (`Pinus strobus` + `Tsuga canadensis`) >= 0.1 & softwood_pct >= 0.25 ~ 401, # Eastern white pine/northern red oak/white ash
        softwood_pct >= 0.25 ~ 409, # Other pine/hardwood
       
        # This leaves us with stands dominated by sweet birch, which FIA
        # seems to think can't happen. Assign them 801 sugar maple, even though there's
        # no sugar maple on there.
        `Betula lenta` >= 0.20 ~ 801, # Sugar maple/beech/yellow birch
      )
    ) |>
    left_join(
        fia_tbl(fiadb, "REF_FOREST_TYPE", \(.data, con) {
          .data |>
            select(VALUE, MEANING) |>
            rename(
              FORTYPCD = VALUE,
              FOREST_TYPE = MEANING,
            )
        }),
        by = join_by(FORTYPCD)
    )
})
