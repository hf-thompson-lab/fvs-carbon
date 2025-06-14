tarchetypes::tar_file_read(
  tblDWSPCFIPlotVisitDisturbances,
  "data/raw/DWSP_CFI/tblDWSPCFIPlotVisitDisturbances.xlsx",
  read_excel(!!.x) |>
    # Fix column name
    rename(
      PlotVisitDisturbanceYearCorrected = PlotVisitDisturbandYearCorrected,
      MasterPlotVisitID = PlotVisitID
    ) |>
    left_join(
      tblSuppPlotDisturbanceCodes |>
        select(DisturbanceCode, DisturbanceDesc),
      by = join_by(PlotVisitDisturbanceCode == DisturbanceCode)
    ) |>
    mutate(
      DisturbanceDesc = as.factor(DisturbanceDesc)
    )
)
