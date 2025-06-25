tarchetypes::tar_file_read(
  qryDWSPCFIPlotVisitTreeDetail,
  "data/raw/DWSP_CFI/qryDWSPCFIPlotVisitTreeDetail_Partial_HF.csv",
  read_csv(!!.x, col_types = list(.default = col_double())) |>
    # Fix column names
    rename(
      MasterTreeID = VisitMasterTreeID,
      MasterPlotVisitID = VisitIDNumberDetail
    ) |>
    left_join(
      tblSuppTreeStatusCodes |>
        select(StatusCode, TreeStatusDesc) |>
        rename(VisitTreeStatusCode = StatusCode),
      by = join_by(VisitTreeStatusCode)
    )
)
