tarchetypes::tar_file_read(
  qryDWSPCFIPlotVisitTreeDetail,
  "data/raw/DWSP_CFI/qryDWSPCFIPlotVisitTreeDetail_Partial_HF.xlsx",
  read_excel(!!.x) |>
    # Fix column names
    rename(
      MasterTreeID = VisitMasterTreeID,
      MasterPlotVisitID = VisitIDNumberDetail
    ) |>
    # Fix column types - giving read_excel the right types causes it to hang
    mutate(
      MasterTreeID = as.numeric(MasterTreeID),
      VisitTreeNumberDetail = as.numeric(VisitTreeNumberDetail)
    ) |>
    left_join(
      tblSuppTreeStatusCodes |>
        select(StatusCode, TreeStatusDesc) |>
        rename(VisitTreeStatusCode = StatusCode),
      by = join_by(VisitTreeStatusCode)
    )
)
