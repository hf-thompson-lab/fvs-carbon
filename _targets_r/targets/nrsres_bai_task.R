tar_target(nrsres_bai_task, {
  fva_bai_residual_training <- nrsres_obs |>
    select(
      BAI_Residual,
      LAT,
  #    LON,
      ELEV,
  #    ECOCD,
  #    ECOSUBCD,
  #    BALIVE_METRIC,
      BA_Proj = BA_Calb,
      Tph_Proj = Tph_Calb,
      FOREST_TYPE_GROUP,
  #    FOREST_TYPE,
  #    STDAGE,
      SLOPE,
      ASPECT,
  #    PHYSIOGRAPHIC_CLASS
    )
  
  nrsres_bai_task <- as_task_regr(
    fva_bai_residual_training,
    target = "BAI_Residual"
  )
  
  # Remove incomplete rows from training data
  nrsres_bai_task$filter(nrsres_bai_task$row_ids[complete.cases(nrsres_bai_task$data())])
})
