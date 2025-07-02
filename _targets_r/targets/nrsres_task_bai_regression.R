tar_target(nrsres_task_bai_regression, {
  fva_bai_residual_training <- nrsres_obs |>
    select(
      BAI_Residual,
      LAT,
      LON,
      ELEV,
      ECOCD,
      ECOSUBCD,
      BALIVE_METRIC,
      # I dreamed last night (this is true) that I used BA_Calb and Tph_Calb,
      # and it improved the model and model workflow a lot.
      BA_Calb,
      Tph_Calb,
      FOREST_TYPE_GROUP,
      FOREST_TYPE,
      STDAGE,
      SLOPE,
      ASPECT,
      PHYSIOGRAPHIC_CLASS
    ) |>
    rename(
      BA_Start = BALIVE_METRIC,
      BA_Proj = BA_Calb,
      Tph_Proj = Tph_Calb
    )
  
  nrsres_task_bai_regression <- as_task_regr(
    fva_bai_residual_training,
    target = "BAI_Residual"
  )
})
