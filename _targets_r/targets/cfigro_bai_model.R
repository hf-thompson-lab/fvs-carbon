tar_target(cfigro_bai_model, {
  cfigro_bai_task <- as_task_regr(
    cfigro_training_data,
    target = "BAI_Residual"
  )
  cfigro_bai_task$select(
    c("Acer_rubrum", "ELEV", "LAT", "Projected_BA", "Projected_Tph", "Pinus_strobus")
  )
  
  #cfigro_bai_model <- lrn(
  #  "regr.ranger",
  #  importance = "permutation",
  #  max.depth = 30,
  #  min.node.size = 17,
  #  mtry = 2,
  #  num.trees = 31
  #)
  cfigro_bai_model <- lrn(
    "regr.ranger",
    importance = "impurity",
    max.depth = 9,
    min.node.size = 11,
    mtry = 2,
    num.trees = 63
  )
  
  cfigro_bai_model$train(cfigro_bai_task)
  
  cfigro_bai_model
})
