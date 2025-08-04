tar_target(cfigro_bai_model, {
  cfigro_bai_task <- as_task_regr(
    cfigro_training_data,
    target = "BAI_Residual"
  )
  cfigro_bai_task$select(
    c("Acer_rubrum", "LAT", "Projected_BA", "Projected_Tph", "Quercus_rubra")
  )
  
  cfigro_bai_model <- lrn(
    "regr.ranger",
    importance = "impurity",
    max.depth = 28,
    min.node.size = 11,
    mtry = 2,
    num.trees = 70
  )
  
  cfigro_bai_model$train(cfigro_bai_task)
  
  cfigro_bai_model
})
