tar_target(cfigro_bai_model, {
  cfigro_bai_task <- as_task_regr(
    cfigro_training_data,
    target = "BAI_Residual"
  )
  cfigro_bai_task$select(
    c("Acer_rubrum", "LAT", "Pinus_strobus", "Projected_BA", "Projected_Tph", "Quercus_rubra", "SLOPE")
  )
  
  cfigro_bai_model <- lrn(
    "regr.ranger",
    importance = "permutation",
    max.depth = 16,
    min.node.size = 5,
    mtry = 3,
    num.trees = 22
  )
  
  cfigro_bai_model$train(cfigro_bai_task)
  
  cfigro_bai_model
})
