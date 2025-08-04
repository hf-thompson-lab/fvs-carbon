tar_target(cfigro_aglcf_model, {
  cfigro_aglcf_task <- as_task_regr(
    cfigro_training_data,
    target = "Carbon_Flux_Residual"
  )
  cfigro_aglcf_task$select(
    c("Acer_rubrum", "LAT", "Projected_BA", "Projected_Tph", "Quercus_rubra")
  )
  cfigro_aglcf_model <- lrn(
    "regr.ranger",
    importance = "impurity",
    max.depth = 28,
    min.node.size = 11,
    mtry = 2,
    num.trees = 70
  )
  
  cfigro_aglcf_model$train(cfigro_aglcf_task)
  
  cfigro_aglcf_model
})
