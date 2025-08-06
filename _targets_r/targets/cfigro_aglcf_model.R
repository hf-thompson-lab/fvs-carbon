tar_target(cfigro_aglcf_model, {
  cfigro_aglcf_task <- as_task_regr(
    cfigro_training_data,
    target = "Carbon_Flux_Residual"
  )
  cfigro_aglcf_task$select(
    c("Acer_rubrum", "LAT", "Pinus_strobus", "Projected_BA", "Projected_Tph", "Quercus_rubra", "SLOPE")
  )
  cfigro_aglcf_model <- lrn(
    "regr.ranger",
    importance = "permutation",
    max.depth = 16,
    min.node.size = 5,
    mtry = 3,
    num.trees = 22
  )
  
  cfigro_aglcf_model$train(cfigro_aglcf_task)
  
  cfigro_aglcf_model
})
