tar_target(cfigro_bai_model, {
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
    max.depth = 16,
    min.node.size = 11,
    mtry = 2,
    num.trees = 68
  )
  
  cfigro_bai_model$train(cfigro_bai_task)
  
  cfigro_bai_model
})
