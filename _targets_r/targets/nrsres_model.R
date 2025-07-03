tar_target(nrsres_model, {
  # If tuning from the tuner implementation (ti), you can set hyperparameters 
  # directly from the tuner:
  #   nrsres_model = lrn("regr.ranger")
  #   nrsres_model$param_set$values = ti_lrn$result_learner_param_vals
  # For reproducibility, we set hyperparameters manually:
  nrsres_model = lrn(
    "regr.ranger",
    importance = "impurity",
    max.depth = 19,
    min.node.size = 11,
    mtry = 2,
    num.trees = 96
  )
  nrsres_model$train(nrsres_bai_task)
  nrsres_model # The model must be saved/restored in conjunction with the learner
})
