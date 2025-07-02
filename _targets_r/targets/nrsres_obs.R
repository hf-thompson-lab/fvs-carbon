tar_target(nrsres_obs, {
  nrsres_obs <- nrsres_depen |>
    left_join(nrsres_indep, by = join_by(StandID == STAND_ID))
})
