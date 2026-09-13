tar_target(
  fiagro_biomass,
  {
    do.call(
      rbind,
      pbapply(fiagro_states, 1, \(state) {
        fia_biomass_for_state(fiadb, state[["STATECD"]])
      })
    )
  },
  iteration = "vector",
  # cross() and map() are unparsed targets:: functions here:
  pattern = map(fiagro_states)
)
