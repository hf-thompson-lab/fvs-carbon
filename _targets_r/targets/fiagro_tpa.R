tar_target(
  fiagro_tpa,
  {
    do.call(
      rbind,
      pbapply(fiagro_states, 1, \(state) {
        fia_tpa_for_state(fiadb, state[["STATECD"]])
      })
    )
  },
  iteration = "vector",
  # cross() and map() are unparsed targets:: functions here:
  pattern = map(fiagro_states)
)
