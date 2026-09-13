tar_target(fiagro_states, {
  fia_plots_filtered(fiadb, plots = NULL, \(.data, con) {
    fia_state <- tbl(con, "REF_RESEARCH_STATION") |>
      distinct(STATECD, STATE_NAME, STATE_ABBR)
    .data |>
      filter(STATECD <= 56) |> # Wyoming is 56 because there are gaps
      distinct(STATECD) |>
      left_join(fia_state, by = join_by(STATECD)) |>
      filter(STATE_NAME != "Hawaii")
  })
})
