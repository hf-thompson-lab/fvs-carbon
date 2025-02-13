tar_target(fvsne_states, {
  fia_plots_filtered(fiadb, plots = NULL, \(.data, con) {
    fia_state <- tbl(con, "REF_RESEARCH_STATION") |>
      distinct(STATECD, STATE_NAME, STATE_ABBR)
  
    .data |>
      filter_plots_ners(con) |>
      distinct(STATECD) |>
      left_join(fia_state, by = join_by(STATECD))
  })
})
