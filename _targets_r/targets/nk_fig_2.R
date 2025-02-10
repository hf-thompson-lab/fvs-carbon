nk_read_fig_2 <- function(dir) {
  nk_read_fig_2_helper <- function(dir, column) {
    filename <- file.path(dir, paste0("NK_Fig2_", column, ".csv"))
    read_csv(
      filename,
      col_names = c("Year", "value"),
      # Read columns as double, since that is how they were written
      col_types = "dd"
    ) |>
    # Round columns to nearest integer, since that is what was intended
    mutate(
      Year = round(Year),
      name = column,
      value = round(value)
    )
  }
  columns <- c(
    "NoManagement",
    "ClearcutHigh", "ClearcutLow",
    "ShelterwoodHigh", "ShelterwoodLow",
    "ITS_LowHigh", "ITS_LowLow", "ITS_HighHigh", "ITS_HighLow"
  )
  bind_rows(lapply(columns, \(column) nk_read_fig_2_helper(dir, column) )) |>
    pivot_wider(id_cols = "Year") |>
    # The last year in Fig2 is 2164; nudge it to 2165
    # to align with contemporary FVS runs.
    mutate(Year = if_else(Year == 2164, 2165, Year)) |>
    arrange(Year)
}

# By specifying the folder as the file source,
# we make nk_fig_2 depend on all files in the folder.
tarchetypes::tar_file_read(
  nk_fig_2,
  "data/raw/nk_fig2/",
  nk_read_fig_2(!!.x)
)
