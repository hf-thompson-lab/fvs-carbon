tarchetypes::tar_file_read(
  fiadb_2_5_35_physclcd,
    "data/raw/FIADB_2.5.35_PHYSCLCD.csv",
  read_csv(!!.x, show_col_types = FALSE) |>
    # Remove headers embedded in the table
    filter(!is.na(Code)) |>
    # Make it easier to join
    rename(
      PHYSCLCD = Code,
      PHYSIOGRAPHIC_CLASS = Name
    ) |>
    mutate(
      PHYSIOGRAPHIC_CLASS = as.factor(PHYSIOGRAPHIC_CLASS)
    )
)
