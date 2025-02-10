tar_target(fia_ref_species, {
  con <- DBI::dbConnect(RSQLite::SQLite(), fiadb, flags = SQLITE_RO)
  on.exit(dbDisconnect(con), add = TRUE, after = FALSE)
  
  tbl(con, 'REF_SPECIES') |>
    select(SPCD, GENUS, SPECIES, E_SPGRPCD, JENKINS_SPGRPCD, SCIENTIFIC_NAME, COMMON_NAME) |>
    left_join(
      tbl(con, 'REF_SPECIES_GROUP') |> distinct(SPGRPCD, NAME),
      by = join_by(E_SPGRPCD == SPGRPCD)
    ) |>
    rename(SPGRPCD = E_SPGRPCD, SPGRP_NAME = NAME) |>
    collect()
})
