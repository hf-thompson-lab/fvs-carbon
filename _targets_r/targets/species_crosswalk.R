tar_target(species_crosswalk, {
  jenkins_spgrpcd_mixin <- fia_table_11_5_17 |>
    select(JENKINS_SPGRPCD, NAME) |>
    rename(JENKINS_SPGRP_NAME = NAME)
  
  fvs_spcd_mixin <- fvsne_table_3_2_1 |>
    select(`FIA Code`, `Species Number`, `Species Code`) |>
    rename(
      SPCD = `FIA Code`,
      FVS_SPNO = `Species Number`,
      FVS_SPCD = `Species Code`
    )
  
  fia_ref_species |>
    left_join(jenkins_spgrpcd_mixin, by = join_by(JENKINS_SPGRPCD)) |>
    left_join(fvs_spcd_mixin, by = join_by(SPCD)) |>
    mutate(HARD_SOFT = case_when(
      SPGRPCD %in% 1:24 ~ "SOFTWOOD",
      SPGRPCD %in% 25:48 ~ "HARDWOOD",
      grepl("hardwood", SPGRP_NAME) ~ "HARDWOOD",
      grepl("softwood", SPGRP_NAME) ~ "SOFTWOOD",
      grepl("hardwood", JENKINS_SPGRP_NAME) ~ "HARDWOOD",
      grepl("softwood", JENKINS_SPGRP_NAME) ~ "SOFTWOOD",
      .default = NA
    ))
})
