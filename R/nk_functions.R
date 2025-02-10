nk_extract_regen <- function(nk_regen, mgmt_id) {
  nk_regen |>
    select(FVS_SPCD, {{mgmt_id}}) |>
    rename(SPECIES = FVS_SPCD, DENSITY = {{mgmt_id}}) |>
    filter(!is.na(DENSITY)) |>
    mutate(STAND_CN = NA, YEAR = NA)
}
