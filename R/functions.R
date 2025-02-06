hectare_at <- function(lat, lon) {
  old_axis_order <- st_axis_order()
  st_axis_order(TRUE)
  
  crs <- 'EPSG:4326'
  center <- c(lat, lon)
  center_point <- st_point(center)
  # 0.001 degrees is about 111.17 meters at the equator, so start with half of that
  approx_offset <- 0.001 / 2
  a <- st_sfc(center_point + c( approx_offset, 0), crs = crs)
  b <- st_sfc(center_point + c(-approx_offset, 0), crs = crs)
  # The result is the distance between a and b is in meters per 0.001 degree;
  # invert and multiply by 50 to get degrees per 50 meters
  fifty_meter_angle_lat <- 0.05 / as.numeric(st_distance(a, b))
  
  c <- st_sfc(center_point + c(0,  approx_offset), crs = crs)
  d <- st_sfc(center_point + c(0, -approx_offset), crs = crs)
  fifty_meter_angle_lon <- 0.05 / as.numeric(st_distance(c, d))
  
  south_west <- c(-fifty_meter_angle_lat, -fifty_meter_angle_lon)
  north_west <- c( fifty_meter_angle_lat, -fifty_meter_angle_lon)
  north_east <- c( fifty_meter_angle_lat,  fifty_meter_angle_lon)
  south_east <- c(-fifty_meter_angle_lat,  fifty_meter_angle_lon)
  
  # Note: winding direction matters!
  hectare_geometry = matrix(
    c(
      center + north_west,
      center + south_west,
      center + south_east,
      center + north_east,
      center + north_west
    ),
    ncol = 2,
    byrow = TRUE
  )
  hectare_polygon <- st_sfc(st_polygon(list(hectare_geometry)), crs = crs)
  st_axis_order(old_axis_order)
  hectare_polygon
}

generate_species_crosswalk <- function(fiadb, fvsne_table_3_2_1, fiadb_table_11_5_17) {
  con <- DBI::dbConnect(RSQLite::SQLite(), fiadb, flags = SQLITE_RO)
  on.exit(dbDisconnect(con), add = TRUE, after = FALSE)
  
  jenkins_spgrpcd_mixin <- fiadb_table_11_5_17 |>
    select(JENKINS_SPGRPCD, NAME) |>
    rename(JENKINS_SPGRP_NAME = NAME)
  
  fvs_spcd_mixin <- fvsne_table_3_2_1 |>
    select(`FIA Code`, `Species Number`, `Species Code`) |>
    rename(
      SPCD = `FIA Code`,
      FVS_SPNO = `Species Number`,
      FVS_SPCD = `Species Code`
    )
  
  tbl(con, 'REF_SPECIES') |>
    select(SPCD, GENUS, SPECIES, E_SPGRPCD, JENKINS_SPGRPCD, SCIENTIFIC_NAME, COMMON_NAME) |>
    left_join(
      tbl(con, 'REF_SPECIES_GROUP') |> distinct(SPGRPCD, NAME),
      by = join_by(E_SPGRPCD == SPGRPCD)
    ) |>
    rename(SPGRPCD = E_SPGRPCD, SPGRP_NAME = NAME) |>
    collect() |>
    left_join(jenkins_spgrpcd_mixin, by = join_by(JENKINS_SPGRPCD)) |>
    left_join(fvs_spcd_mixin, by = join_by(SPCD)) |>
    # From FIADB User Guides
    # Volume Database Description (version 9.2)
    # Appendix E: Tree Species Group Codes
    # Softwoods: 1 - 24
    # Hardwoods: 25 - 48
    # Tropical and subtropical: 51 - 54
    # Urban: 55, 56
    # SPGRP_NAME often has hardwood or softwood in the name;
    # less frequently, JENKINS_SPGRP_NAME does.
    mutate(HARD_SOFT = case_when(
      SPGRPCD %in% 1:24 ~ "SOFTWOOD",
      SPGRPCD %in% 25:48 ~ "HARDWOOD",
      grepl("hardwood", SPGRP_NAME) ~ "HARDWOOD",
      grepl("softwood", SPGRP_NAME) ~ "SOFTWOOD",
      grepl("hardwood", JENKINS_SPGRP_NAME) ~ "HARDWOOD",
      grepl("softwood", JENKINS_SPGRP_NAME) ~ "SOFTWOOD",
      .default = NA
    ))
}
