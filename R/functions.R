# From https://github.com/charlotte-ngs/rmdhelp/blob/master/R/misc_helper.R
# (MIT Licensed)
get_this_rmd_file <- function(){
  # return the current rmd file depending on usage mode
  return(ifelse(rstudioapi::isAvailable(),
                rstudioapi::getSourceEditorContext()$path,
                rprojroot::thisfile()))
}

tar_objects_defined_in_rmd <- function(filename) {
  read_lines(filename) |>
    # Find targets code blocks
    grep("^```\\{targets ", x = _, value = TRUE) |>
    # Parse out the names of those blocks
    sub("^[^ ]+ ([^,}]+).*", "\\1", x = _)
}

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


fia_state_clipped <- function(fiadb, statecd) {
  con <- DBI::dbConnect(RSQLite::SQLite(), fiadb, flags = RSQLite::SQLITE_RO)
  on.exit(DBI::dbDisconnect(con), add = TRUE, after = FALSE)
  
  state <- tbl(con, "REF_RESEARCH_STATION") |>
    filter(STATECD == statecd) |>
    distinct(STATECD, STATE_NAME, STATE_ABBR) |>
    collect()
  
  state_abbr <- state[["STATE_ABBR"]]
  state_data <- rFIA::readFIA(
    con = con,
    schema = "main",
    states = state_abbr, # state abbreviation, e.g. "MA", or c("MA", "CT")
    inMemory = TRUE # findEVALID doesn't work with inMemory = FALSE
  )
  
  state_name <- state[["STATE_NAME"]]
  evalids <- rFIA::findEVALID(
    state_data,
    mostRecent = FALSE,
    state = state_name, # full names of states of interest
    year = 1999:2024, # Evidence suggests this wants an INVYR, not MEASYEAR
    type = NULL #  ('ALL', 'CURR', 'VOL', 'GROW', 'MORT', 'REMV', 'CHANGE', 'DWM', 'REGEN')
  )
  
  # Clip to inventories of interest
  # This is necessary for biomass() to return anything other than just
  # the most recent year.
  rFIA::clipFIA(state_data, mostRecent = FALSE, evalid = evalids)
}


#' FIA Biomass for each plot in a state
#' 
#' Given an FIA state code and file path to a SQLite FIADB, returns
#' the biomass for all plots in the state for each inventory year from 1999:2024
#' Result columns are as described in https://doserlab.com/files/rfia/reference/biomass,
#' with the addition of:
#'
#' - BIO_HECTARE - BIO_ACRE converted to megagrams per hectare
#' - CARB_HECTARE - CARB_ACRE converted to megagrams per hectare
#'
#' @param fiadb character path to SQLite FIADB
#' @param statecd numeric FIA STATECD for state of interest
#'
#' @returns tibble of biomass for each plot in the state for each year
#' @export
#'
#' @examples
fia_biomass_for_state <- function(fiadb, statecd) {
  state_data_clipped <- fia_state_clipped(fiadb, statecd)
  
  # note that byPlot=TRUE causes the result's YEAR to be MEASYEAR
  rFIA::biomass(state_data_clipped, byPlot = TRUE) |>
    mutate(
      # BIO_ACRE: estimate of mean tree biomass per acre (short tons/acre)
      BIO_HECTARE = conv_multiunit(BIO_ACRE, "short_ton / acre", "Mg / hectare"),
      # CARB_ACRE: estimate of mean tree carbon per acre (short tons/acre)
      CARB_HECTARE = conv_multiunit(CARB_ACRE, "short_ton / acre", "Mg / hectare")
    )
}

fia_tpa_for_state <- function(fiadb, statecd) {
  state_data_clipped <- fia_state_clipped(fiadb, statecd)

  # note that byPlot=TRUE causes the result's YEAR to be MEASYEAR
  rFIA::tpa(state_data_clipped, byPlot = TRUE) |>
    mutate(
      # TPA - estimate of mean trees per acre
      # we are converting from 1/acre to 1/hectare, which is
      # the same as converting hectares to acres.
      TPH = conv_unit(TPA, "hectare", "acre"),
      # BAA - estimate of mean basal area (sq. ft.) per acre
      BAH = conv_multiunit(BAA, "ft2 / acre", "m2 / hectare")
    )
}
