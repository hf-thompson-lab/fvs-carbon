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
