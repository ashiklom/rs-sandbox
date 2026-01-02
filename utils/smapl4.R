#' @export
smap_raster <- function(fname) {
  box::use(hdf5r, terra) # nolint
  hf <- hdf5r::H5File$new(fname, mode = "r")
  crs_attrs <- hdf5r::h5attributes(hf[["EASE2_global_projection"]])
  xy_unit <- hdf5r::h5attr(hf[["x"]], "units")
  crs_proj4 <- glue::glue(
    "+proj=cea ",
    "+lat_ts={crs_attrs[['standard_parallel']]} ",
    "+lon_0={crs_attrs[['false_easting']]} ",
    "+lat_0={crs_attrs[['false_northing']]} ",
    "+x_0=0 +y_0=0 +datum=WGS84 ",
    "+units={xy_unit} ",
    "+no_defs"
  )
  x <- hf[["x"]][]
  y <- hf[["y"]][]
  time_unit <- hdf5r::h5attr(hf[["time"]], "units")
  basetime <- gsub("seconds since ", "", time_unit) |>
    lubridate::ymd_hms()
  time_raw <- hf[["time"]][]
  time <- basetime + lubridate::duration(time_raw, "seconds")
  hf$close()
  raster <- terra::rast(fname, guessCRS = FALSE, noflip = TRUE)
  terra::crs(raster) <- crs_proj4
  dx <- mean(diff(x))
  dy <- mean(diff(y))
  terra::ext(raster) <- terra::ext(
    min(x) - dx / 2,
    max(x) + dx / 2,
    min(y) - dy / 2,
    max(y) + dy / 2
  )
  return(list(raster = raster, time = time))
}
