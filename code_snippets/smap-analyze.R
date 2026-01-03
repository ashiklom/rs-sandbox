# Read SMAP L4 data, build a `stars` spatiotemporal data cube, crop to the
# boundaries of CA, and plot.
#
# NOTE: This uses `box` (https://klmr.me/box/) for module management
box::use(./utils/smap)

# Shapefile for California boundary
ca_shapefile <- "/mnt/c/Users/alexe/Downloads/pecan/ca_state/CA_State.shp"

outdir <- "data-raw"
smapfiles <- list.files(outdir, full.names = TRUE)
smap_list <- purrr::map(smapfiles, smap$smap_raster)

california <- terra::vect(ca_shapefile) |>
  terra::project(smap_list[[1]][["raster"]])

smap_cali_rast <- smap_list |>
  purrr::map("raster") |>
  purrr::map(terra::crop, y = california, mask = TRUE)

sm_times <- purrr::map_vec(smap_list, "time")
sm_list <- purrr::map(smap_cali_rast, ~ .x[["sm_rootzone"]]) |>
  purrr::map(stars::st_as_stars, proxy = TRUE)
sm <- do.call(c, c(sm_list, along = "time"))
stars::st_dimensions(sm)[["time"]][["values"]] <- sm_times

plot(sm)
