#!/usr/bin/env Rscript

box::use(. / utils / cmr)
# box::reload(cmr)

start_date <- "2018-01-01"
end_date <- "2019-01-01"

site_info <- list(
  site_id = "losthills",
  lat = 35.5103,
  lon = -119.6675
)

collection_info <- readr::read_csv("./code_snippets/cmr-descriptions.csv")
is2 <- collection_info |>
  dplyr::filter(short_name == "ICESat2_L3A") |>
  dplyr::pull(concept_collection_id)

granules <- cmr$search_granules(
  collection_concept_id = is2,
  # temporal = c(start_date, end_date),
  point = with(site_info, c(lon, lat))
)

granule_sub <- granules |>
  dplyr::filter(
    link_type == "application/x-hdf5",
    stringr::str_starts(link_href, "https")
  )

urls <- granule_sub |>
  dplyr::pull(link_href) |>
  head(20)

outdir <- file.path("./external_data", "icesat-2")
cmr$download_parallel(urls, outdir)

is2files <- list.files(outdir, full.names = TRUE)
fname <- is2files[[1]]

read_beam <- function(hf, beam) {
  beam_hf <- hf[[beam]][["land_segments"]]
  lat <- beam_hf[["latitude"]][]
  lon <- beam_hf[["longitude"]][]
  hgt <- beam_hf[["canopy"]][["h_canopy"]][]
  tibble::tibble(beam = beam, lat = lat, lon = lon, hgt = hgt)
}

read_beams <- function(hf) {
  beams <- c("gt1l", "gt1r", "gt2l", "gt2r", "gt3l", "gt3r")
  purrr::map(beams, read_beam, hf = hf) |>
    purrr::list_rbind()
}

hf <- hdf5r::H5File$new(fname, mode = "r")
dat <- read_beams(hf)

dsub <- dat |>
  dplyr::filter(
    abs(site_info[["lat"]] - lat) < 0.5,
    abs(site_info[["lon"]] - lon) < 0.5
  )

library(ggplot2)

ggplot(dsub) +
  aes(x = lon, y = lat, color = hgt) +
  geom_point()
