# Download SMAP L4 data from NASA EarthData.
#
# NOTE: This uses `box` (https://klmr.me/box/) for module management
box::use(./utils/cmr)

start_date <- "2018-01-01"
end_date <- "2019-01-01"

site_info <- list(
  site_id = "losthills",
  lat = 35.5103,
  lon = -119.6675,
  start_date = "1999-01-01",
  end_date = "2012-12-31"
)

search <- cmr$search_granules(
  "C3480440870-NSIDC_CPRD",
  point = c(site_info[["lon"]], site_info[["lat"]]),
  temporal = c(start_date, end_date)
)

urls <- search |>
  dplyr::filter(
    stringr::str_starts(link_title, "Download"),
    stringr::str_ends(link_href, ".h5")
  ) |>
  dplyr::pull(link_href) |>
  head(10)

outdir <- "data-raw"
resps <- cmr$download_parallel(urls, outdir)
