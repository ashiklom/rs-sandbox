#' Search for "granules" (files) using NASA CMR
#'
#' This requires knowing the `collection_concept_id` of the dataset in advance.
#' An easy way to do this is to identify it interactively at
#' `search.earthdata.nasa.gov` and then copy it here.
#'
#' @author Alexey Shiklomanov
#' @param collection_concept_id (character) Unique identifier of the collection.
#' @param ... Additional CMR granules query options. See
#'  https://cmr.earthdata.nasa.gov/search/site/docs/search/api.html#granule-search-by-parameters
#' @return `tibble::tibble` of granule metadata. Note that each granule will
#' have multiple links associated with it, and some of these may be `s3` (not
#' HTTPS) URLs, OpenDAP endpoints, or other things you may not want, so you may
#' have to post-process the data frame before downloading.
#'
#' @examples
#' search <- search_granules(
#'   "C3480440870-NSIDC_CPRD",  # SMAP L4 geophysical variables
#'   point = c(-119.6675, 35.51),  # lon, lat
#'   temporal = c("2018-01-01", "2019-01-01")  # start, end
#' )
#' @export
search_granules <- function(collection_concept_id, ...) {
  box::use(httr2) #nolint
  cmr_base <- "https://cmr.earthdata.nasa.gov/search"
  cmr_granule <- paste0(cmr_base, "/granules.json")
  req <- httr2::request(cmr_granule) |>
    httr2::req_url_query(
      .multi = "comma",
      collection_concept_id = collection_concept_id,
      ...
    )
  # Paginated response
  resp <- httr2::req_perform_iterative(
    req,
    httr2::iterate_with_offset("page_num")
  )
  tidy_granules(resp)
}

#' @export
tidy_links <- function(granule) {
  box::use(purrr) #nolint
  granule[["links"]] |>
    # NOTE: This check is inconsistent. Not all valid links have a `title`
    # field (e.g., ICESat-2 ATL07 doesn't). So just return all links and let
    # the users sort it out.
    # purrr::keep(~ ("title" %in% names(.x))) |>
    purrr::map(tibble::as_tibble_row) |>
    purrr::list_rbind() |>
    dplyr::rename_with(~paste0("link_", .x))
}

#' @export
tidy_granule <- function(granule) {
  box::use(dplyr) #nolint
  metadata <- granule[names(granule) != "links"]
  link_df <- tidy_links(granule)
  dplyr::bind_cols(tibble::as_tibble_row(metadata), link_df)
}

#' @export
tidy_granules <- function(resp) {
  box::use(httr2, purrr, dplyr) #nolint
  httr2::resps_data(resp, httr2::resp_body_json) |>
    purrr::keep(~length(.x$entry) > 0) |>
    purrr::map("entry") |>
    # Drop the `feed` piece
    purrr::list_c() |>
    purrr::map(tidy_granule) |>
    dplyr::bind_rows()
}

#' Download list of URLs in parallel
#'
#' This uses `httr2::req_perform_parallel` to execute a bunch of HTTP downloads
#' in parallel. It assumes you have a properly configured `~/.netrc` file with
#' your NASA EarthData Login credentials (if downloading from NASA EarthData).
#' It automatically handles shared cookies through a temporary cookie jar file.
#'
#' @param urls (character) URLs to download.
#' @param outdir (character) Destination directory. Will be created if it
#'  doesn't exist.
#' @param skip_existing (boolean) If `TRUE`, skip files that already exist.
#'
#' @return List of `httr2` response objects, or `NULL` (if there are no files
#' to download).
#' @export
download_parallel <- function(urls, outdir, skip_existing = TRUE) {
  cookie_jar <- tempfile("cookies-", fileext = ".txt")
  dir.create(outdir, showWarnings = FALSE)
  outfiles <- file.path(outdir, basename(urls))
  if (skip_existing) {
    file_exists <- file.exists(outfiles)
    urls <- urls[!file_exists]
    outfiles <- outfiles[!file_exists]
  }
  if (length(urls) == 0) {
    message("No files to download (possibly because they all exist)")
    return(NULL)
  }
  reqs <- urls |>
    purrr::map(httr2::request) |>
    purrr::map(httr2::req_options, netrc = TRUE) |>
    purrr::map(httr2::req_cookie_preserve, path = cookie_jar)
  resps <- httr2::req_perform_parallel(reqs, paths = outfiles)
  return(resps)
}
