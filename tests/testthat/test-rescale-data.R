test_that("rescale data ssddata", {
  data_sets <- ssd_data_sets()
  dists <- ssdtools::ssd_dists_all()
  
  fit_dists <- function(data, dists, rescale) {
    ssdtools::ssd_fit_dists(data = data, dists = dists, rescale = rescale, computable = TRUE, at_boundary_ok = FALSE, silent = TRUE)
  }
  
  data <- envirotox::envirotox_acute |>
    dplyr::nest_by(Chemical) |>
    dplyr::mutate(ssd_fit_unscale = list(fit_dists(.data$data, dists = dists, rescale = FALSE)),
                  ssd_fit_rescale = list(fit_dists(.data$data, dists = dists, rescale = TRUE))) |>
    dplyr::mutate(dists_unscale = list(names(ssd_fit_unscale)),
                  dists_rescale = list(names(ssd_fit_rescale))) |>
    dplyr::select(!c(ssd_fit_unscale, ssd_fit_rescale))
  
  unscaled <- data |>
    dplyr::select(Chemical, Distribution = dists_unscale) |>
    tidyr::unnest(Distribution) |>
    dplyr::ungroup() |>
    dplyr::count(Distribution) |>
    dplyr::mutate(n = n / nrow(data) * 100) |>
    dplyr::select(Distribution, Unscaled = n)
  
  rescaled <- data |>
    dplyr::select(Chemical, Distribution = dists_rescale) |>
    tidyr::unnest(Distribution) |>
    dplyr::ungroup() |>
    dplyr::count(Distribution) |>
    dplyr::mutate(n = n / nrow(data) * 100) |>
    dplyr::select(Distribution, Rescaled = n)
  
  results <- unscaled |>
    dplyr::inner_join(rescaled, by = "Distribution")
  
  expect_snapshot_data(results, "rescale")
})
