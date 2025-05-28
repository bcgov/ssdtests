test_that("rescale data ssddata", {
  data_sets <- ssddata::ssd_data_sets()
  results <- data.frame(item = names(data_sets))
  
  fit_dists <- function(data, dists, rescale) {
    fit <- suppressWarnings(ssdtools::ssd_fit_dists(data = data, dists = dists, rescale = rescale))
  }
  
  for(i in seq_along(data_sets)) {
    set.seed(42)
    dists <- c("burrIII3", "gamma", "gompertz", "lgumbel", "llogis", "llogis_llogis", 
               "lnorm", "lnorm_lnorm", "weibull")
    rescale_fit <- fit_dists(data = data_sets[[i]], dists = dists, rescale = TRUE)
    unscale_fit <- fit_dists(data = data_sets[[i]], dists = dists, rescale = FALSE)
    
    rescale_dists <- names(rescale_fit)
    unscale_dists <- names(unscale_fit)
    
    results$rescale_n[i] <- length(rescale_dists)
    results$unscale_n[i] <- length(unscale_dists)
    results$rescale_dists[[i]] <- rescale_dists
    results$unscale_dists[[i]] <- unscale_dists
  }
  expect_identical(length(dists), 9L)
  expect_identical(nrow(results), 20L)
  expect_identical(sum(results$rescale_n), 165L)
  expect_identical(sum(results$unscale_n), 161L)
  expect_snapshot_data(results, "rescale")
})

test_that("rescale envirotox acute", {
  dists <- ssdtools::ssd_dists_bcanz()
  
  data <- envirotox::envirotox_acute |>
    dplyr::nest_by(Chemical) |>
    dplyr::mutate(ssd_fit_unscale = list(ssdtools::ssd_fit_dists(data, dists = dists, silent = TRUE)),
                  ssd_fit_rescale = list(ssdtools::ssd_fit_dists(data, dists = dists, rescale = TRUE, silent = TRUE))) |>
    dplyr::mutate(dists_unscale = list(names(ssd_fit_unscale)),
           dists_rescale = list(names(ssd_fit_rescale)),
           n_unscale = length(dists_unscale),
           n_rescale = length(dists_rescale),
           max = length(dists))

  # do by distribution
  expect_equal(nrow(data), 729L)
  expect_equal(sum(data$n_unscale) / sum(data$max), 0.884, tolerance = 0.01)
  expect_equal(sum(data$n_rescale) / sum(data$max), 0.931, tolerance = 0.01)
})
