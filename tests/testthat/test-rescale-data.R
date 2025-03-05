test_that("rescale data", {
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
