test_that("weibull", {
  test_dist2("weibull")
})

test_that("weibull fails to converge", {
  withr::with_seed(97, {
    data <- data.frame(Conc = ssdtools::ssd_rweibull(1000))
  })
  skip_on_ci()
  expect_warning(
    fits <- ssdtools::ssd_fit_dists(data = data, dists = c("lnorm", "weibull")),
    "Distribution 'weibull' failed to converge \\(try rescaling data\\): ERROR: ABNORMAL_TERMINATION_IN_LNSRCH.")
})