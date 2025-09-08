test_that("ssd_hc passing all boots ccme_chloride lnorm_lnorm", {
  fits <- ssd_fit_dists(ssddata::ccme_chloride,
    min_pmix = 0.0001, at_boundary_ok = TRUE,
    dists = c("lnorm_lnorm", "llogis_llogis")
  )

  withr::with_seed(102, {
    expect_warning(hc <- ssd_hc(fits, ci = TRUE, nboot = 1000, average = FALSE))
  })
  expect_s3_class(hc, "tbl_df")
  expect_snapshot_boot_data(hc, "hc_cis_chloride50")
})

test_that("ssd_hc cis with error and multiple dists", {
  withr::with_seed(99, {
    conc <- ssd_rlnorm_lnorm(30, meanlog1 = 0, meanlog2 = 1, sdlog1 = 1 / 10, sdlog2 = 1 / 10, pmix = 0.2)
  })
  data <- data.frame(Conc = conc)
  fit <- ssd_fit_dists(data, dists = c("lnorm", "llogis_llogis"), min_pmix = 0.1)
  expect_identical(attr(fit, "min_pmix"), 0.1)
  skip_on_ci()
  withr::with_seed(99, {
    expect_warning(hc_err_two <- ssd_hc(fit, ci = TRUE, nboot = 100, average = FALSE, delta = 100))
  })
  expect_snapshot_boot_data(hc_err_two, "hc_err_two")
  withr::with_seed(99, {
    expect_warning(hc_err_avg <- ssd_hc(fit,
      ci = TRUE, nboot = 100,
      delta = 100, ci_method = "MACL"
    ))
  })
  expect_snapshot_boot_data(hc_err_avg, "hc_err_avg")
})

test_that("ssd_hc cis with error and multiple dists", {
  withr::with_seed(99, {
    conc <- ssd_rlnorm_lnorm(30, meanlog1 = 0, meanlog2 = 1, sdlog1 = 1 / 10, sdlog2 = 1 / 10, pmix = 0.2)
  })
  data <- data.frame(Conc = conc)
  fit <- ssd_fit_dists(data, dists = c("lnorm", "llogis_llogis"), min_pmix = 0.1)
  expect_identical(attr(fit, "min_pmix"), 0.1)
  skip_on_ci()
  withr::with_seed(99, {
    expect_warning(hc_err_two <- ssd_hc(fit, ci = TRUE, nboot = 100, average = FALSE, delta = 100))
  })
  expect_snapshot_boot_data(hc_err_two, "hc_err_two")
  withr::with_seed(99, {
    expect_warning(hc_err_avg <- ssd_hc(fit,
      ci = TRUE, nboot = 100,
      delta = 100, ci_method = "MACL"
    ))
  })
  expect_snapshot_boot_data(hc_err_avg, "hc_err_avg")
})
