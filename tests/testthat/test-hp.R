test_that("ssd_hp cis with error and multiple dists", {
  withr::with_seed(99, {
    conc <- ssd_rlnorm_lnorm(30, meanlog1 = 0, meanlog2 = 1, sdlog1 = 1 / 10, sdlog2 = 1 / 10, pmix = 0.2)
  })
  data <- data.frame(Conc = conc)
  fit <- ssd_fit_dists(data, dists = c("lnorm", "llogis_llogis"), min_pmix = 0.1)
  expect_identical(attr(fit, "min_pmix"), 0.1)
  skip_on_ci()
  withr::with_seed(99, {
    expect_warning(hp_err_two <- ssd_hp(fit,
                                        conc = 1, ci = TRUE, nboot = 100, average = FALSE,
                                        delta = 100, proportion = FALSE
    ))
  })
  expect_snapshot_boot_data(hp_err_two, "hp_err_two")
  withr::with_seed(99, {
    expect_warning(hp_err_avg <- ssd_hp(fit,
                                        conc = 1, ci = TRUE, nboot = 100,
                                        delta = 100, ci_method = "MACL", proportion = FALSE))
  })
  expect_snapshot_boot_data(hp_err_avg, "hp_err_avg")
})
