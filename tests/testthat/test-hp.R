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
      delta = 100, ci_method = "MACL", proportion = FALSE
    ))
  })
  expect_snapshot_boot_data(hp_err_avg, "hp_err_avg")
})

test_that("ssd_hp calculates cis in parallel but one distribution", {
  local_multisession()
  data <- ssddata::ccme_boron
  fits <- ssd_fit_dists(data, dists = "lnorm")
  withr::with_seed(10, {
    hp <- ssd_hp(fits, 1, ci = TRUE, nboot = 10, ci_method = "MACL", proportion = FALSE)
  })
  expect_snapshot_value(hp$se, style = "deparse")
})

test_that("ssd_hp calculates cis in parallel with two distributions", {
  local_multisession()
  data <- ssddata::ccme_boron
  fits <- ssd_fit_dists(data, dists = c("lnorm", "llogis"))
  withr::with_seed(10, {
    hp <- ssd_hp(fits, 1, ci = TRUE, nboot = 10, ci_method = "MACL", proportion = FALSE)
  })
  expect_snapshot_value(hp$se, style = "deparse")
})

test_that("hp est_method and ci_method combos", {
  fit1 <- ssd_fit_dists(ssddata::ccme_boron, dists = "lnorm")
  fit2 <- ssd_fit_dists(ssddata::ccme_boron, dists = c("lnorm", "llogis"))
  fits <- list(fit1, fit2)
  
  est_methods <- ssd_est_methods()
  ci_methods <- ssd_ci_methods()
  parametric <- c(TRUE, FALSE)
  ci <- c(FALSE, TRUE)
  average <- c(TRUE, FALSE)
  
  data <- tidyr::expand_grid(fit = fits, average = average, est_method = est_methods, ci = ci, parametric = parametric, ci_method = ci_methods)
  data$seed <- 10
  data$id <- seq_len(nrow(data))
  
  func <- function(fit, average, est_method, ci_method, parametric, ci, seed, id) {
    withr::with_seed(seed, {
      hp <- ssd_hp(fit, average = average, est_method = est_method, ci_method = ci_method, parametric = parametric, ci = ci, nboot = 10, proportion = TRUE)
    })
    expect_s3_class(hp, "tbl")
    hp$id <- id
    hp
  }
  ls <- purrr::pmap(data, .f = func)
  
  ls <- dplyr::bind_rows(ls)
  data <- dplyr::rename(data, ci_method_arg = "ci_method", est_method_arg = "est_method")
  data <- dplyr::inner_join(data, ls, by = "id")
  data$fit <- NULL
  expect_snapshot_data(data, "all_hp_combos")
})
