
test_that("ssd_run_scenario.data.frame all", {
  ssdsims::with_lecuyer_cmrg_seed(10, {
    scenario <- ssd_run_scenario(ssddata::ccme_boron, nrow = c(6L, 10L), replace = c(FALSE, TRUE), dists = c("lnorm", "gamma"), rescale = c(FALSE, TRUE), computable = c(FALSE, TRUE), at_boundary_ok = c(TRUE, FALSE), min_pmix = list(ssdtools::ssd_min_pmix, function(n) return(0.5)), range_shape1 = list(c(0.05, 20), c(0.01, 30)), range_shape2 = list(c(0.05, 20)), ci = FALSE, nboot = c(1, 2), est_method = c("arithmetic", "geometric", "multi"), ci_method = "weighted_samples", parametric = c(TRUE, FALSE), nsim = 2)
  })
  expect_snapshot_data(scenario, "scenarioall", delist = TRUE)
})
