# Copyright 2023 Environment and Climate Change Canada
#
#    Licensed under the Apache License, Version 2.0 (the "License");
#    you may not use this file except in compliance with the License.
#    You may obtain a copy of the License at
#
#       https://www.apache.org/licenses/LICENSE-2.0
#
#    Unless required by applicable law or agreed to in writing, software
#    distributed under the License is distributed on an "AS IS" BASIS,
#    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#    See the License for the specific language governing permissions and
#    limitations under the License.

save_png <- function(x, width = 400, height = 400) {
  path <- tempfile(fileext = ".png")
  grDevices::png(path, width = width, height = height)
  on.exit(grDevices::dev.off())
  print(x)
  
  path
}

save_csv <- function(x) {
  path <- tempfile(fileext = ".csv")
  readr::write_csv(x, path)
  path
}

expect_snapshot_plot <- function(x, name) {
  
  path <- save_png(x)
  testthat::expect_snapshot_file(path, paste0(name, ".png"))
}

expect_snapshot_boot_data <- function(x, name, digits = 6, min_pboot = 0.9, max_pboot = 1) {
  if (!is.na(min_pboot) && min_pboot > 0) {
    testthat::expect_true(all(x$pboot >= min_pboot))
  }
  if (!is.na(min_pboot) && max_pboot < 1) {
    testthat::expect_true(all(x$pboot <= max_pboot))
  }
  x$pboot <- NULL
  expect_snapshot_data(x, name, digits = digits)
}

expect_snapshot_data <- function(x, name, digits = 6) {
  fun <- function(x) {
    if(!is.double(x))
      return(x)
    signif(x, digits = digits)
  }
  lapply_fun <- function(x) I(lapply(x, fun))
  x <- dplyr::mutate(x, dplyr::across(dplyr::where(is.numeric), fun))
  x <- dplyr::mutate(x, dplyr::across(dplyr::where(is.list), lapply_fun))
  path <- save_csv(x)
  testthat::expect_snapshot_file(
    path,
    paste0(name, ".csv"),
    compare = testthat::compare_file_text
  )
}
