test_that("Alignment for exact fit in central region works correctly", {
  skip_on_cran()
  query <- c(1,5,4,3,9,8,5,2,6,5,4)
  reference <- c(rnorm(5), query, rnorm(5))
  lambda <- rep(0, length(reference))
  maxshift <- 11
  res <- VPdtw(reference, query, lambda, maxshift)
  expect_equal(res$shift,  c(rep(NA, 5), rep(5, 11), rep(NA, 5)))
})

test_that("Alignment for exact fit on right side works correctly", {
  skip_on_cran()
  reference <- c(1,5,4,3,9,8,5,2,6,5,4)
  query <- c(rnorm(5), reference)
  reference <- c(reference, rnorm(5))
  lambda <- rep(0, length(reference))
  maxshift <- 10
  res <- VPdtw(reference, query, lambda, maxshift)
  expect_equal(res$shift, c(rep(-5, 16), rep(NA, 5)))
})

test_that("Alignment for exact fit on left side works correctly", {
  skip_on_cran()
  reference <- c(1,5,4,3,9,8,5,2,6,5,4)
  query <- c(reference, rnorm(5))
  reference <- c(rnorm(5), reference)
  lambda <- rep(0, length(reference))
  maxshift <- 10
  res <- VPdtw(reference, query, lambda, maxshift)
  plot(res)
  res$shift
  expect_equal(res$shift, c(rep(NA, 5), rep(5, 16)))
})

test_that("Alignment for exact fit in middle with left-drop", {
  skip_on_cran()
  reference <- c(1,5,4,3,9,8,5,2,6,5,4)
  query <- c(reference[1:5], 20, reference[6:11])
  reference <- c(rnorm(5), reference)
  query <- c(query, rnorm(5))
  lambda <- rep(0, length(reference))
  maxshift <- 10
  res <- VPdtw(reference, query, lambda, maxshift)
  expect_equal(res$shift, c(rep(NA, 5), rep(5, 5),rep(4, 11)))
})

### Visual tests ###

test_that("Alignment plots look as expected", {
  skip_on_cran()
  skip_if_not_installed("vdiffr")
  
  data(reference)
  data(query)
  
  reference <- log(reference)
  query <- log(query)
  
  result <- VPdtw(reference=reference[1:2500], query = query[1:2500],
                  penalty = dilation(reference[1:2500], 150)/4, 
                  maxshift = 150)
  
  alignment1_all <- function(){plot(result, type = "All")}
  vdiffr::expect_doppelganger("Alignment1 - 3 panel", alignment1_all)
  
  alignment1_b <- function() {plot(result, type = "Before")}
  vdiffr::expect_doppelganger("Alignment1 - Before", alignment1_b)
  
  alignment1_a <- function() {plot(result, type = "After")}
  vdiffr::expect_doppelganger("Alignment1 - After", alignment1_a)
  
  alignment1_s <- function() {plot(result, type = "Shift")}
  vdiffr::expect_doppelganger("Alignment1 - Shift", alignment1_s)
  
  alignment1_c <- function() {plot(result, type = "Chromatograms")}
  vdiffr::expect_doppelganger("Alignment1 - Chromatograms", alignment1_c)
})

test_that("Zero-penalty alignment plots look as expected", {
  skip_on_cran()
  skip_if_not_installed("vdiffr")
  
  data(reference)
  data(query)
  
  reference <- log(reference)
  query <- log(query)
  
  result <- suppressWarnings(VPdtw(reference = reference[1:2500], 
                                   query = query[1:2500], 
                                   penalty = rep(0, length(reference)), 
                                   maxshift = 150))

  alignment2_all <- function() {plot(result, type = "All")}
  vdiffr::expect_doppelganger("Alignment2 - 3 panel", alignment2_all)
  
  alignment2_b <- function() {plot(result, type = "Before")}
  vdiffr::expect_doppelganger("Alignment2 - Before", alignment2_b)
  
  alignment2_a <- function() {plot(result, type = "After")}
  vdiffr::expect_doppelganger("Alignment2 - After", alignment2_a)
  
  alignment2_s <- function() {plot(result, type = "Shift")}
  vdiffr::expect_doppelganger("Alignment2 - Shift", alignment2_s)
  
  alignment2_c <- function() {plot(result, type = "Chromatograms")}
  vdiffr::expect_doppelganger("Alignment2 - Chromatograms", alignment2_c)
})