test_that("Initialising a pathlist object works", {

  paths <- c("r/abc", "r/abc/d", "r/abc/e")
  pl <- pathlist::pathlist(paths = paths)

  testthat::expect_identical(pl@root, "r")
  testthat::expect_identical(pl@folders[, 1], c("abc", "abc", "abc"))
  testthat::expect_identical(pl@folders[, 2], c("", "d", "e"))
  testthat::expect_identical(pl@depths, c(1L, 2L, 2L))
})
