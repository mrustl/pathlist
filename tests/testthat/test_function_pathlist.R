test_that("Initialising a pathlist object works", {

  paths <- c("r/abc", "r/abc/d", "r/abc/e")
  pl <- pathlist::pathlist(paths = paths)

  testthat::expect_identical(pl@root, "r/abc")
  testthat::expect_identical(pl@folders, matrix(c("", "d", "e"), ncol = 1))
  testthat::expect_identical(pl@depths, c(0L, 1L, 1L))

  pathlist::pathlist(c("a", "a/b"))@depths
})

