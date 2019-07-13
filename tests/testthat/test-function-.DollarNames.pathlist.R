test_that(".DollarNames.pathlist() works", {

  f <- pathlist:::.DollarNames.pathlist
  pl <- pathlist::pathlist(c("a/bar", "a/beer", "a/bear"))
  expect_error(f())
  expect_identical(f(pl, "b"), c("bar", "beer", "bear"))
  expect_identical(f(pl, "c"), character(0))
})
