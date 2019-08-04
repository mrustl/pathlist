test_that("Hiding the server name and hidden folders works", {

  expect_identical(
    hide_server_("//my_server/hidden_folder$/abc"),
    "//server/hidden_folder/abc"
  )
})
