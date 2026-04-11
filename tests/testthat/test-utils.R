test_that("message_once only emits a message once until reset", {
  reset_message_once()
  on.exit(reset_message_once(), add = TRUE)

  expect_message(
    message_once("utils-test-message"),
    "utils-test-message"
  )

  expect_silent(
    message_once("utils-test-message")
  )

  expect_null(
    reset_message_once()
  )

  expect_message(
    message_once("utils-test-message"),
    "utils-test-message"
  )
})
