test_that("fill_missing_child_ages() basic example works", {
  dt_test <- data.table(new_id = c(1, 2, 3, 4),
                        age = c(2, NA, NA, 5),
                        age_strata = c("0-4", "5-9", "5-9", "5-9"))

  dt_answer <- data.table(new_id = c(1, 2, 3, 4),
                          age = c(2, NA, NA, 5),
                          age_strata = c("0-4", "5-9", "5-9", "5-9"),
                          imp_age = c(2, 4, 4, 5))

  expect_equal(fill_missing_child_ages(dt_test, seed = 1527), dt_answer)
})
