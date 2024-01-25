test_that("make_age_gender_grid() works for minimal example", {
  expect_equal(
    make_age_gender_grid(2),
    data.table::data.table(
      expand.grid(age = seq(0,1),
                  alter_age_strata = c("0-4", "5-9", "10-14", "15-19", "20-24", "25-34", "35-44",
                                       "45-54", "55-64", "65-69", "70-74", "75-79", "80-84"),
                  gender = c("Male", "Female"),
                  alter_gender = c("Male", "Female"))
    )
  )
})
