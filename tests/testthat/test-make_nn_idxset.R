test_that("make_nn_idxset() generates the correct index set", {
  expect_equal(
    make_nn_idxset(3),
    matrix(c(3,1,4,1,5,1,2,2,3,2,4,2,1,3,2,3,3,3),
           ncol = 2,
           byrow = T)
  )
})


