agreeableness = psychTools::bfi[1:100, c("A1", "A2", "A3", "A4", "A5")]
agreeableness[, "A1"] = 7 - agreeableness[, "A1"] # Reverse-coded item.
object = suppressWarnings(conogive(agreeableness))

test_that("congive works on data theoretical input.", {
  object$cuts = qnorm(seq(0, 1, length.out = 6 + 1))
  obj = conogive(object, weights = "optimal")
  expect_equal(obj$lambda, object$lambda)
})

test_that("predict for optimal is not the same as predict for equal", {
  expect_true(predict(object, agreeableness[1, ]) !=
              predict(object, agreeableness[1, ], weights = "equal"))
})

test_that("predict for vector same as for array", {
  expect_equal(predict(object, as.numeric(agreeableness[1, ])),
               predict(object, agreeableness[1, ]))
})

test_that("omega equal smaller than omega optimal", {
  expect_gt(ordinal_omega(object), ordinal_omega(object, weights = "equal"))
  expect_gt(omega(object), omega(object, weights = "equal"))
})

test_that("omega sigma smaller than omega optimal", {
  expect_gt(ordinal_omega(object), ordinal_omega(object, weights = "sigma"))
  expect_gt(omega(object), omega(object, weights = "sigma"))
})

test_that("omega optimal default", {
  expect_equal(ordinal_omega(object, weights = "optimal"),
               ordinal_omega(object))
  expect_equal(omega(object, weights = "optimal"),
               omega(object))
})

test_that("ordinal alpha smaller than alpha", {
  expect_lt(ordinal_alpha(object), alpha(object))
  expect_lt(ordinal_alpha(object, xi = "theoretical"),alpha(object))
})
