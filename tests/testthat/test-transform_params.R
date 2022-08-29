y = psych::bfi[, 1:5]
obj = lavaan::cfa(
  model = "z =~ A1 + A4 + A3 + A2 + A5",
  data = y,
  ordered = TRUE,
  std.lv = TRUE)

result = transform_params(obj)

expect_equal(length(result), 2)
expect_equal(length(result$tau),5)
expect_equal(names(result$tau), c("A1", "A4", "A3", "A2", "A5"))
