#' Make pretty strings.
#' @param x Numeric matrix.
#' @param digits Digits to prettify with.
#' @param nsmall Smallest number of digits to keep.
#' @return Pretty version of the digits.

prettify = function(x, digits = 3, nsmall = 3) {
  y = sapply(x, function(x) format(signif(round(x, digits = digits), digits),
                                   nsmall = nsmall))
  dim(y) = dim(x)
  y
}
