#' Estimate a Latent Congeneric Model
#'
#' This function uses `psych::polychoric` and `psych::fa` to do a barebones
#'   multivariate ogive model. It is similar to `psych::irt.fa`. The `...`
#'   arguments are passed to `psych::fa`, which is called with `fm = "ml"` by
#'   default.
#'
#' @param data A data frame of observations or a named list with elements
#'    `lambda`, `sigma`, and `cuts`. See the details..
#' @param use Passed to `stats::cov`; defaults to `"complete.obs"`.
#' @param ... Passed to `psych::fa`, where `fm = "ml"` by default.
#' @export
#' @return An object of class `latcon`.
#' @examples
#' extraversion = psychTools::bfi[c("E1", "E2", "E3", "E4", "E5")]
#' extraversion[, "E1"] = 7 - extraversion[, "E1"] # Reverse-coded item.
#' fit = latcon(extraversion)
latcon = function(data, use = "complete.obs", ...) {

  if(is.list(data)) {

    if(all(c("lambda", "sigma", "cuts") %in% names(data))) {
      checkmate::assertNumeric(data$lambda)
      k = length(data$lambda)
      checkmate::assertNumeric(data$sigma, len = k)
      cuts = massage_cuts(data$cuts, k)
      checkmate::assertList(cuts, len = k)

      lambda = standardize_lambda(data$lambda, data$sigma)
      sigma = standardize_sigma(data$lambda, data$sigma)
      rho = tcrossprod(lambda, lambda) + diag(sigma^2)

      object = list(rho = rho,
                    cuts = cuts,
                    lambda = lambda,
                    sigma = sigma,
                    xi_sample = xi_theoretical(cuts, rho),
                    n = Inf)

      class(object) = "latcon"
      return(object)
    }

  }

  args = list(...)
  if(is.null(args$fm)) args$fm = "ml"

  poly = psych::polychoric(data)
  fa = do.call(what = psych::fa, args = c(list(r = poly$rho), args))
  lambda = stats::setNames(c(fa$loadings), colnames(data))
  sigma = c(sqrt(fa$uniquenesses))
  xi = xi_sample(y = ordered_y(data), cuts = poly$tau, use = use)

  object = list(rho = poly$rho, cuts = poly$tau, lambda = lambda, sigma = sigma,
                xi_sample = xi, n = nrow(data))

  class(object) = "latcon"
  object

}

predict.latcon = function(object, newdata, weights = c("optimal", "equal")) {

  weights = match.arg(weights)
  lambda = object$lambda
  sigma = object$sigma

  if(is.null(dim(newdata))) dim(newdata) = c(1, length(newdata))

  names = rownames(newdata)
  newdata = ordered_y(newdata)

  k = ncol(newdata)
  cuts = massage_cuts(object$cuts)
  mat = sapply(seq.int(k), function(i) x_hat(newdata[, i], cuts[[i]]))

  v = if (weights == "optimal") thurstone(lambda, sigma) else
    mean(lambda) / (k * mean(lambda)^2 + mean(sigma^2)) * rep(1, k)

  stats::setNames(c(mat %*% v), rownames(names))

}

#' Calculate the Ordinal Alpha
#'
#' @param object An object of class `latcon`.
#' @param xi One of `"theoretical"` and `"sample"`, defaults to `"sample"`.
#' @param weights One of `"optimal"` and `"equal"`, defaults to `"optimal"`.
#' @param limit If `TRUE`, calculates the limit ordinal omega.
#' @export
#' @return The ordinal omega.

ordinal_omega = function(object, xi = c("sample", "theoretical"),
                        weights = c("optimal", "equal"), limit = FALSE) {

  weights = match.arg(weights)
  xi = match.arg(xi)

  if (limit) {

    lambda = object$lambda
    sigma = object$sigma

    a = if(weights == "optimal") sum(lambda^2 / sigma^2) else
      length(lambda) * mean(abs(lambda))^2 / mean(sigma^2)
    return(a / (a + 1))

  }

  cuts = object$cuts
  rho = object$rho
  v = thurstone(object$lambda, object$sigma)
  xi = if(xi  == "sample") object$xi else xi_theoretical(cuts, rho)

  if(weights == "optimal") {
    c(crossprod(v, xi %*% v))
  } else {
    i = rep(1, nrow(rho))
    c(crossprod(i, xi %*% v))^2/sum(xi)
  }

}


reliability = function(object, weights = c("optimal", "equal", "std", "sigma")) {
  lambda = object$lambda
  sigma = object$sigma
  weights = match.arg(weights)
  k <- length(lambda)
  if(weights == "optimal") a <- sum(lambda^2 / sigma^2)
  if(weights == "equal") a <- k * mean(abs(lambda))^2 / mean(sigma^2)
  if(weights == "std") a <- k * mean(abs(lambda) / sigma)^2
  if(weights == "sigma") a <- k * mean(abs(lambda) / sqrt(lambda^2 + sigma^2))^2 / mean(sigma^2 / (lambda^2 + sigma^2))
  a / (a + 1)
}
