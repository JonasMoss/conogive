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
#' @return An object of class `conogive`.
#' @examples
#' extraversion = psychTools::bfi[c("E1", "E2", "E3", "E4", "E5")]
#' extraversion[, "E1"] = 7 - extraversion[, "E1"] # Reverse-coded item.
#' extraversion[, "E2"] = 7 - extraversion[, "E2"] # Reverse-coded item.
#' fit = conogive(extraversion)
conogive = function(data, use = "complete.obs", ...) {

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

      class(object) = "conogive"
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

  class(object) = "conogive"
  object

}

#' @export
predict.conogive = function(object, newdata, weights = c("optimal", "equal"),
                                                         ...) {

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

#' Calculate the Ordinal Reliabiltiy
#'
#' The functions `ordinal_omega ` and `ordinal_alpha` calculate the ordinal
#'    reliability and the ordinal alpha. The functions `omega` and `alpha`
#'    calculates the theoretical reliability and alpha based on the polychoric
#'    correlation matrix.
#'
#' The population value of ordinal alpha equals the ordinal reliability when
#'    the underlying multivariate normal is parallel. The ordinal reliability
#'    is the sqaured correlation between the true latent variable and the
#'    best linear predictor of the observed Likert-type data.
#'
#' @param object An object of class `conogive`.
#' @param xi How to calculate the Xi matrix. Option `"theoretical"` calculates
#'    the theoretical Xi matrix from `rho`, while  `"sample"` calculates the
#'    sample Xi matrix..
#' @param weights The weights used to calculate the ordinal omega and omega.
#'    Option `"optimal"` uses the optimal weights, `"equal"` the equal weights,
#'    and `"sigma"` the sigma weights.
#' @export
#' @name reliability
#' @return The ordinal omega, ordinal alpha, theoretical omega, or theoretical
#'    alpha.
#' @examples
#' agreeableness = psychTools::bfi[c("A1", "A2", "A3", "A4", "A5")]
#' agreeableness[, "A1"] = 7 - agreeableness[, "A1"] # Reverse-coded item.
#' object = conogive(agreeableness)
#' ordinal_alpha(object) # 0.6267724
#' ordinal_omega(object, weights = "equal") # 0.6394087
#' alpha(object) # 0.7589922
#' omega(object, weights = "equal") # 0.7689878
#' ordinal_omega(object, weights = "optimal") # 0.6763742
#' omega(object) # 0.8101108

ordinal_omega = function(object, xi = c("sample", "theoretical"),
                        weights = c("optimal", "equal")) {

  weights = match.arg(weights)
  xi = match.arg(xi)

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

#' @rdname reliability
#' @export
ordinal_alpha = function(object, xi = c("sample", "theoretical")) {

  xi = match.arg(xi)

  cuts = object$cuts
  rho = object$rho

  alpha <- function(rho) {
    k <- nrow(rho)
    k / (k - 1) * (1 - tr(rho) / sum(rho))
  }

  xi = if(xi  == "sample") object$xi else xi_theoretical(cuts, rho)

  alpha(rho) * sum(xi) / sum(rho)
}

#' @rdname reliability
#' @export
omega = function(object, weights = c("optimal", "equal", "sigma")) {

  lambda = object$lambda
  sigma = object$sigma
  weights = match.arg(weights)
  k <- length(lambda)

  if(weights == "optimal") {
    a <- sum(lambda^2 / sigma^2)
  } else if(weights == "equal") {
    a <- k * mean(abs(lambda))^2 / mean(sigma^2)
  } else if(weights == "sigma") {
    a <- k * mean(abs(lambda) / sqrt(lambda^2 + sigma^2))^2 /
      mean(sigma^2 / (lambda^2 + sigma^2))
  }

  a / (a + 1)

}

#' @rdname reliability
#' @export
alpha = function(object) {
  rho = object$rho
  k <- nrow(rho)
  k / (k - 1) * (1 - tr(rho) / sum(rho))
}
