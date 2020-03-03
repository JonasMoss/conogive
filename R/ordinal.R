#' Theoretical ordinal reliability
#'
#' @param lambda Vector of loadings.
#' @param sigma Vector of standard deviations.
#' @param cuts List of cuts, matrix of cuts, or vector of cuts that will be
#'    repeated.
#' @param type One of `"H"` and `"std"`, defaults to `"H"`.
#' @return The population ordinal reliabiltiy
#' @name ordinal_omega
#' @export

ordinal_omega_ = function(lambda, sigma, cuts, type = c("H", "std")) {

  checkmate::assertNumeric(lambda)
  k = length(lambda)
  checkmate::assertNumeric(sigma, len = k)
  cuts = massage_cuts(cuts, k)
  checkmate::assertList(cuts, len = k)
  type = match.arg(type)

  lambda = standardize_lambda(lambda, sigma)
  sigma = standardize_sigma(lambda, sigma)
  rho = tcrossprod(lambda, lambda) + diag(sigma^2)
  v = thurstone(lambda, sigma)

  xi = xi_theoretical(cuts, rho)

  if(type == "H") {
    c(crossprod(v, xi %*% v))
  } else {
    i = rep(1, k)
    c(crossprod(i, xi %*% v))^2/sum(xi)
  }

}

#' Calculating the Empirical Ordinal Alpha
#'
#' Use `ordinal_alpha` to calculate the empricial ordinal alpha from a `latcon`
#'    object.
#'
#' The population value of ordinal alpha equals the ordinal reliability when
#'    the underlying multivariate normal is parallel. The ordinal reliability
#'    is the sqaured correlation between the true latent variable and the
#'    best linear predictor of the observed Likert-type data.
#'
#' @param object A an object of class `latcon`.
#' @export
#' @return The empirical ordinal alpha.
#' @examples
#' agreeableness = psychTools::bfi[c("A1", "A2", "A3", "A4", "A5")]
#' agreeableness[, "A1"] = 7 - agreeableness[, "A1"] # Reverse-coded item.
#' object = latcon(agreeableness)
#' ordinal_alpha(object) # 0.6267724
#' ordinal_omega(object, weights = "equal") # 0.6394087

ordinal_alpha = function(object) {
  cuts = object$cuts
  rho = object$rho
  alpha_std(rho) * sum(xi_theoretical(cuts, rho)) / sum(rho)
}
