#' Transform latent y_star to y.
#'
#' @keywords internal
#' @param y_star Latent y_star.
#' @param tau Vector of cutoffs.
#' @return Observed ys.
to_y = function(y_star, tau) {
  as.numeric(cut(y_star, breaks = tau, labels = seq(length(tau) - 1)))
}

#' Simulate from a congeneric normal o-give model.
#'
#' @keywords internal
#' @param n Number of samples to create.
#' @param lambda Vector of lambdas.
#' @param tau List of tau cutoffs.
#' @return List containing the true latent `z`, the true
#'    latent score `y_star`, and the observed `y`.

simulate = function(n, lambda, tau) {
  sigma = 1 - lambda ^ 2
  temp = cbind(lambda, sigma)/sqrt(lambda^2 + sigma^2)
  lambda = temp[, 1]
  sigma = temp[, 2]
  k = length(sigma)
  z = rnorm(n)
  errors = tmvtnorm::rtmvnorm(n, sigma * 0, diag(sigma^2))
  y_star = matrix(lambda, nrow = n, ncol = k, byrow = TRUE) * z + errors
  y_star = scale(y_star)
  y = sapply(seq(k), function(i) to_y(y_star[, i], tau[[i]]))
  list(y = y, y_star = y_star, z = z)
}
