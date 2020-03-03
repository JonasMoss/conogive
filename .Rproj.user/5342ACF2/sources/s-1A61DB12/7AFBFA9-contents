#' Variants of coefficient alpha
#'
#' The ordinary, standardized and sigma coefficient alpha.
#'
#' @param sigma A positive definite covariance matrix.
#' @return Cofficient alpha, standardized, orsigma coefficient alpha.
#' @name alpha
#' @export

alpha <- function(sigma) {
  k <- nrow(sigma)
  k / (k - 1) * (1 - tr(sigma) / sum(sigma))
}

#' @rdname alpha
#' @export
alpha_std <- function(sigma) {
  rho <- stats::cov2cor(sigma)
  k <- nrow(sigma)
  k / (k - 1) * (1 - k / sum(rho))
}

#' Variants of the omega coefficient
#'
#' The congeneric reliability, coefficient H, standardized reliability, and
#'    sigma reliability.
#'
#' @param lambda Vector of loadings.
#' @param sigma Vector of standard deviations.
#' @return The congeneric reliability, coefficient H, standardized reliability,
#'    or sigma reliability.
#' @name omega
#' @export
omega <- function(lambda, sigma) {
  k <- length(lambda)
  a <- k * mean(abs(lambda))^2 / mean(sigma^2)
  a / (a + 1)
}

#' @rdname omega
#' @export
omega_h <- function(lambda, sigma) {
  a <- sum(lambda^2 / sigma^2)
  a / (a + 1)
}

#' @rdname omega
#' @export
omega_sigma <- function(lambda, sigma) {
  k <- length(lambda)
  a <- k * mean(abs(lambda) / sigma)^2
  a / (a + 1)
}

#' @rdname omega
#' @export
omega_std <- function(lambda, sigma) {
  k <- length(lambda)
  a <- k * mean(abs(lambda) / sqrt(lambda^2 + sigma^2))^2 /
    mean(sigma^2 / (lambda^2 + sigma^2))
  a / (a + 1)
}

#' The bias term for the weighted alpha.
#'
#' The bias term is described in the paper.
#'
#' @param lambda Vector of loadings.
#' @param sigma Vector of standard deviations.
#' @param w Vector of weights.
#' @return The bias term, i.e. B such that omega = alpha + B.

bias = function(lambda, sigma, w = rep(1, length(lambda))) {
  k = length(lambda)
  k / (k - 1) * (- crossprod(w, lambda)^2/k + crossprod(w^2, lambda^2)) /
    (crossprod(w, lambda)^2 + crossprod(w^2, sigma^2))
}

#' Thurstone weights
#'
#' @param lambda Vector of loadings.
#' @param sigma Vector of standard deviations.
#' @return The Thurstone weights.

thurstone = function(lambda, sigma) {
  c(lambda/(sigma^2 * (1 + sum (lambda^2 / sigma^2))))
}

