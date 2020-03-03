#' Variance of the beta distribution
#' @param shape1,shape2 Shape parameters of the beta density.
#' @return The variance of the beta distribution.
vbeta <- function(shape1, shape2) {
  shape1 * shape2 / ((shape1 + shape2)^2 * (shape1 + shape2 + 1))
}

#' Variance of the gamma distribution
#' @param shape,ratio Shape parameters of the gamma density.
#' @return The variance of the gamma distribution.
vgamma <- function(shape, ratio) shape / ratio^2

#' Error distributions.
#'
#' Three error distributions, all with mean 0 and variance 1. Rescaled variants
#'    of the Beta distribution, Gamma distriution, and t distribution.
#' @param n Number of samples.
#' @return Random variates.
error_beta <- function(n) (stats::rbeta(n, 1 / 10, 1 / 10) * 2 - 1) / (2 * sqrt(vbeta(1 / 10, 1 / 10)))
error_t <- function(n) stats::rt(n, df = 5) / sqrt(5/3)
error_gamma <- function(n) {
  (stats::rgamma(n, shape = 1 / 100, rate = 1 / 100) - 1) / sqrt(vgamma(1 / 100, 1 / 100))
}


#' Simulate covariance matrix from a tau-equivalent model.
#'
#' @param n Number of observations.
#' @param k Size of matrix or number of testlets.
#' @param lambda Scalar factor loading.
#' @param sigma Vector of error standard deviations.
#' @param latent Distribution of the latent variable.
#' @param error Distribution of the error variable.
#' @return Covariance matrix.
#' @export
simulate_tau <- function(n, k, lambda = 1, sigma = 1,
                         latent = stats::rnorm, error = stats::rnorm) {
  lambda <- rep(lambda, length.out = k)
  sigma <- rep(sigma, length.out = k)
  z <- latent(n, mean = 0, sd = 1)
  eps <- matrix(error(k * n), nrow = n)
  sweep(matrix(rep(z, k), nrow = n), 2, lambda, "*") +
   sweep(eps, 2, sigma, "*")
}

#' Simulate MSE for alpha and standardized alpha for a parallel model.
#'
#' The parameter lambda is fixed to one and the latent variable is unit normal.
#'
#' @param nreps Number of reptitions.
#' @param n Number of observations.
#' @param k Size of matrix or number of testlets.
#' @param sigma Residual standard deviation.
#' @param error Distribution of the error variable.
#' @return Covariance matrix.
#' @export
simulate_MSE <- function(nreps = 10^5, n = 100, k = 5, sigma = 1, error = stats::rnorm) {
  sigma <- rep_len(x = sigma, length.out = k)
  lambda <- rep_len(1, length.out = k)

  sims <- replicate(nreps, {
    x <- simulate_tau(n, k, sigma = sigma, error = error, latent = stats::rnorm)
    S <- stats::cov(x)
    c(alpha = alpha(S), alpha_s = alpha_std(S))
  })

  alpha_true <- omega(lambda, sigma)
  result <- rowMeans((sqrt(n) * (sims - alpha_true))^2)
  result[1]/result[2]
}

#' Run the simulation study of the paper.
#' @param nreps Number of repetitons.
#' @return A matrix of alphas and standardized alphas presented in the same way
#'    as in the the paper.
#' @export
simulation <- function(nreps) {

  ##  The parameters used; just as in the paper!
  params <- expand.grid(
    k = c(5, 20), n = c(50, 200),
    sigma = c(2, 1, 0.5),
    error = c(error_t, error_beta, error_gamma)
  )

  sims <- apply(params, 1, function(param) {
    simulate_MSE(
      nreps = nreps, n = param$n, k = param$k, sigma = param$sigma,
      error = param$error
    )
  })

  dim(sims) = c(9, 4)
  t(sims)
}
