#' Trace of matrix
#' @param A A square matrix.
#' @value Trace of the matrix.
tr = function(A) sum(diag(A))


#' Coefficient alpha
#' @param S A positive definite covariance matrix.
#' @value Cofficient alpha.
alpha = function(S) {
  k = nrow(S)
  k / (k - 1) * (1 - tr(S) / sum(S))
}

#' Standardized coefficient alpha
#' @param S A positive definite covariance matrix.
#' @value Cofficient alpha.
alpha_std = function(S) {
  R = cov2cor(S)
  k = nrow(S)
  k / (k - 1) * (1 - k / sum(R))
}

#' Variance of the beta distribution
#' @param shape1,shape2 Shape parameters of the beta density.
#' @return The variance of the beta distribution.
vbeta = function(shape1, shape2) {
  shape1 * shape2 / ((shape1 + shape2)^2 * (shape1 + shape2 + 1))
}

#' Variance of the gamma distribution
#' @param shape1,shape2 Shape parameters of the gamma density.
#' @return The variance of the gamma distribution.
vgamma = function(shape, ratio) shape / ratio^2


#' Error distributions.
#' 
#' Three error distributions, all with mean 0 and variance 1. Rescaled variants
#'    of the Beta distribution, Gamma distriution, and t distribution.
#' @param n Number of samples.
#' @return Random variates.
error_beta = function(n) (rbeta(n, 1/10, 1/10)*2 - 1)/(2*sqrt(vbeta(1/10, 1/10)))
error_t = function(n) rt(n, df = 1)/sqrt(3)
error_gamma = function(n) 
  (rgamma(n, shape = 1/100, rate = 1/100) - 1)/sqrt(vgamma(1/100, 1/100))


#' Simulate covariance matrix from a tau-equivalent model.
#' 
#' @param n Number of observations.
#' @param k Size of matrix or number of testlets.
#' @param lambda Scalar factor loading.
#' @param sigma Vector of error standard deviations.
#' @param latent Distribution of the latent variable.
#' @param error Distribution of the error variable.
#' @return Covariance matrix.

simulate_tau = function(n, k, lambda = 1, sigma = rep(1, k), latent = rnorm, error = rnorm) {
  z = latent(n, mean = 0, sd = 1)
  eps = matrix(error(k * n), nrow = n)
  y = lambda * z + sweep(eps, 2, sigma, "*") 
  y
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

simulate_MSE = function(nreps = 10^5, n = 100, k = 5, sigma = 1, error = rnorm) {
  
  sigma = rep(sigma, k)
  lambda = rep(1, k)

  sims = replicate(nreps, {
    x = simulate_tau(n, k, sigma = sigma, error = error, latent = rnorm)
    S = cov(x)
    c(alpha = alpha(S), alpha_s = alpha_std(S))
  })
  
  alpha_true = rel_congen(lambda, sigma)
  result = sqrt(n)*(sims - alpha_true)
  rowMeans(result^2)
  
}

#' Run the simulation study of the paper.
#' @param nreps Number of repetitons.
#' @return A matrix of alphas and standardized alphas presented in the same way
#'    as in the the paper.
simulation = function(nreps) {
  
  ##  The parameters used; just as in the paper!
  params = expand.grid(k = c(4, 20), n = c(20, 200), 
                       sigma = c(1, 0.5, 0.1),
                       error = c(error_t, error_beta, error_gamma))
  
  sims = t(apply(params, 1, function(param) {
    simulate_MSE(nreps = nreps, n = param$n, k = param$k, sigma = param$sigma,
                 error = param$error)
  }))
  
  ## This is a convoluted way of changing the sims result into the format used
  ## in the table in the paper. Please forgive my sloppiness!
  
  sims_1 = sims[, 1]
  dim(sims_1) = c(4, 9)
  sims_2 = sims[, 2]
  dim(sims_2) = c(4, 9)
  dim(sims) = c(8, 9)
  sims[c(1, 3, 5, 7), ] = sims_1
  sims[c(2, 4, 6, 8), ] = sims_2
  
  sims
  
}

#' Composite reliability under the congeneric model.
#' 
#' Calculate four different reliability coefficients under the congeneric model.
#' 
#' @param lambda Vector of factor loadings.
#' @param sigma Vector of residual standard deviations.
#' @param type One of "congeneric" (default), "H", "standardized" and "sigma".
#' @return The theoretical reliability at `lambda` and `sigma`.

reliability = function(lambda, sigma) {}


rel_congen = function(lambda, sigma) {
  k = length(lambda)
  a = k * mean(abs(lambda))^2
  b = mean(sigma^2)
  a/(a + b)
}

rel_h = function(lambda, sigma) {
  a = sum(lambda^2/sigma^2)
  b = 1
  a/(a + b)
}

rel_1 = function(lambda, sigma) {
  k = length(lambda)
  a = k*mean(abs(lambda)/sigma)^2
  b = 1
  a/(a + b)
}

rel_2 = function(lambda, sigma) {
  k = length(lambda)
  a = k * mean(abs(lambda) / sqrt(lambda^2 + sigma^2))^2
  b = mean(sigma^2 / (lambda^2 + sigma^2))
  a/(a + b)
}

rel = function(lambda, sigma) {
  c("sigma" = rel_1(lambda, sigma),
    "standardized" = rel_2(lambda, sigma),
    "congeneric" = rel_congen(lambda, sigma),
    "h" = rel_h(lambda, sigma))
}
