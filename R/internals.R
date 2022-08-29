#' Reliability
#'
#' @param y Observed values.
#' @param method One of `"sum"`, `"linear"`, `"marginal"`, or `"optimal"`.
#' @param lambda Vector of factor loadings in the normal-ogive model.
#' @param tau A list containing the vector of cuttoffs.
#' @return The reliability coefficient.
reliability = function(y, method = c("sum", "linear", "marginal", "optimal"),
                       lambda, tau){

  method = match.args(method)

  if (method == "sum") {
  } else if (method == "linear") {

  } else if (method == "marg_con") {

  } else if (method == "optimal") {

  }
}

#' Factor scores for the congeneric normal-ogive model.
#'
#' @param y Array of observed Likert variables, starting with `1`.
#' @param method One of `"sum"`, `"linear"`, `"marg_con"`, and `"optimal"`.
#' @param tau List of cutoff, the `i`th element belonging to the `i`th column of
#'    `y`.
#' @param lambda Vector of lambda coefficients in the normal-ogive model.
#' @return A vector of factor scores.

score = function(y, method = c("sum", "linear", "marg_con", "optimal"),
                 tau, lambda) {
  method = match.args(method)

  if (method == "sum") {
    rowsums(y)
  } else if (method == "linear") {

  } else if (method == "marg_con") {

  } else if (method == "optimal") {

  }


}

#' Transform parameters on `lavaan` form to usable form.
#'
#' @param obj A `lavaan` object.
#' @return A list containing `lambda` and `tau`.
#' @keywords internal

transform_params = function(obj) {
  names_to_ns = function(names) {
    filtered = sapply(names, function(x) strsplit(x, "|", fixed = TRUE)[[1]][1])
    c(table(factor(filtered, levels=unique(filtered))))
  }

  lambda = c(lavaan::lavInspect(obj, what = "est")$lambda)
  tau_ = lavaan::lavInspect(obj, what = "est")$tau
  ns = names_to_ns(rownames(tau_))
  tau = lapply(vec_to_list(tau_, ns), trim_vector)
  list(lambda = lambda, tau = tau)
}


#' Internal reliability functions.
#'
#' These reliability functions are on a format suitable for numerical
#'    differentiation of data coming from `lavaan`.
#'
#' @keywords internal
#' @param theta Parameter vector.
#' @param i The length of the lambda parameter.
#' @param ns The number of cutoff points for each variable.
#' @return The gradient at theta.
NULL

reliability_linear = function(lambda, tau) {
  phi = lambda_to_phi(lambda)
  psi = cov_yy_star(phi, tau)
  gamma = cov_y(phi, tau)
  v = thurstone(lambda)
  w = solve(gamma, psi %*% v)
  rel = c(crossprod(w, psi %*% v))^2 / c(crossprod(w, gamma %*% w))
  list(scores = w, reliability = rel)
}

reliability_sum = function(lambda, tau) {
  phi = lambda_to_phi(lambda)
  psi = cov_yy_star(phi, tau)
  gamma = cov_y(phi, tau)
  v = thurstone(lambda)
  w = rep(1, length(lambda))
  rel = c(crossprod(w, psi %*% v))^2 / c(crossprod(w, gamma %*% w))
  list(scores = w, reliability = rel)
}

reliability_marginal = function(lambda, tau) {
  phi = lambda_to_phi(lambda)
  xi = cov_mu(phi, tau)
  v = thurstone(lambda)
  w = v
  rel = c(crossprod(w, xi %*% v))^2 / c(crossprod(w, xi %*% w))
  list(scores = w, reliability = rel)
}

reliability_optimal = function(lambda, tau) {
  v = thurstone(lambda)
  phi = lambda_to_phi(lambda)
  w = x_hat(y, phi, tau)
  rel = crossprod(v, cov(mus) %*% v)
  list(scores = w, reliability = rel)
}
