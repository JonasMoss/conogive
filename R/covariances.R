#' Calculate covariance matrices.
#'
#' Covariance of y and y* given phi and tau.
#'
#' @param phi Correlation matrix of y*.
#' @param tau List of cutoffs tau.
#' @return Covariance matrix.

cov_yy_star = function(phi, tau) {
  cov_ij = Vectorize(function(i, j) {
    if (i == j) {
      sum(dnorm(tau[[i]]))
    } else {
      phi_ij = matrix(1, nrow = 2, ncol = 2)
      phi_ij[1, 2] = phi[i, j]
      phi_ij[2, 1] = phi[i, j]
      g = function(k) {
        k * tmvtnorm::mtmvnorm(
          mean = c(0, 0),
          sigma = phi_ij,
          lower = c(-Inf, tau[[i]][k]),
          upper = c(Inf, tau[[i]][k+1]),
          doComputeVariance = FALSE)$tmean[1] *
          mvtnorm::pmvnorm(
            mean = c(0, 0),
            sigma = phi_ij,
            lower = c(-Inf, tau[[i]][k]),
            upper = c(Inf, tau[[i]][k+1]))[1]

      }
      sum(sapply(seq(length(tau[[i]]) - 1), g))
    }
  })

  k = nrow(phi)
  outer(seq(k), seq(k), cov_ij)
}

#' Covariance of y given phi and tau.
#'
#' NEED TO FIX THE VARIANCE.
#'
#' @param phi Correlation matrix of y*.
#' @param tau List of cutoffs tau.
#' @return Covariance matrix.

cov_y = function(phi, tau) {

  cov_ij = Vectorize(function(i, j) {
    if (i == j) {
      m2 = Vectorize(function(k) {
        p = (pnorm(tau[[i]][k+1]) - pnorm(tau[[i]][k]))
        k ^ 2 * p
      })
      m1 = Vectorize(function(k) {
        p = (pnorm(tau[[i]][k+1]) - pnorm(tau[[i]][k]))
        k * p
      })
      m = length(tau[[i]]) - 1
      sum(sapply(seq(m), m2)) - sum(sapply(seq(m), m1))^2


    } else {
      phi_ij = matrix(1, nrow = 2, ncol = 2)
      phi_ij[1, 2] = phi[i, j]
      phi_ij[2, 1] = phi[i, j]
      g = Vectorize(function(k1, k2) {
        lower = c(tau[[i]][k2], tau[[j]][k1])
        upper = c(tau[[i]][k2+1], tau[[j]][k1+1])
        k1 * k2 * (mvtnorm::pmvnorm(
          mean = c(0, 0),
          sigma = phi_ij,
          lower = lower,
          upper = upper)[1] -
            (pnorm(tau[[i]][k2+1]) - pnorm(tau[[i]][k2])) *
            (pnorm(tau[[j]][k1+1]) - pnorm(tau[[j]][k1]))
        )

      })
      m1 = length(tau[[i]]) - 1
      m2 = length(tau[[j]]) - 1
      sum(outer(seq(m1), seq(m2), g))
    }
  })

  k = nrow(phi)
  outer(seq(k), seq(k), cov_ij)

}

#' Calculate the Theoretical Xi
#'
#' @keywords internal
#' @param phi Correlation matrix of y*.
#' @param tau List of cutoffs tau.
#' @return Theoretical xi matrix.

cov_mu = function(phi, tau) {

  checkmate::assert_matrix(phi, mode = "numeric", any.missing = FALSE)

  items_k = nrow(phi)
  tau = massage_tau(tau, items_k)
  probs = lapply(tau, function(x) diff(stats::pnorm(x)))
  ms = sapply(probs, length)

  f = function(i, cut)
    -(stats::dnorm(cut[i + 1]) - stats::dnorm(cut[i])) /
    (stats::pnorm(cut[i + 1]) - stats::pnorm(cut[i]))

  etas = lapply(seq.int(probs), function(i) f(seq.int(ms[i]), tau[[i]]))

  g = function(i, j, k, l) {
    lower = rep(-Inf, items_k)
    upper = rep(Inf, items_k)
    lower[c(i, j)] = c(tau[[i]][k], tau[[j]][l])
    upper[c(i, j)] = c(tau[[i]][k + 1], tau[[j]][l + 1])
    mvtnorm::pmvnorm(lower = lower, upper = upper, corr = phi)
  }

  FUN <- Vectorize(function(i, j) {

    if(i != j) {
      grid = as.matrix(expand.grid(seq.int(ms[i]), (seq.int(ms[j]))))
      sum(apply(grid, 1, function(y)
        g(i, j, y[1], y[2]) * etas[[i]][y[1]] * etas[[j]][y[2]]))
    } else {
      grid = seq.int(ms[i])
      sum(sapply(grid, function(y)
        g(i, i, y, y) * etas[[i]][y]^2))
    }

  })

  outer(X = seq.int(items_k), Y = seq.int(items_k), FUN = FUN)

}

