#' Remove infinities from vector and append and prepend -Inf and Inf.
#'
#' @param x Numeric vector.
#' @return Fixed vector.

trim_vector = function(x) {
  checkmate::assertNumeric(x)
  x = x[x != Inf & x != -Inf]
  c(-Inf, x, Inf)
}

#' Massage cuts to the desired shape
#'
#' @param cuts A matrix, list, or vector of cuts
#' @param k Optional `k` saying how many times the vector of cuts should be
#'    repeated. Only matters when `cuts` is a vector.

massage_cuts = function(cuts, k)  {

  if (checkmate::testAtomicVector(cuts)) {
    if(missing(k)) k = 1
    cuts = t(matrix(rep(trim_vector(cuts), k), nrow = length(cuts)))
  }

  if(is.matrix(cuts)) {
    cuts = lapply(seq.int(nrow(cuts)), function(i) trim_vector(cuts[i, ]))
  }

  cuts

}

#' Calculate the theoretical Xi
#'
#' @param cuts A matrix, list, or vector of cuts
#' @param k Optional `k` saying how many times the vector of cuts should be
#'    repeated. Only matters when `cuts` is a vector.
#' @param Sigma The polychoric correlation matrix.
#' @return Theoretical Xi matrix.

Xi_theoretical = function(cuts, Sigma) {

  items_k = nrow(Sigma)
  cuts = massage_cuts(cuts, items_k)
  probs = lapply(cuts, function(x) diff(pnorm(x)))
  ms = sapply(probs, length)

  f = function(i, cut)
    -(dnorm(cut[i + 1]) - dnorm(cut[i])) / (pnorm(cut[i + 1]) - pnorm(cut[i]))

  etas = lapply(seq.int(probs), function(i) f(seq.int(ms[i]), cuts[[i]]))

  g = function(i, j, k, l) {
    lower = rep(-Inf, items_k)
    upper = rep(Inf, items_k)
    lower[c(i, j)] = c(cuts[[i]][k], cuts[[j]][l])
    upper[c(i, j)] = c(cuts[[i]][k + 1], cuts[[j]][l + 1])
    mvtnorm::pmvnorm(lower = lower, upper = upper, corr = Sigma)
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



















x_hats = function(y, cuts) {
  apply(y, 2, x_hat, cuts)
}

omega_prime_hat = function(y, poly) {

  k = ncol(y)
  n = nrow(y)

  if(missing(poly))  {
    poly = psych::polychoric(y)
    cuts = cbind(rep(-Inf, k), poly$tau, rep(Inf, k))
    x = do.call(what = cbind,
                args = lapply(seq.int(k), function(i) x_hat(y[, i], cuts[i, ])))
  }

  Xi = cov(x)
  v = thurstone(psych::fa(poly$rho)$loadings,
                sqrt(psych::fa(poly$rho)$uniquenesses))


  c(crossprod(v, Xi %*% v))

}

omega_prime = function(lambda, sigma, cuts) {
  lambda = standardize_lambda(lambda, sigma)
  sigma = standardize_sigma(lambda, sigma)
  Sigma = tcrossprod(lambda, lambda) + diag(sigma^2)
  v = thurstone(lambda, sigma)

  if(is.matrix(cuts))
    cuts = lapply(seq_len(nrow(cuts)), function(i) cuts[i, ])

  probs = lapply(cuts, function(x) diff(pnorm(x)))
  ms = sapply(probs, length)

  f = function(i, cut)
    -(dnorm(cut[i + 1]) - dnorm(cut[i])) / (pnorm(cut[i + 1]) - pnorm(cut[i]))

  etas = lapply(seq.int(probs), function(i) f(seq.int(ms[i]), cuts[[i]]))

  items_k = length(cuts)

  g = function(i, j, k, l) {
    lower = rep(-Inf, items_k)
    upper = rep(Inf, items_k)
    lower[c(i, j)] = c(cuts[[i]][k], cuts[[j]][l])
    upper[c(i, j)] = c(cuts[[i]][k + 1], cuts[[j]][l + 1])
    mvtnorm::pmvnorm(lower = lower, upper = upper, corr = Sigma)
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

  covs = outer(X = seq.int(items_k), Y = seq.int(items_k), FUN = FUN)

}

for(i in 1:length(grid)) {
  y = grid[i, ]
  g(i, j, y[1], y[2])
  print(i)
}


omega_prime_hat = function(y) {
  poly = psych::polychoric(y)
  rho = poly$rho
  k = ncol(y)
  n = nrow(y)
  cuts = cbind(rep(-Inf, k), poly$tau, rep(Inf, k))
  xhats = do.call(what = cbind,
            args = lapply(seq.int(k), function(i) x_hat(y[, i], cuts[i, ])))
  Xi = cov(xhats)

  lambda = psych::fa(rho)$loadings
  sigma = sqrt(psych::fa(rho)$uniquenesses)
  # tcrossprod(lambda, lambda) + diag(sigma^2)

  # This one find alpha prime.
  w0 = mean(lambda)/(k*mean(lambda)^2 + mean(sigma^2))
  sum(Xi) * w0^2

  w = lambda / (sigma^2 * (1 + sum(lambda * 1/sigma^2)))
  crossprod(rep(1, k), Xi %*% w)^2 / sum(Xi)
}

omega_prime(lambda, sigma, tau) {
  # Find w0
  lambda_star = lambda/sqrt(lambda^2 + sigma^2)
  sigma_star = sigma/sqrt(lambda^2 + sigma^2)
  w0 = mean(lambda)/(k*mean(lambda)^2 + mean(sigma^2))

}




x_hat(c(y[, 1]), cuts)
x_hats(y, cuts)

corr = cov(xhats)
colnames(corr) <- c("X1", "X2", "X3", "X4", "X5")
rownames(corr) <- colnames(corr)
model = " y =~ X1 + X2 + X3 + X4 + X5"

fit <- lavaan::sem(model = model, sample.cov = corr, sample.nobs = n)
coefs <- lavaan::lavInspect(fit, what = "x")

lambda <- abs(c(coefs$lambda * sqrt(as.numeric(coefs$psi))))
sigma <- sqrt(diag(lavaan::lavInspect(fit, what = "x")$theta))

v = thurstone(lambda, sigma)
w = eigen(Xi_sq %*% tcrossprod(v, v) %*% Xi_sq)$vectors[1, ]
crossprod(w, Xi %*% v)^2 / crossprod(w, Xi %*% w)
crossprod(v, Xi %*% v)^2 / crossprod(v, Xi %*% v)

f = function(w) {
  -crossprod(w, Xi %*% v)^2 / crossprod(w, Xi %*% w)
}

nlm(f, p = v)

Psi = diag(sigma^2)
(diag(5) - v %*% t(lambda)) %*% solve(Psi) %*% v
(diag(5) - (lambda %*% t(v))) %*% solve(Psi) %*% v

(diag(5) - solve(Psi) %*% lambda %*% t(lambda)/(1 + sum(lambda^2/sigma^2))) %*% solve(Psi)
(diag(5) - v %*% t(lambda)) %*% solve(Psi)
Sigma = tcrossprod(lambda, lambda) + diag(sigma^2)
Sigma %*% v
