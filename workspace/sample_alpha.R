tr = function(mat) sum(diag(mat))


sample_alpha = function(Sigma) {
  k = nrow(Sigma)
  k/(k-1)*(1 - tr(Sigma)/sum(Sigma))
}

sample_alpha_std = function(Sigma) {
  Sigma = cov2cor(Sigma)
  k = nrow(Sigma)
  k/(k-1)*(1 - tr(Sigma)/sum(Sigma))
}



sample_alpha(A)
sample_alpha_std(A)

N = 10000
n = 1000
phi = phi
covs = rWishart(N, df = n, Sigma = matrix(c(1, phi,
                                            phi, 1), nrow = 2, byrow = TRUE))
res = sapply(1:N, function(i) sample_alpha_std(covs[, , i]))


res = replicate(N, {
  x = rexp(n)
  val = cov(cbind(x + rexp(n), x + rexp(n)))
  c(sample_alpha_std(val), sample_alpha(val))
})
