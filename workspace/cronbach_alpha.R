n = 100
z = rnorm(n)
k = 4
sigma = rep(1, 4)
lambda = rep(1, 4)

Y = data.frame(x1 = lambda[1]*z,
               x2 = lambda[2]*z,
               x3 = lambda[3]*z,
               x4 = lambda[4]*z)

X = data.frame(x1 = lambda[1]*z + sigma[1]*rnorm(n),
               x2 = lambda[2]*z + sigma[2]*rnorm(n),
               x3 = lambda[3]*z + sigma[3]*rnorm(n),
               x4 = lambda[4]*z + sigma[4]*rnorm(n))

A = cov(X)
summary(lm(z ~ ., data = X))

model <- ' z =~ 1*x1 + 1*x2 + 1*x3 + 1*x4
           x1 ~~ var*x1
           x2 ~~ var*x2
           x3 ~~ var*x3
           x4 ~~ var*x4'

fit <- lavaan::cfa(model, data = X)
coefs = lavaan::lavInspect(fit, what = "x")
lambda_est = coefs$lambda*sqrt(as.numeric(coefs$psi))
sigma_est = sqrt(diag(lavaan::lavInspect(fit, what = "x")$theta))

r2 = function(lambda, sigma) {
  sum(lambda^2*sigma^-2)/(1 + sum(lambda^2*sigma^-2))
}

omega = function(lambda, sigma) {
  (1 + mean(sigma^2)/(k*mean(lambda)^2))^-1
}

weights = function(lambda, sigma){
  lambda/(sigma^2*(1 + sum(lambda^2*sigma^(-2))))
}

weight(lambda, sigma)
r2(lambda, sigma)
r2(lambda_est, sigma_est)
semTools::reliability(fit)

sum(lambda*weights(lambda, sigma))^2/
  (sum(lambda*weights(lambda, sigma))^2 +
     sum(sigma^2*weights(lambda, sigma)^2))


X = lavaan::HolzingerSwineford1939[, 7:15]
HS.model <- 'visual  =~ x4 + x5 + x6 + x7 + x8 + x9'
fit <- lavaan::cfa(HS.model, data = lavaan::HolzingerSwineford1939)
coefs = lavaan::lavInspect(fit, what = "x")
lambda_est = coefs$lambda*sqrt(as.numeric(coefs$psi))
sigma_est = sqrt(diag(lavaan::lavInspect(fit, what = "x")$theta))
rel(lambda_est, sigma_est)
semTools::reliability(fit)


psych::alpha(X)

A = cov(X)
lambda = mean(A[row(A) == (col(A) - 1)])
vars = diag(A - lambda)
sum(vars^-1*lambda)/(1 + sum(vars^-1*lambda))

psych::alpha(X)
1/(1 + mean(vars)/k)


1/(k + sigma[1]^2)
1 - (k*sigma[1]^2 + sigma[1]^4)/(k + sigma[1]^2)^2
k/(k + sigma[1]^2)


### ============================================================================
### Is tau-equivalence enough?
###
###   Yes, it is. This is simple enough to show mathematically.
###   *But!* It requires non-optimal loadings.
### ============================================================================


sigma = sqrt(1:4)
lambda = rep(2, 4)

omega = function(lambda, sigma) (1 + mean(sigma^2)/(k*mean(lambda)^2))^-1

n = 500000
z = rnorm(n)
X = data.frame(x1 = lambda[1]*z + sigma[1]*rnorm(n),
               x2 = lambda[2]*z + sigma[2]*rnorm(n),
               x3 = lambda[3]*z + sigma[3]*rnorm(n),
               x4 = lambda[4]*z + sigma[4]*rnorm(n))

omega(lambda, sigma)
model = ' z =~ 1*x1 + 1*x2 + 1*x3 + 1*x4'
fit = lavaan::cfa(model, data = X)
semTools::reliability(fit)

### ============================================================================
### Efficiency:
###   1.) Does standardized alpha outperform ordinary alpha?
### ============================================================================

n = 10000
sigma = rep(1, 4)
lambda = rep(2, 4)
omega(lambda, sigma)
k = 4

results = replicate(n = 10000, {
  z = rnorm(n)
  X = matrix(c(lambda[1]*z + sigma[1]*(rexp(n) - 1),
               lambda[2]*z + sigma[2]*(rexp(n) - 1),
               lambda[3]*z + sigma[3]*(rexp(n) - 1),
               lambda[4]*z + sigma[4]*(rexp(n) - 1)), ncol = 4)
  covar = cov(X)
  v_hat = mean(diag(covar))
  diag(covar) = NA
  c_hat = mean(covar, na.rm = TRUE)
  r = cor(X)
  diag(r) = NA
  r = mean(r, na.rm = TRUE)
  c(k*c_hat/(v_hat + (k - 1)*c_hat),
    k*r/(1 + (k - 1)*r))
    
})

n*((rowMeans(results) - omega(lambda, sigma))^2 + apply(results, 1, var))

### ============================================================================
### Translations of alpha
### ============================================================================

k = 4
lambda = c(sqrt(15.77), sqrt(15.77), sqrt(0.01), sqrt(0.01))
sigma = c(1.75, 1.75, 0.97, 0.97)

alpha = function(lambda, sigma) {
  k = length(lambda)
  k/(k-1)*(1 - sum(sigma^2 + lambda^2)/(sum(lambda)^2 + sum(sigma^2)))
}

alpha2 = function(lambda, sigma) {
  k/(k-1)*(k*mean(lambda)^2 - mean(lambda^2))/(k*mean(lambda)^2 + mean(sigma^2))
}

alpha_std = function(lambda, sigma) {
  mat = cov2cor(outer(lambda, lambda) + diag(sigma^2))
  k/(k-1)*(1 - k/sum(mat))
}

alpha(lambda, sigma)
alpha2(lambda, sigma)
alpha_std(lambda, sigma)
omega(lambda, sigma)
