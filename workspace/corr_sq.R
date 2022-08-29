tau = lapply(seq(5), function(x) c(-Inf, sort(rnorm(5)), Inf))
sigma = rep(1, 5)
lambda = 1:5
temp = cbind(lambda, sigma)/sqrt(lambda^2 + sigma^2)
lambda = temp[, 1]
sigma = temp[, 2]
n = 10000
result = simulate(n, lambda, sigma, tau)

phi = lambda_to_phi(lambda)
preds = x_hat(result$y, phi, tau)
hats = preds %*% thurstone(lambda)
eps = result$z - hats
cor(result$z, eps)


c(cor(result$y %*% rep(1, 5), result$z)^2)

v = thurstone(lambda, sigma)
i = rep(1, 5)
psi = cov(result$y, result$y_star)
gamma = cov(result$y, result$y)


### Verify cov mu.
mu = sapply(1:ncol(result$y), function(i) x_hat(result$y[, i], tau[[i]]))
phi = cov(result$y_star)
cov_mu(phi, tau)

### Verify mu reliability, score
cor(mu %*% i, result$z)^2
phi = lambda_to_phi(lambda)
xi = cov_mu (phi, tau)

### Verify mu reliability, optimal
cor(mu %*% w, result$z)^2

xi = cov(mu)
xi2 = cov(mu, result$y_star)
v = thurstone(lambda)
w = solve(xi) %*% xi2 %*% v

cor(mu %*% i, result$z)^2
c(crossprod(i, xi2 %*% v))^2 / c(crossprod(i, xi %*% i))

cor(mu %*% w, result$z)^2
c(crossprod(w, xi2 %*% v))^2 / c(crossprod(w, xi %*% w))
c(crossprod(w, xi %*% w))


cor(mu %*% v, result$z)^2

### Verify linear reliability
### This is verified.

w = solve(gamma) %*% psi %*% v
c(cor(result$y %*% i, result$z)^2)
c(t(i) %*% psi %*% v)^2 / c(t(i) %*% gamma %*% i)

c(cor(result$y %*% w, result$z)^2)
c(t(w) %*% psi %*% v)^2 / c(t(w) %*% gamma %*% w)

c(t(v) %*% t(psi) %*% solve(gamma) %*% psi %*% v)


cov(result$y %*% i, result$y_star %*% v)
var(result$y %*% i)

cov(result$y %*% i, result$z)
cov(result$y %*% i, result$y_star %*% v)
