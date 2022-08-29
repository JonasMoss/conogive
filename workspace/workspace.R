
tau = lapply(seq(5), function(x) c(-Inf, sort(rnorm(5)), Inf))
sigma = rep(1, 5)
lambda = 1:5
temp = cbind(lambda, sigma)/sqrt(lambda^2 + sigma^2)
lambda = temp[, 1]
sigma = temp[, 2]
n = 1000000
result = simulate(n, lambda, sigma, tau)

#diag(cov(result$y, result$y_star))
#sapply(tau, function(t) sum(dnorm(t)))
#
#
# tau = c(-Inf, -2, -1, -0.5, 0, 0.5, 1, 2, Inf)
# n = 10000
# y_star = rnorm(n)
# y = to_y(y_star, tau)
#
# sum(dnorm(tau))
# cov(y, y_star)
